-- | FeelsSOL module for managing the synthetic SOL token.
-- | This module manages the conversion between JitoSOL and FeelsSOL,
-- | including synthetic asset creation and oracle-driven pricing.
module Protocol.FeelsSOL
  ( FeelsSOLState
  , OraclePrice
  , MintResult
  , BurnResult
  , initFeelsSOL
  , enterSystem
  , exitSystem
  , getExchangeRate
  , getTotalLocked
  , getTotalMinted
  , getOraclePrice
  , getTotalSupply
  , getSystemHealth
  , getBufferStatus
  , rebalanceBuffer
  , checkSolvency
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect (Effect)
import Effect.Ref (Ref, read, write, new)
import Protocol.Token (TokenType(..))
import FFI (currentTime)
import Protocol.Error (ProtocolError(..))
import Effect.Console (log) -- Added import for log

--------------------------------------------------------------------------------
-- FeelsSOL Types
--------------------------------------------------------------------------------

-- Oracle price information
type OraclePrice =
  { price :: Number      -- JitoSOL/SOL exchange rate
  , timestamp :: Number  -- When price was fetched
  }

-- Result of minting FeelsSOL
type MintResult =
  { feelsSOLMinted :: Number
  , jitoSOLLocked :: Number
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- Result of burning FeelsSOL
type BurnResult =
  { feelsSOLBurned :: Number
  , jitoSOLReleased :: Number
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- FeelsSOL state for managing the synthetic SOL system with withdrawal buffer
type FeelsSOLState =
  { totalFeelsSOLSupply :: Ref Number      -- Total FeelsSOL in circulation
  , totalJitoSOLBacking :: Ref Number      -- Total JitoSOL backing the synthetics
  , jitoSOLBuffer :: Ref Number            -- JitoSOL buffer for withdrawals
  , priceOracle :: Effect Number           -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Ref Number         -- Last oracle update timestamp
  , cachedPrice :: Ref (Maybe OraclePrice) -- Cached oracle price
  , entryFee :: Number                     -- Fee for entering (e.0.001 = 0.1%)
  , exitFee :: Number                      -- Fee for exiting (e.0.002 = 0.2%)
  , polAllocationRate :: Number            -- Portion of fees going to POL (e.0.25 = 25%)
  , bufferTargetRatio :: Number            -- Target buffer as % of backing (e.0.10 = 10%)
  }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Validate if a token can be used as collateral
validateCollateral :: TokenType -> Either String Unit
validateCollateral JitoSOL = Right unit
validateCollateral FeelsSOL = Right unit  -- Allow FeelsSOL for composability
validateCollateral token = Left $ "Unsupported collateral type: " <> show token

-- Get oracle price with caching
getOraclePrice :: FeelsSOLState -> Effect OraclePrice
getOraclePrice state = do
  cachedPrice <- read state.cachedPrice
  lastUpdate <- read state.lastOracleUpdate
  currentTime' <- currentTime
  
  -- Use cached price if less than 1 minute old
  case cachedPrice of
    Just cached | (currentTime' - lastUpdate) < 60000.0 -> 
      pure cached
    _ -> do
      -- Fetch fresh price from oracle
      price <- state.priceOracle
      let newPrice = { price: price
                     , timestamp: currentTime'
                     }
      
      -- Update cache
      _ <- write (Just newPrice) state.cachedPrice
      _ <- write currentTime' state.lastOracleUpdate
      
      pure newPrice

-- Mint FeelsSOL against JitoSOL collateral
mintFeelsSOL :: FeelsSOLState -> Number -> Effect (Either String MintResult)
mintFeelsSOL state jitoSOLAmount = do
  if jitoSOLAmount <= 0.0
    then pure $ Left "Amount must be positive"
    else do
      -- Validate collateral type (implicit - only JitoSOL accepted)
      case validateCollateral JitoSOL of
        Left err -> pure $ Left err
        Right _ -> do
          -- Get current oracle price
          oraclePrice <- getOraclePrice state
          let exchangeRate = oraclePrice.price
          
          -- Calculate FeelsSOL to mint
          let feelsSOLAmount = jitoSOLAmount * exchangeRate
          
          -- Update state and manage buffer
          currentSupply <- read state.totalFeelsSOLSupply
          currentBacking <- read state.totalJitoSOLBacking
          currentBuffer <- read state.jitoSOLBuffer
          
          -- Allocate portion to buffer based on target ratio
          let bufferTarget = (currentBacking + jitoSOLAmount) * state.bufferTargetRatio
              bufferAddition = if bufferTarget > currentBuffer 
                              then min jitoSOLAmount (bufferTarget - currentBuffer)
                              else 0.0
              backingAddition = jitoSOLAmount - bufferAddition
          
          _ <- write (currentSupply + feelsSOLAmount) state.totalFeelsSOLSupply
          _ <- write (currentBacking + backingAddition) state.totalJitoSOLBacking
          _ <- write (currentBuffer + bufferAddition) state.jitoSOLBuffer
          
          -- Check solvency after minting
          _ <- checkSolvency state
          
          -- Return result
          timestamp <- currentTime
          pure $ Right
            { feelsSOLMinted: feelsSOLAmount
            , jitoSOLLocked: jitoSOLAmount
            , exchangeRate: exchangeRate
            , fee: 0.0  -- No fee for minting
            , timestamp: timestamp
            }

-- Burn FeelsSOL and release JitoSOL collateral
burnFeelsSOL :: FeelsSOLState -> Number -> Effect (Either String BurnResult)
burnFeelsSOL state feelsSOLAmount = do
  if feelsSOLAmount <= 0.0
    then pure $ Left "Amount must be positive"
    else do
      -- Check if enough FeelsSOL supply exists
      currentSupply <- read state.totalFeelsSOLSupply
      if feelsSOLAmount > currentSupply
        then pure $ Left "Insufficient FeelsSOL supply to burn"
        else do
          -- Get current oracle price
          oraclePrice <- getOraclePrice state
          let exchangeRate = oraclePrice.price
              jitoSOLToRelease = feelsSOLAmount / exchangeRate
          
          -- Check buffer and backing availability
          currentBacking <- read state.totalJitoSOLBacking
          currentBuffer <- read state.jitoSOLBuffer
          let totalAvailable = currentBacking + currentBuffer
          
          if jitoSOLToRelease > totalAvailable
            then pure $ Left "Insufficient JitoSOL backing and buffer"
            else do
              -- Use buffer first, then backing
              let fromBuffer = min jitoSOLToRelease currentBuffer
                  fromBacking = jitoSOLToRelease - fromBuffer
              
              -- Update state
              _ <- write (currentSupply - feelsSOLAmount) state.totalFeelsSOLSupply
              _ <- write (currentBacking - fromBacking) state.totalJitoSOLBacking
              _ <- write (currentBuffer - fromBuffer) state.jitoSOLBuffer
              
              -- Check solvency after burning
              _ <- checkSolvency state
              
              -- Return result
              timestamp <- currentTime
              pure $ Right
                { feelsSOLBurned: feelsSOLAmount
                , jitoSOLReleased: jitoSOLToRelease
                , exchangeRate: exchangeRate
                , fee: 0.0  -- No fee for burning
                , timestamp: timestamp
                }


-- Get total FeelsSOL supply
getTotalSupply :: FeelsSOLState -> Effect Number
getTotalSupply state = read state.totalFeelsSOLSupply

-- Get current collateral ratio (backing / supply)
getCollateralRatio :: FeelsSOLState -> Effect Number
getCollateralRatio state = do
  supply <- read state.totalFeelsSOLSupply
  backing <- read state.totalJitoSOLBacking
  oraclePrice <- getOraclePrice state
  
  if supply == 0.0
    then pure 1.0  -- 100% collateralized when no supply
    else do
      let backingValue = backing * oraclePrice.price
      pure (backingValue / supply)

--------------------------------------------------------------------------------
-- FeelsSOL Initialization
--------------------------------------------------------------------------------

-- Initialize the FeelsSOL system with oracle, fee parameters, and buffer management
initFeelsSOL :: 
  (Effect Number) ->    -- Price oracle function
  Number ->             -- Entry fee
  Number ->             -- Exit fee
  Effect FeelsSOLState
initFeelsSOL oracle entryFee exitFee = do
  totalSupply <- new 0.0
  totalBacking <- new 0.0
  buffer <- new 0.0
  lastUpdate <- new 0.0
  cachedPrice <- new Nothing
  
  pure { totalFeelsSOLSupply: totalSupply
       , totalJitoSOLBacking: totalBacking
       , jitoSOLBuffer: buffer
       , priceOracle: oracle
       , lastOracleUpdate: lastUpdate
       , cachedPrice: cachedPrice
       , entryFee: entryFee
       , exitFee: exitFee
       , polAllocationRate: 0.25  -- Default 25% to POL
       , bufferTargetRatio: 0.01  -- Default 1% buffer target (can be DAO managed)
       }

--------------------------------------------------------------------------------
-- System Entry (JitoSOL -> FeelsSOL)
--------------------------------------------------------------------------------

-- Enter the system by converting JitoSOL to FeelsSOL
enterSystem :: 
  FeelsSOLState ->
  String ->         -- User address (for future use)
  Number ->         -- Amount of JitoSOL to deposit
  Effect (Either ProtocolError MintResult)
enterSystem state _user jitoAmount = do
  -- Validate amount
  if jitoAmount <= 0.0
    then pure $ Left (InvalidAmountError jitoAmount)
    else do
      -- Calculate fee and net amount
      let feeAmount = jitoAmount * state.entryFee
          netJitoAmount = jitoAmount - feeAmount
      
      -- Mint FeelsSOL against the net JitoSOL amount
      mintResult <- mintFeelsSOL state netJitoAmount
      
      case mintResult of
        Left err -> pure $ Left (InvalidCommandError err)
        Right result -> do
          pure $ Right 
            { feelsSOLMinted: result.feelsSOLMinted
            , jitoSOLLocked: jitoAmount
            , exchangeRate: result.exchangeRate
            , fee: feeAmount
            , timestamp: result.timestamp
            }

--------------------------------------------------------------------------------
-- System Exit (FeelsSOL -> JitoSOL)
--------------------------------------------------------------------------------

-- Exit the system by burning FeelsSOL to receive JitoSOL
exitSystem :: 
  FeelsSOLState ->
  String ->         -- User address (for future use)
  Number ->         -- Amount of FeelsSOL to burn
  Effect (Either ProtocolError BurnResult)
exitSystem state _user feelsAmount = do
  -- Validate amount
  if feelsAmount <= 0.0
    then pure $ Left (InvalidAmountError feelsAmount)
    else do
      -- Burn FeelsSOL to get JitoSOL
      burnResult <- burnFeelsSOL state feelsAmount
      
      case burnResult of
        Left err -> pure $ Left (InvalidCommandError err)
        Right result -> do
          -- Calculate fee on JitoSOL output
          let feeAmount = result.jitoSOLReleased * state.exitFee
          
          pure $ Right 
            { feelsSOLBurned: feelsAmount
            , jitoSOLReleased: result.jitoSOLReleased - feeAmount
            , exchangeRate: result.exchangeRate
            , fee: feeAmount
            , timestamp: result.timestamp
            }

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get current exchange rate (JitoSOL/FeelsSOL)
getExchangeRate :: FeelsSOLState -> Effect Number
getExchangeRate state = do
  oraclePrice <- getOraclePrice state
  pure oraclePrice.price

-- Get total JitoSOL locked in the FeelsSOL system
getTotalLocked :: FeelsSOLState -> Effect Number
getTotalLocked state = read state.totalJitoSOLBacking

-- Get total FeelsSOL minted
getTotalMinted :: FeelsSOLState -> Effect Number
getTotalMinted state = getTotalSupply state

-- | Get system health metrics including buffer status
-- | Provides a high-level view of the FeelsSOL system's status
getSystemHealth :: FeelsSOLState -> Effect { collateralRatio :: Number, totalLocked :: Number, totalMinted :: Number, bufferRatio :: Number, isHealthy :: Boolean }
getSystemHealth state = do
  ratio <- getCollateralRatio state
  locked <- getTotalLocked state
  minted <- getTotalMinted state
  bufferStatus <- getBufferStatus state
  let isHealthy = ratio >= 1.0 && bufferStatus.isHealthy  -- System healthy if collateralized and buffer adequate
  pure { collateralRatio: ratio, totalLocked: locked, totalMinted: minted, bufferRatio: bufferStatus.ratio, isHealthy }

--------------------------------------------------------------------------------
-- BUFFER MANAGEMENT FUNCTIONS
--------------------------------------------------------------------------------
-- Functions for managing jitoSOL withdrawal buffer

-- | Get current buffer status
-- | Provides detailed information about buffer health and recommendations
getBufferStatus :: FeelsSOLState -> Effect { current :: Number, target :: Number, ratio :: Number, isHealthy :: Boolean, needsRebalancing :: Boolean }
getBufferStatus state = do
  currentBuffer <- read state.jitoSOLBuffer
  totalBacking <- read state.totalJitoSOLBacking
  
  let target = totalBacking * state.bufferTargetRatio
      ratio = if totalBacking > 0.0 then currentBuffer / totalBacking else 0.0
      minRatio = 0.005  -- 0.5% minimum buffer (could be configurable)
      isHealthy = ratio >= minRatio
      needsRebalancing = abs (ratio - state.bufferTargetRatio) > 0.005  -- 0.5% threshold
  
  pure 
    { current: currentBuffer
    , target: target
    , ratio: ratio
    , isHealthy: isHealthy
    , needsRebalancing: needsRebalancing
    }

-- | Rebalance buffer to target ratio
-- | Can be called algorithmically or by DAO governance
rebalanceBuffer :: FeelsSOLState -> Effect (Either String { oldBuffer :: Number, newBuffer :: Number, adjustment :: Number })
rebalanceBuffer state = do
  bufferStatus <- getBufferStatus state
  
  if not bufferStatus.needsRebalancing
    then pure $ Left "Buffer rebalancing not needed"
    else do
      currentBacking <- read state.totalJitoSOLBacking
      let adjustment = bufferStatus.target - bufferStatus.current
      
      -- Check if we have enough backing to increase buffer
      if adjustment > 0.0 && adjustment > currentBacking
        then pure $ Left "Insufficient backing to increase buffer"
        else do
          -- Adjust buffer and backing
          _ <- write bufferStatus.target state.jitoSOLBuffer
          _ <- write (currentBacking - adjustment) state.totalJitoSOLBacking
          
          pure $ Right 
            { oldBuffer: bufferStatus.current
            , newBuffer: bufferStatus.target
            , adjustment: adjustment
            }

--------------------------------------------------------------------------------
-- SOLVENCY INVARIANT CHECK
--------------------------------------------------------------------------------

-- | Checks the core solvency invariant: Total JitoSOL backing >= FeelsSOL supply * exchange rate
checkSolvency :: FeelsSOLState -> Effect Boolean
checkSolvency state = do
  totalFeelsSOL <- read state.totalFeelsSOLSupply
  totalJitoSOLBacking <- read state.totalJitoSOLBacking
  currentBuffer <- read state.jitoSOLBuffer
  oraclePrice <- getOraclePrice state

  let totalJitoSOLAvailable = totalJitoSOLBacking + currentBuffer
      requiredJitoSOL = totalFeelsSOL / oraclePrice.price -- FeelsSOL / (JitoSOL/FeelsSOL) = JitoSOL

  if totalJitoSOLAvailable >= requiredJitoSOL
    then pure true
    else do
      log $ "SOLVENCY ALERT: JitoSOL available (" <> show totalJitoSOLAvailable <> ") < Required JitoSOL (" <> show requiredJitoSOL <> ")"
      pure false
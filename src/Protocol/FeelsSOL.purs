-- | FeelsSOL module for managing the synthetic SOL token.
-- | This module manages the conversion between JitoSOL and FeelsSOL,
-- | including synthetic asset creation and oracle-driven pricing.
module Protocol.FeelsSOL
  ( FeelsSOLState
  , TransformResult
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
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, read, write, new)
import Protocol.Token (TokenType(..), TokenAmount)
import FFI (currentTime)
import Protocol.Errors (ProtocolError(..))

--------------------------------------------------------------------------------
-- FeelsSOL Types
--------------------------------------------------------------------------------

-- Oracle price information
type OraclePrice =
  { price :: Number      -- JitoSOL/SOL exchange rate
  , timestamp :: Number  -- When price was fetched
  , confidence :: Number -- Price confidence (0.0-1.0)
  }

-- Result of minting FeelsSOL
type MintResult =
  { feelsSOLMinted :: Number
  , jitoSOLLocked :: Number
  , exchangeRate :: Number
  , timestamp :: Number
  }

-- Result of burning FeelsSOL
type BurnResult =
  { feelsSOLBurned :: Number
  , jitoSOLReleased :: Number
  , exchangeRate :: Number
  , timestamp :: Number
  }

-- Result of a transformation
type TransformResult =
  { inputAmount :: TokenAmount
  , outputAmount :: TokenAmount
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- FeelsSOL state for managing the synthetic SOL system
type FeelsSOLState =
  { totalFeelsSOLSupply :: Ref Number      -- Total FeelsSOL in circulation
  , totalJitoSOLBacking :: Ref Number      -- Total JitoSOL backing the synthetics
  , priceOracle :: Effect Number           -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Ref Number         -- Last oracle update timestamp
  , cachedPrice :: Ref (Maybe OraclePrice) -- Cached oracle price
  , entryFee :: Number                     -- Fee for entering (e.g., 0.001 = 0.1%)
  , exitFee :: Number                      -- Fee for exiting (e.g., 0.002 = 0.2%)
  , polAllocationRate :: Number            -- Portion of fees going to POL (e.g., 0.25 = 25%)
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
                     , confidence: 0.99  -- High confidence for JitoSOL
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
          
          -- Update state
          currentSupply <- read state.totalFeelsSOLSupply
          currentBacking <- read state.totalJitoSOLBacking
          _ <- write (currentSupply + feelsSOLAmount) state.totalFeelsSOLSupply
          _ <- write (currentBacking + jitoSOLAmount) state.totalJitoSOLBacking
          
          -- Return result
          timestamp <- currentTime
          pure $ Right
            { feelsSOLMinted: feelsSOLAmount
            , jitoSOLLocked: jitoSOLAmount
            , exchangeRate: exchangeRate
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
          
          -- Check if enough backing exists
          currentBacking <- read state.totalJitoSOLBacking
          if jitoSOLToRelease > currentBacking
            then pure $ Left "Insufficient JitoSOL backing"
            else do
              -- Update state
              _ <- write (currentSupply - feelsSOLAmount) state.totalFeelsSOLSupply
              _ <- write (currentBacking - jitoSOLToRelease) state.totalJitoSOLBacking
              
              -- Return result
              timestamp <- currentTime
              pure $ Right
                { feelsSOLBurned: feelsSOLAmount
                , jitoSOLReleased: jitoSOLToRelease
                , exchangeRate: exchangeRate
                , timestamp: timestamp
                }

-- Get collateral value in FeelsSOL terms
getCollateralValue :: TokenType -> Number -> Number -> Either String Number
getCollateralValue JitoSOL amount exchangeRate = Right (amount * exchangeRate)
getCollateralValue FeelsSOL amount _ = Right amount
getCollateralValue token _ _ = Left $ "Cannot value unsupported collateral: " <> show token

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

-- Initialize the FeelsSOL system with oracle and fee parameters
initFeelsSOL :: 
  (Effect Number) ->    -- Price oracle function
  Number ->             -- Entry fee
  Number ->             -- Exit fee
  Effect FeelsSOLState
initFeelsSOL oracle entryFee exitFee = do
  totalSupply <- new 0.0
  totalBacking <- new 0.0
  lastUpdate <- new 0.0
  cachedPrice <- new Nothing
  
  pure { totalFeelsSOLSupply: totalSupply
       , totalJitoSOLBacking: totalBacking
       , priceOracle: oracle
       , lastOracleUpdate: lastUpdate
       , cachedPrice: cachedPrice
       , entryFee: entryFee
       , exitFee: exitFee
       , polAllocationRate: 0.25  -- Default 25% to POL
       }

--------------------------------------------------------------------------------
-- System Entry (JitoSOL -> FeelsSOL)
--------------------------------------------------------------------------------

-- Enter the system by converting JitoSOL to FeelsSOL
enterSystem :: 
  FeelsSOLState ->
  String ->         -- User address (for future use)
  Number ->         -- Amount of JitoSOL to deposit
  Effect (Either ProtocolError TransformResult)
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
        Left err -> pure $ Left $ InvalidCommandError err
        Right result -> do
          -- Create transform result
          timestamp <- currentTime
          pure $ Right 
            { inputAmount: { tokenType: JitoSOL, amount: jitoAmount }
            , outputAmount: { tokenType: FeelsSOL, amount: result.feelsSOLMinted }
            , exchangeRate: result.exchangeRate
            , fee: feeAmount
            , timestamp: timestamp
            }

--------------------------------------------------------------------------------
-- System Exit (FeelsSOL -> JitoSOL)
--------------------------------------------------------------------------------

-- Exit the system by burning FeelsSOL to receive JitoSOL
exitSystem :: 
  FeelsSOLState ->
  String ->         -- User address (for future use)
  Number ->         -- Amount of FeelsSOL to burn
  Effect (Either ProtocolError TransformResult)
exitSystem state _user feelsAmount = do
  -- Validate amount
  if feelsAmount <= 0.0
    then pure $ Left (InvalidAmountError feelsAmount)
    else do
      -- Burn FeelsSOL to get JitoSOL
      burnResult <- burnFeelsSOL state feelsAmount
      
      case burnResult of
        Left err -> pure $ Left $ InvalidCommandError err
        Right result -> do
          -- Calculate fee on JitoSOL output
          let feeAmount = result.jitoSOLReleased * state.exitFee
              netJitoAmount = result.jitoSOLReleased - feeAmount
          
          -- Create transform result
          timestamp <- currentTime
          pure $ Right 
            { inputAmount: { tokenType: FeelsSOL, amount: feelsAmount }
            , outputAmount: { tokenType: JitoSOL, amount: netJitoAmount }
            , exchangeRate: result.exchangeRate
            , fee: feeAmount
            , timestamp: timestamp
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

-- | Get system health metrics
-- | Provides a high-level view of the FeelsSOL system's status
getSystemHealth :: FeelsSOLState -> Effect { collateralRatio :: Number, totalLocked :: Number, totalMinted :: Number, isHealthy :: Boolean }
getSystemHealth state = do
  ratio <- getCollateralRatio state
  locked <- getTotalLocked state
  minted <- getTotalMinted state
  let isHealthy = ratio >= 1.0  -- System is healthy if fully collateralized
  pure { collateralRatio: ratio, totalLocked: locked, totalMinted: minted, isHealthy }
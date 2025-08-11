-- | Gateway module for entering and exiting the Feels Protocol.
-- | This module manages the conversion between JitoSOL and FeelsSOL,
-- | including synthetic asset creation and oracle-driven pricing.
module Gateway
  ( GatewayState
  , SyntheticSOLState
  , TransformResult
  , TransformDirection(..)
  , OraclePrice
  , MintResult
  , BurnResult
  , initGateway
  , enterSystem
  , exitSystem
  , getExchangeRate
  , getTotalLocked
  , getTotalMinted
  -- High-level query functions
  , getOraclePrice
  , getTotalSupply
  , getSystemHealth
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, read, write, new)
import Token (TokenType(..), TokenAmount)
import FFI (currentTime)
import POL (POLState, contribute)
import Accounts (AccountRegistry, depositFromChain, withdrawToChain, getChainAccountBalance, getFeelsAccountBalance)
import Errors (ProtocolError(..))

--------------------------------------------------------------------------------
-- Gateway Types
--------------------------------------------------------------------------------

-- Direction of transformation
data TransformDirection
  = Enter  -- JitoSOL -> FeelsSOL
  | Exit   -- FeelsSOL -> JitoSOL

derive instance eqTransformDirection :: Eq TransformDirection

instance showTransformDirection :: Show TransformDirection where
  show Enter = "Enter (JitoSOL -> FeelsSOL)"
  show Exit = "Exit (FeelsSOL -> JitoSOL)"

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
  { direction :: TransformDirection
  , inputAmount :: TokenAmount
  , outputAmount :: TokenAmount
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- Synthetic SOL state (previously separate module)
type SyntheticSOLState =
  { totalFeelsSOLSupply :: Ref Number    -- Total FeelsSOL in circulation
  , totalJitoSOLBacking :: Ref Number    -- Total JitoSOL backing the synthetics
  , priceOracle :: Effect Number         -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Ref Number       -- Last oracle update timestamp
  , cachedPrice :: Ref (Maybe OraclePrice) -- Cached oracle price
  }

-- Gateway state for managing system entry/exit
type GatewayState =
  { syntheticSOL :: SyntheticSOLState    -- Synthetic FeelsSOL management
  , entryFee :: Number                   -- Fee for entering (e.g., 0.001 = 0.1%)
  , exitFee :: Number                    -- Fee for exiting (e.g., 0.002 = 0.2%)
  , accountRegistry :: AccountRegistry   -- Registry for both account types
  , polAllocationRate :: Number          -- Portion of fees going to POL (e.g., 0.25 = 25%)
  , polState :: POLState                 -- POL state for contributions
  }

--------------------------------------------------------------------------------
-- Synthetic SOL Functions
--------------------------------------------------------------------------------

-- Initialize synthetic SOL state
initSyntheticSOL :: (Effect Number) -> Effect SyntheticSOLState
initSyntheticSOL priceOracle = do
  totalSupply <- new 0.0
  totalBacking <- new 0.0
  lastUpdate <- new 0.0
  cachedPrice <- new Nothing
  
  pure { totalFeelsSOLSupply: totalSupply
       , totalJitoSOLBacking: totalBacking
       , priceOracle: priceOracle
       , lastOracleUpdate: lastUpdate
       , cachedPrice: cachedPrice
       }

-- Validate if a token can be used as collateral
validateCollateral :: TokenType -> Either String Unit
validateCollateral JitoSOL = Right unit
validateCollateral FeelsSOL = Right unit  -- Allow FeelsSOL for composability
validateCollateral token = Left $ "Unsupported collateral type: " <> show token

-- Get oracle price with caching
getOraclePrice :: SyntheticSOLState -> Effect OraclePrice
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
mintFeelsSOL :: SyntheticSOLState -> Number -> Effect (Either String MintResult)
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
burnFeelsSOL :: SyntheticSOLState -> Number -> Effect (Either String BurnResult)
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
getTotalSupply :: SyntheticSOLState -> Effect Number
getTotalSupply state = read state.totalFeelsSOLSupply

-- Get current collateral ratio (backing / supply)
getCollateralRatio :: SyntheticSOLState -> Effect Number
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
-- Gateway Initialization
--------------------------------------------------------------------------------

-- Initialize the gateway with oracle and fee parameters
initGateway :: 
  (Effect Number) ->    -- Price oracle function
  Number ->             -- Entry fee
  Number ->             -- Exit fee
  AccountRegistry ->    -- Account registry
  POLState ->           -- POL state
  Effect GatewayState
initGateway oracle entryFee exitFee accountRegistry pol = do
  syntheticSOLState <- initSyntheticSOL oracle
  pure { syntheticSOL: syntheticSOLState
       , entryFee: entryFee
       , exitFee: exitFee
       , accountRegistry: accountRegistry
       , polAllocationRate: 0.25  -- Default 25% to POL
       , polState: pol
       }

--------------------------------------------------------------------------------
-- System Entry (JitoSOL -> FeelsSOL)
--------------------------------------------------------------------------------

-- Enter the system by depositing JitoSOL and receiving FeelsSOL
enterSystem :: 
  GatewayState ->
  String ->         -- User address
  Number ->         -- Amount of JitoSOL to deposit
  Effect (Either ProtocolError TransformResult)
enterSystem state user jitoAmount = do
  -- Validate amount
  if jitoAmount <= 0.0
    then pure $ Left (InvalidAmountError jitoAmount)
    else do
      -- Check user has sufficient JitoSOL in ChainAccount
      jitoBalance <- getChainAccountBalance state.accountRegistry user
      if jitoBalance < jitoAmount
        then pure $ Left $ InsufficientBalanceError $ 
          "Insufficient JitoSOL balance. Required: " <> show jitoAmount <> ", Available: " <> show jitoBalance
        else do
          -- Calculate fee and net amount
          let feeAmount = jitoAmount * state.entryFee
              netJitoAmount = jitoAmount - feeAmount
          
          -- Mint FeelsSOL against the net JitoSOL amount
          mintResult <- mintFeelsSOL state.syntheticSOL netJitoAmount
          
          case mintResult of
            Left err -> pure $ Left $ InvalidCommandError err
            Right result -> do
              -- Move JitoSOL from chain account to protocol reserves
              _ <- depositFromChain state.accountRegistry user jitoAmount
              
              -- Credit FeelsSOL to user's FeelsAccount
              _ <- depositFromChain state.accountRegistry user result.feelsSOLMinted
              
              -- Allocate portion of fee to POL
              let polContribution = feeAmount * state.polAllocationRate
              contribute state.polState polContribution
              
              -- Create transform result
              timestamp <- currentTime
              pure $ Right 
                { direction: Enter
                , inputAmount: { tokenType: JitoSOL, amount: jitoAmount }
                , outputAmount: { tokenType: FeelsSOL, amount: result.feelsSOLMinted }
                , exchangeRate: result.exchangeRate
                , fee: feeAmount
                , timestamp: timestamp
                }

--------------------------------------------------------------------------------
-- System Exit (FeelsSOL -> JitoSOL)
--------------------------------------------------------------------------------

-- Exit the system by burning FeelsSOL and receiving JitoSOL
exitSystem :: 
  GatewayState ->
  String ->         -- User address
  Number ->         -- Amount of FeelsSOL to burn
  Effect (Either ProtocolError TransformResult)
exitSystem state user feelsAmount = do
  -- Validate amount
  if feelsAmount <= 0.0
    then pure $ Left (InvalidAmountError feelsAmount)
    else do
      -- Check user has sufficient FeelsSOL
      feelsBalance <- getFeelsAccountBalance state.accountRegistry user FeelsSOL
      if feelsBalance < feelsAmount
        then pure $ Left $ InsufficientBalanceError $ 
          "Insufficient FeelsOL balance. Required: " <> show feelsAmount <> ", Available: " <> show feelsBalance
        else do
          -- Burn FeelsSOL to get JitoSOL
          burnResult <- burnFeelsSOL state.syntheticSOL feelsAmount
          
          case burnResult of
            Left err -> pure $ Left $ InvalidCommandError err
            Right result -> do
              -- Calculate fee on JitoSOL output
              let feeAmount = result.jitoSOLReleased * state.exitFee
                  netJitoAmount = result.jitoSOLReleased - feeAmount
              
              -- Deduct FeelsSOL from user's FeelsAccount
              _ <- withdrawToChain state.accountRegistry user feelsAmount
              
              -- Credit JitoSOL to user's chain account
              _ <- withdrawToChain state.accountRegistry user netJitoAmount
              
              -- Allocate portion of fee to POL
              let polContribution = feeAmount * state.polAllocationRate
              contribute state.polState polContribution
              
              -- Create transform result
              timestamp <- currentTime
              pure $ Right 
                { direction: Exit
                , inputAmount: { tokenType: FeelsSOL, amount: feelsAmount }
                , outputAmount: { tokenType: JitoSOL, amount: netJitoAmount }
                , exchangeRate: result.exchangeRate
                , fee: feeAmount
                , timestamp: timestamp
                }

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get current exchange rate (JitoSOL/FeelsSOL)
getExchangeRate :: GatewayState -> Effect Number
getExchangeRate state = do
  oraclePrice <- getOraclePrice state.syntheticSOL
  pure oraclePrice.price

-- Get total JitoSOL locked in the gateway
getTotalLocked :: GatewayState -> Effect Number
getTotalLocked state = read state.syntheticSOL.totalJitoSOLBacking

-- Get total FeelsSOL minted
getTotalMinted :: GatewayState -> Effect Number
getTotalMinted state = getTotalSupply state.syntheticSOL

-- | Get system health metrics
-- | Provides a high-level view of the gateway's status
getSystemHealth :: GatewayState -> Effect { collateralRatio :: Number, totalLocked :: Number, totalMinted :: Number, isHealthy :: Boolean }
getSystemHealth state = do
  ratio <- getCollateralRatio state.syntheticSOL
  locked <- getTotalLocked state
  minted <- getTotalMinted state
  let isHealthy = ratio >= 1.0  -- System is healthy if fully collateralized
  pure { collateralRatio: ratio, totalLocked: locked, totalMinted: minted, isHealthy }
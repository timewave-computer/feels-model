-- Synthetic FeelsSOL asset management with oracle-driven pricing.
-- Handles the creation and destruction of FeelsSOL synthetic assets backed by JitoSOL collateral.
-- Contains oracle logic for JitoSOL/SOL pricing and the mechanics of synthetic asset minting/burning.
-- FeelsSOL represents synthetic SOL exposure backed by liquid staking tokens.
module SyntheticSOL
  ( SyntheticSOLState
  , OraclePrice
  , MintResult
  , BurnResult
  , initSyntheticSOL
  , validateCollateral
  , getOraclePrice
  , mintFeelsSOL
  , burnFeelsSOL
  , getCollateralValue
  , getTotalSupply
  , getCollateralRatio
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Token (TokenType(..), TokenAmount)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Synthetic SOL Types
--------------------------------------------------------------------------------

-- Oracle price information
type OraclePrice =
  { price :: Number      -- JitoSOL/SOL exchange rate
  , timestamp :: Number  -- When price was fetched
  , confidence :: Number -- Price confidence (0.0-1.0)
  }

-- State for managing synthetic FeelsSOL
type SyntheticSOLState =
  { totalFeelsSOLSupply :: Ref Number    -- Total FeelsSOL in circulation
  , totalJitoSOLBacking :: Ref Number    -- Total JitoSOL backing the synthetics
  , priceOracle :: Effect Number         -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Ref Number       -- Last oracle update timestamp
  , cachedPrice :: Ref (Maybe OraclePrice) -- Cached oracle price
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

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize synthetic SOL system
initSyntheticSOL :: Effect Number -> Effect SyntheticSOLState
initSyntheticSOL oracle = do
  supply <- new 0.0
  backing <- new 0.0
  lastUpdate <- new 0.0
  cachedPrice <- new Nothing
  pure { totalFeelsSOLSupply: supply
       , totalJitoSOLBacking: backing
       , priceOracle: oracle
       , lastOracleUpdate: lastUpdate
       , cachedPrice: cachedPrice
       }

--------------------------------------------------------------------------------
-- Oracle Functions
--------------------------------------------------------------------------------

-- Get current oracle price with caching
getOraclePrice :: SyntheticSOLState -> Effect OraclePrice
getOraclePrice state = do
  now <- currentTime
  lastUpdate <- read state.lastOracleUpdate
  
  -- Cache oracle price for 30 seconds
  if (now - lastUpdate) < 30000.0
    then do
      cached <- read state.cachedPrice
      case cached of
        Just price -> pure price
        Nothing -> fetchFreshPrice state now
    else fetchFreshPrice state now

-- Fetch fresh price from oracle
fetchFreshPrice :: SyntheticSOLState -> Number -> Effect OraclePrice
fetchFreshPrice state timestamp = do
  price <- state.priceOracle
  let oraclePrice = { price: price, timestamp: timestamp, confidence: 0.95 }
  
  -- Update cache
  _ <- write timestamp state.lastOracleUpdate
  _ <- write (Just oraclePrice) state.cachedPrice
  
  pure oraclePrice

--------------------------------------------------------------------------------
-- Collateral Validation
--------------------------------------------------------------------------------

-- Validate that only JitoSOL can be used as collateral
validateCollateral :: TokenType -> Either String Unit
validateCollateral JitoSOL = Right unit
validateCollateral token = Left $ "Invalid collateral: " <> show token <> ". Only JitoSOL accepted for FeelsSOL minting."

--------------------------------------------------------------------------------
-- Synthetic Asset Management
--------------------------------------------------------------------------------

-- Mint FeelsSOL by locking JitoSOL collateral
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

--------------------------------------------------------------------------------
-- Information Functions
--------------------------------------------------------------------------------

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
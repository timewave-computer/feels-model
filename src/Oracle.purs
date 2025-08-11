-- | Oracle Module - Simplified price oracle for the pool-centric system
-- |
-- | This module provides basic price tracking functionality
-- | until the full oracle system is implemented.
module Oracle
  ( Oracle
  , PricePoint
  , TWAPWindow(..)
  , MarketSnapshot
  , initOracle
  , updatePrice
  , getCurrentPrice
  , getTWAP
  , getVolatility
  , takeMarketSnapshot
  ) where

import Prelude
import Data.Array ((:), take, length, filter)
import Data.Int as Int
import Data.Int (toNumber)
import Data.Number (sqrt)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Price oracle state
type Oracle = Ref
  { currentPrice :: Number
  , priceHistory :: Array PricePoint
  , lastUpdate :: Number
  }

-- | Price point in history
type PricePoint =
  { price :: Number
  , timestamp :: Number
  }

-- | TWAP calculation windows
data TWAPWindow
  = FiveMinutes
  | FifteenMinutes
  | OneHour

derive instance eqTWAPWindow :: Eq TWAPWindow

instance showTWAPWindow :: Show TWAPWindow where
  show FiveMinutes = "5m"
  show FifteenMinutes = "15m"
  show OneHour = "1h"

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize oracle with starting price
initOracle :: Number -> Effect Oracle
initOracle initialPrice = do
  timestamp <- currentTime
  new
    { currentPrice: initialPrice
    , priceHistory: [{ price: initialPrice, timestamp }]
    , lastUpdate: timestamp
    }

--------------------------------------------------------------------------------
-- Price Updates
--------------------------------------------------------------------------------

-- | Update current price
updatePrice :: Number -> Oracle -> Effect Unit
updatePrice newPrice oracleRef = do
  timestamp <- currentTime
  modify_ (\oracle ->
    oracle
      { currentPrice = newPrice
      , priceHistory = take 100 ({ price: newPrice, timestamp } : oracle.priceHistory)
      , lastUpdate = timestamp
      }
  ) oracleRef

-- | Get current price
getCurrentPrice :: Oracle -> Effect Number
getCurrentPrice oracleRef = do
  oracle <- read oracleRef
  pure oracle.currentPrice

--------------------------------------------------------------------------------
-- TWAP Calculations
--------------------------------------------------------------------------------

-- | Calculate TWAP for given window
getTWAP :: TWAPWindow -> Oracle -> Effect Number
getTWAP window oracleRef = do
  oracle <- read oracleRef
  timestamp <- currentTime
  let windowMs = windowToMs window
      cutoff = timestamp - windowMs
      relevantPrices = filter (\p -> p.timestamp >= cutoff) oracle.priceHistory
  
  if length relevantPrices == 0
  then pure oracle.currentPrice
  else pure $ sum (map _.price relevantPrices) / toNumber (length relevantPrices)

-- | Convert window to milliseconds
windowToMs :: TWAPWindow -> Number
windowToMs FiveMinutes = 300000.0     -- 5 * 60 * 1000
windowToMs FifteenMinutes = 900000.0  -- 15 * 60 * 1000
windowToMs OneHour = 3600000.0        -- 60 * 60 * 1000

--------------------------------------------------------------------------------
-- Volatility
--------------------------------------------------------------------------------

-- | Calculate simple volatility
getVolatility :: Oracle -> Effect Number
getVolatility oracleRef = do
  oracle <- read oracleRef
  let prices = map _.price $ take 20 oracle.priceHistory
  
  if length prices < 2
  then pure 0.0
  else do
    let mean = sum prices / Int.toNumber (length prices)
        variance = sum (map (\p -> (p - mean) * (p - mean)) prices) / Int.toNumber (length prices)
    pure $ sqrt variance / mean  -- Coefficient of variation

--------------------------------------------------------------------------------
-- Market Snapshot
--------------------------------------------------------------------------------

-- | Take market snapshot for historical tracking
takeMarketSnapshot :: Oracle -> Effect MarketSnapshot
takeMarketSnapshot oracleRef = do
  oracle <- read oracleRef
  vol <- getVolatility oracleRef
  twap5m <- getTWAP FiveMinutes oracleRef
  
  pure
    { spot: oracle.currentPrice
    , twap5m
    , volatility: vol
    , timestamp: oracle.lastUpdate
    }

-- | Market snapshot type
type MarketSnapshot =
  { spot :: Number
  , twap5m :: Number
  , volatility :: Number
  , timestamp :: Number
  }

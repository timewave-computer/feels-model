-- | Oracle System for Price Tracking and TWAP Calculations
-- |
-- | This module provides essential price oracle functionality for the Feels Protocol:
-- |
-- | Key Features:
-- | - Real-time spot price tracking with historical data retention
-- | - Time-Weighted Average Price (TWAP) calculations over multiple windows
-- | - Volatility calculations for risk assessment
-- | - Market snapshots for historical analysis and monitoring
-- |
-- | The oracle maintains a circular buffer of price history for efficient
-- | memory usage while providing accurate TWAP and volatility metrics.
module Protocol.Oracle
  ( Oracle
  , PricePoint
  , TWAPWindow(..)
  , MarketSnapshot
  , BufferConfig
  , initOracle
  , updatePrice
  , updatePriceWithTimestamp
  , getCurrentPrice
  , getTWAP
  , getVolatility
  , takeMarketSnapshot
  , getBufferTarget
  , updateBufferConfig
  , isBufferHealthy
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
-- ORACLE TYPE DEFINITIONS
--------------------------------------------------------------------------------
-- Core data structures for price tracking and historical data

-- | Price oracle state with historical data retention and buffer management
-- | Uses a Ref for mutable state to handle frequent price updates efficiently
type Oracle = Ref
  { currentPrice :: Number            -- Latest spot price
  , priceHistory :: Array PricePoint  -- Historical price data (circular buffer)
  , lastUpdate :: Number              -- Timestamp of last price update
  , bufferConfig :: BufferConfig      -- Buffer management configuration
  }

-- | Individual price observation with timestamp
-- | Forms the basis for TWAP calculations and volatility analysis
type PricePoint =
  { price :: Number       -- Price at this point in time
  , timestamp :: Number   -- Unix timestamp in milliseconds
  }

-- | Time windows for TWAP calculations
-- | Common intervals used in DeFi for price stability assessment
data TWAPWindow
  = FiveMinutes     -- Short-term price movements
  | FifteenMinutes  -- Medium-term trend analysis
  | OneHour         -- Longer-term price stability

derive instance eqTWAPWindow :: Eq TWAPWindow

instance showTWAPWindow :: Show TWAPWindow where
  show FiveMinutes = "5m"
  show FifteenMinutes = "15m"
  show OneHour = "1h"

-- | Buffer management configuration
-- | Manages jitoSOL reserves for withdrawal liquidity and system stability
type BufferConfig =
  { targetRatio :: Number     -- Target buffer as ratio of total backing (e.g., 0.10 = 10%)
  , minRatio :: Number        -- Minimum buffer ratio before health warning (e.g., 0.05 = 5%)
  , maxRatio :: Number        -- Maximum buffer ratio before rebalancing (e.g., 0.20 = 20%)
  , rebalanceThreshold :: Number -- Threshold for automatic rebalancing (e.g., 0.02 = 2%)
  , daoManaged :: Boolean     -- Whether buffer is managed by DAO governance
  }

-- | Market snapshot for historical tracking and analysis
-- | Provides a comprehensive view of market conditions at a point in time
type MarketSnapshot =
  { spot :: Number        -- Current spot price
  , twap5m :: Number      -- 5-minute TWAP
  , volatility :: Number  -- Price volatility coefficient
  , timestamp :: Number   -- Snapshot timestamp
  , bufferHealth :: Number -- Buffer health ratio (0.0-1.0)
  }

--------------------------------------------------------------------------------
-- ORACLE INITIALIZATION
--------------------------------------------------------------------------------
-- Functions for setting up the oracle system

-- | Initialize oracle with starting price and default buffer configuration
-- | Creates oracle state with initial price point in history and buffer management
initOracle :: Number -> Effect Oracle
initOracle initialPrice = do
  timestamp <- currentTime
  let defaultBufferConfig = 
        { targetRatio: 0.01      -- 1% buffer target
        , minRatio: 0.005        -- 0.5% minimum buffer
        , maxRatio: 0.02         -- 2% maximum buffer
        , rebalanceThreshold: 0.005 -- 0.5% rebalancing threshold
        , daoManaged: false      -- Start with algorithmic management
        }
  new
    { currentPrice: initialPrice
    , priceHistory: [{ price: initialPrice, timestamp }]
    , lastUpdate: timestamp
    , bufferConfig: defaultBufferConfig
    }

--------------------------------------------------------------------------------
-- PRICE UPDATE OPERATIONS
--------------------------------------------------------------------------------
-- Functions for maintaining current price data and history

-- | Update current price and add to historical record
-- | Maintains circular buffer of last 100 price points for efficiency
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

-- | Update price with specific timestamp (for simulations)
-- | Allows controlled time progression in simulated environments
updatePriceWithTimestamp :: Number -> Number -> Oracle -> Effect Unit
updatePriceWithTimestamp newPrice timestamp oracleRef = do
  modify_ (\oracle ->
    oracle
      { currentPrice = newPrice
      , priceHistory = take 100 ({ price: newPrice, timestamp } : oracle.priceHistory)
      , lastUpdate = timestamp
      }
  ) oracleRef

-- | Get current spot price from oracle
getCurrentPrice :: Oracle -> Effect Number
getCurrentPrice oracleRef = do
  oracle <- read oracleRef
  pure oracle.currentPrice

--------------------------------------------------------------------------------
-- TWAP CALCULATION ENGINE
--------------------------------------------------------------------------------
-- Time-weighted average price calculations for different time windows

-- | Calculate Time-Weighted Average Price for specified window
-- | Returns current price if insufficient historical data available
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

-- | Convert TWAP window enum to milliseconds
-- | Provides precise time calculations for TWAP windows
windowToMs :: TWAPWindow -> Number
windowToMs FiveMinutes = 300000.0     -- 5 * 60 * 1000
windowToMs FifteenMinutes = 900000.0  -- 15 * 60 * 1000
windowToMs OneHour = 3600000.0        -- 60 * 60 * 1000

--------------------------------------------------------------------------------
-- VOLATILITY ANALYSIS
--------------------------------------------------------------------------------
-- Statistical analysis of price movements for risk assessment

-- | Calculate price volatility using coefficient of variation
-- | Uses last 20 price points for volatility calculation
getVolatility :: Oracle -> Effect Number
getVolatility oracleRef = do
  oracle <- read oracleRef
  let prices = map _.price $ take 20 oracle.priceHistory
  
  if length prices < 2
  then pure 0.0
  else do
    let mean = sum prices / Int.toNumber (length prices)
        variance = sum (map (\p -> (p - mean) * (p - mean)) prices) / Int.toNumber (length prices)
    pure $ sqrt variance / mean  -- Coefficient of variation as volatility measure

--------------------------------------------------------------------------------
-- MARKET SNAPSHOT UTILITIES
--------------------------------------------------------------------------------
-- Comprehensive market state capture for analysis and monitoring

-- | Create comprehensive market snapshot
-- | Combines spot price, TWAP, volatility, and buffer health for complete market view
takeMarketSnapshot :: Oracle -> Effect MarketSnapshot
takeMarketSnapshot oracleRef = do
  oracle <- read oracleRef
  vol <- getVolatility oracleRef
  twap5m <- getTWAP FiveMinutes oracleRef
  bufferHealth <- calculateBufferHealth oracle.bufferConfig
  
  pure
    { spot: oracle.currentPrice
    , twap5m
    , volatility: vol
    , timestamp: oracle.lastUpdate
    , bufferHealth
    }
  where
    -- Calculate buffer health as a simple ratio (can be enhanced with actual buffer data)
    calculateBufferHealth config = pure $ config.targetRatio / config.maxRatio

--------------------------------------------------------------------------------
-- BUFFER MANAGEMENT FUNCTIONS
--------------------------------------------------------------------------------
-- Functions for managing jitoSOL withdrawal buffer and system liquidity

-- | Get target buffer amount for given total backing
-- | Calculates the ideal buffer size based on current configuration
getBufferTarget :: Number -> Oracle -> Effect Number
getBufferTarget totalBacking oracleRef = do
  oracle <- read oracleRef
  pure $ totalBacking * oracle.bufferConfig.targetRatio

-- | Update buffer configuration (typically called by DAO governance)
-- | Allows dynamic adjustment of buffer parameters based on market conditions
updateBufferConfig :: BufferConfig -> Oracle -> Effect Unit
updateBufferConfig newConfig oracleRef = do
  modify_ (\oracle -> oracle { bufferConfig = newConfig }) oracleRef

-- | Check if current buffer level is healthy
-- | Returns true if buffer is above minimum threshold
isBufferHealthy :: Number -> Number -> Oracle -> Effect Boolean
isBufferHealthy currentBuffer totalBacking oracleRef = do
  oracle <- read oracleRef
  let bufferRatio = if totalBacking > 0.0 
                   then currentBuffer / totalBacking 
                   else 1.0  -- Consider healthy if no backing yet
  pure $ bufferRatio >= oracle.bufferConfig.minRatio

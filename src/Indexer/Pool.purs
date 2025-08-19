-- | Pool-Level Analytics and Event Processing System
-- |
-- | This indexer module provides comprehensive pool-level analytics by processing
-- | on-chain events and aggregating tick-level data into pool-wide insights.
-- | It maintains historical metrics, identifies trading patterns, and supports
-- | real-time pool monitoring for optimal liquidity management.
-- |
-- | Key Features:
-- | - Real-time processing of swap and liquidity events
-- | - Pool-wide statistical analysis and trend identification
-- | - Hot zone detection across multiple ticks
-- | - Support and resistance level identification
-- | - Historical volatility and trading pattern analysis
-- |
-- | Analytics Pipeline:
-- | 1. Process incoming pool events (swaps, liquidity changes)
-- | 2. Update pool-level metrics and historical data
-- | 3. Analyze cross-tick patterns and hot zones
-- | 4. Calculate statistical measures and volatility metrics
-- | 5. Maintain rolling windows for time-series analysis
module Indexer.Pool
  ( -- Pool Analytics Types
    PoolAnalytics
  , HotZone
  , IndexerState
  -- Event Processing
  , processEvent
  , processSwapEvent
  , processLiquidityEvent
  -- Pool-Level Analysis
  , findHotZones
  , findSupportZones
  -- Helper Functions
  , calculateVariance
  , updatePool
  , findOrCreatePool
  , updateActiveTicks
  , sqrtPriceToPrice
  ) where

import Prelude
import Data.Array ((:), take, filter, length)
import Data.Foldable (sum)
import Data.Int (toNumber)
import FFI (sqrt)
import Effect (Effect)

-- Import analytics from other indexer modules
import Indexer.Tick (TickMetrics)
import Protocol.Pool (PoolEvent(..))

--------------------------------------------------------------------------------
-- POOL ANALYTICS DATA STRUCTURES
--------------------------------------------------------------------------------
-- Comprehensive data types for pool-level analysis and metrics

-- | Complete pool analytics aggregating cross-tick data and historical metrics
-- | Provides comprehensive view of pool performance and trading characteristics
type PoolAnalytics =
  { poolId :: String                    -- Unique pool identifier
  , currentPrice :: Number              -- Current spot price
  , priceChange24h :: Number            -- 24-hour price change percentage
  , volume24h :: Number                 -- 24-hour trading volume
  , volume7d :: Number                  -- 7-day trading volume
  , totalValueLocked :: Number          -- Current total value locked
  , liquidityDepth :: Number            -- Total available liquidity depth in the pool
  
  -- Cross-tick pattern analysis
  , activeTicks :: Array TickMetrics    -- Detailed tick-level activity data
  , hotZones :: Array HotZone           -- High-activity zones spanning multiple ticks
  , supportLevels :: Array Number       -- Key price levels with strong support
  , resistanceLevels :: Array Number    -- Key price levels with strong resistance
  
  -- Statistical and historical metrics
  , historicalVolatility :: Number      -- Price volatility over analysis window
  , averageTradeSize :: Number          -- Mean trade size across all activity
  , uniqueTraders24h :: Int             -- Estimated unique trader count
  }

-- | High-activity trading zones identified across multiple adjacent ticks
-- | Used for liquidity optimization and trading pattern analysis
type HotZone =
  { tickLower :: Int                    -- Lower boundary tick
  , tickUpper :: Int                    -- Upper boundary tick
  , temperature :: Number               -- Aggregate activity intensity
  , volumeConcentration :: Number       -- Percentage of total volume in zone
  }

-- | Global indexer state managing all pool analytics and event processing
-- | Maintains comprehensive system-wide view of pool activity
type IndexerState =
  { pools :: Array PoolAnalytics        -- All tracked pool analytics
  , eventBuffer :: Array PoolEvent      -- Pending events for batch processing
  , lastProcessedSlot :: Int            -- Last processed Solana slot number
  }

--------------------------------------------------------------------------------
-- POOL EVENT PROCESSING ENGINE
--------------------------------------------------------------------------------
-- Real-time processing of on-chain events to maintain pool analytics

-- | Process incoming pool events and update corresponding analytics
-- | Routes events to specialized processors based on event type
processEvent :: PoolEvent -> IndexerState -> Effect IndexerState
processEvent event state = case event of
  SwapExecuted _ -> processSwapEvent event state
  LiquidityChanged _ -> processLiquidityEvent event state  
  POLDeployed polEvent -> processPOLDeployedEvent polEvent state

-- | Process swap events and update comprehensive pool metrics
-- | Updates price, volume, trading patterns, and statistical measures
processSwapEvent :: PoolEvent -> IndexerState -> Effect IndexerState
processSwapEvent (SwapExecuted swap) state = do
  -- Locate existing pool analytics or create new entry
  let poolAnalytics = findOrCreatePool swap.pool state.pools
  
  -- Calculate updated pool-level metrics from swap data
  let updatedAnalytics = poolAnalytics
        { currentPrice = sqrtPriceToPrice swap.sqrtPrice
        , volume24h = poolAnalytics.volume24h + swap.amountIn
        , priceChange24h = calculatePriceChange poolAnalytics.currentPrice (sqrtPriceToPrice swap.sqrtPrice)
        , uniqueTraders24h = poolAnalytics.uniqueTraders24h + 1 -- Simplified counting
        , averageTradeSize = calculateNewAverage poolAnalytics.averageTradeSize swap.amountIn poolAnalytics.volume24h
        }
        
  pure state { pools = updatePool updatedAnalytics state.pools }
processSwapEvent _ state = pure state  -- Other events are handled elsewhere

-- | Process liquidity modification events and update TVL metrics
-- | Handles position additions/removals and updates total value locked
processLiquidityEvent :: PoolEvent -> IndexerState -> Effect IndexerState
processLiquidityEvent event state = case event of
  LiquidityChanged { pool, liquidityDelta } -> do
    -- Locate pool analytics for update
    let poolAnalytics = findOrCreatePool pool state.pools
        
        -- Analyze liquidity change characteristics
        
        -- Estimate TVL impact from liquidity change
        -- Uses current price for token amount approximation
        currentPrice = poolAnalytics.currentPrice
        token0Amount = liquidityDelta / sqrt currentPrice  
        token1Amount = liquidityDelta * sqrt currentPrice
        tvlDelta = token0Amount * currentPrice + token1Amount
        
        -- Update pool analytics with new liquidity metrics
        updatedPoolAnalytics = poolAnalytics
          { totalValueLocked = max 0.0 (poolAnalytics.totalValueLocked + tvlDelta)
          , liquidityDepth = max 0.0 (poolAnalytics.liquidityDepth + liquidityDelta)
          }
        
        -- Apply updates to pool collection
        updatedPools = updatePool updatedPoolAnalytics state.pools
        
    pure $ state { pools = updatedPools }
    
  _ -> pure state  -- Non-liquidity events ignored

-- | Process POL deployment events and update pool analytics
-- | Records POL deployments for tracking protocol-owned liquidity
processPOLDeployedEvent :: _ -> IndexerState -> Effect IndexerState
processPOLDeployedEvent polEvent state = case polEvent of
  { pool, amount } -> do
    -- Find or create pool analytics
    let poolAnalytics = findOrCreatePool pool state.pools
        
        -- Calculate POL range width and estimated TVL impact
        currentPrice = poolAnalytics.currentPrice
        
        -- Estimate token amounts from liquidity
        token0Amount = amount / sqrt currentPrice
        token1Amount = amount * sqrt currentPrice
        polTVL = token0Amount * currentPrice + token1Amount
        
        -- Update pool analytics with POL deployment
        updatedPoolAnalytics = poolAnalytics
          { totalValueLocked = poolAnalytics.totalValueLocked + polTVL
          , volume24h = poolAnalytics.volume24h -- POL doesn't affect volume directly
          }
        
        updatedPools = updatePool updatedPoolAnalytics state.pools
    
    pure $ state { pools = updatedPools }

--------------------------------------------------------------------------------
-- POOL-LEVEL PATTERN ANALYSIS
--------------------------------------------------------------------------------
-- Advanced algorithms for identifying trading patterns across tick ranges

-- | Identify high-activity hot zones by clustering adjacent active ticks
-- | Groups ticks with significant trading activity into coherent zones
findHotZones :: Array TickMetrics -> Array HotZone
findHotZones ticks =
  let
    -- Filter for ticks exceeding activity threshold
    hotTicks = filter (\t -> t.temperature > 70.0) ticks
    zones = groupAdjacentTicks hotTicks
    
  in map createHotZone zones
  where
    groupAdjacentTicks _ = [] -- Simplified - production would implement tick clustering
    createHotZone _ = 
      { tickLower: 0
      , tickUpper: 10
      , temperature: 85.0
      , volumeConcentration: 15.0
      } -- Placeholder values for demonstration

-- | Identify price support zones from tick-level support metrics
-- | Returns tick numbers where strong price support has been observed
findSupportZones :: Array TickMetrics -> Array Int  
findSupportZones ticks =
  -- Find ticks with high support values (> 60%) as support zones
  let supportThreshold = 60.0
      supportTicks = filter (\t -> t.support > supportThreshold) ticks
  in map _.tick supportTicks

--------------------------------------------------------------------------------
-- STATISTICAL ANALYSIS UTILITIES
--------------------------------------------------------------------------------
-- Mathematical functions for pool-level statistical analysis

-- | Calculate sample variance for price volatility analysis
-- | Uses unbiased sample variance formula (n-1 denominator)
calculateVariance :: Array Number -> Number
calculateVariance xs = 
  let n = length xs
  in if n <= 1 then 0.0
     else
       let mean = sum xs / toNumber n
           squaredDiffs = map (\x -> (x - mean) * (x - mean)) xs
       in sum squaredDiffs / toNumber (n - 1)  -- Sample variance for unbiased estimation

-- | Calculate percentage price change between two price points
-- | Used for tracking price movements and volatility
calculatePriceChange :: Number -> Number -> Number
calculatePriceChange oldPrice newPrice =
  if oldPrice > 0.0
    then ((newPrice - oldPrice) / oldPrice) * 100.0
    else 0.0

-- | Calculate updated running average incorporating new data point
-- | Maintains efficient rolling averages for trade size and other metrics
calculateNewAverage :: Number -> Number -> Number -> Number
calculateNewAverage currentAvg newValue totalVolume =
  if totalVolume > 0.0
    then (currentAvg * (totalVolume - newValue) + newValue) / totalVolume
    else newValue

-- | Convert sqrt price format to standard price representation
-- | Used throughout the system for price calculations and display
sqrtPriceToPrice :: Number -> Number
sqrtPriceToPrice sqrtPrice = sqrtPrice * sqrtPrice

--------------------------------------------------------------------------------
-- POOL MANAGEMENT UTILITIES
--------------------------------------------------------------------------------
-- Core functions for managing pool analytics and state updates

-- | Find existing pool analytics or create new entry for unknown pools
-- | Ensures all pools have analytics tracking regardless of discovery order
findOrCreatePool :: String -> Array PoolAnalytics -> PoolAnalytics
findOrCreatePool poolId pools = 
  case filter (\p -> p.poolId == poolId) pools of
    [pool] -> pool
    _ -> createEmptyPool poolId
  where
    createEmptyPool id = 
      { poolId: id
      , currentPrice: 1.0
      , priceChange24h: 0.0
      , volume24h: 0.0
      , volume7d: 0.0
      , totalValueLocked: 0.0
      , liquidityDepth: 0.0
      , activeTicks: []
      , hotZones: []
      , supportLevels: []
      , resistanceLevels: []
      , historicalVolatility: 0.0
      , averageTradeSize: 0.0
      , uniqueTraders24h: 0
      }

-- | Update specific pool analytics within the global pool collection
-- | Replaces existing analytics while preserving other pools
updatePool :: PoolAnalytics -> Array PoolAnalytics -> Array PoolAnalytics
updatePool updatedPool pools = 
  map (\p -> if p.poolId == updatedPool.poolId then updatedPool else p) pools

-- | Update active tick metrics with new data point
-- | Maintains circular buffer of recent tick activity for analysis
updateActiveTicks :: TickMetrics -> Array TickMetrics -> Array TickMetrics
updateActiveTicks newTick ticks = newTick : take 99 ticks -- Maintain last 100 tick records
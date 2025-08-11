-- | Pool-level analytics and indexer for complex pool analysis
-- | Processes events and maintains pool-wide historical data
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
import Data.Array ((:), take, filter, length, head, last)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Ord (max, abs)
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)

-- Import analytics from other indexer modules
import Indexer.Tick (TickMetrics, TickAnalytics, TickEvent)
import Indexer.POL (processPOLEvent)
import Protocol.Pool (PoolEvent(..), POLTriggerType(..))

--------------------------------------------------------------------------------
-- Pool-Level Analytics Types (POL types moved to POL module)
--------------------------------------------------------------------------------

-- | Comprehensive pool analytics aggregating tick data
type PoolAnalytics =
  { poolId :: String
  , currentPrice :: Number
  , priceChange24h :: Number
  , volume24h :: Number
  , volume7d :: Number
  , totalValueLocked :: Number
  
  -- Cross-tick analysis
  , activeTicks :: Array TickMetrics
  , hotZones :: Array HotZone        -- High activity areas spanning multiple ticks
  , supportLevels :: Array Number    -- Key price levels with strong support
  , resistanceLevels :: Array Number -- Key price levels with strong resistance
  
  -- Pool-wide historical metrics
  , historicalVolatility :: Number   -- Price volatility over time
  , averageTradeSize :: Number       -- Mean trade size across all ticks
  , uniqueTraders24h :: Int          -- Count of unique traders
  }

-- | High activity zones spanning multiple ticks
type HotZone =
  { tickLower :: Int
  , tickUpper :: Int
  , temperature :: Number            -- Aggregate temperature across ticks
  , volumeConcentration :: Number    -- % of total volume in this zone
  }

-- | Global indexer state tracking all pools
type IndexerState =
  { pools :: Array PoolAnalytics
  , eventBuffer :: Array PoolEvent
  , lastProcessedSlot :: Int
  }

--------------------------------------------------------------------------------
-- Pool Event Processing
--------------------------------------------------------------------------------

-- | Process on-chain events and update pool-level metrics
processEvent :: PoolEvent -> IndexerState -> Effect IndexerState
processEvent event state = case event of
  SwapExecuted swap -> processSwapEvent swap state
  LiquidityChanged liq -> processLiquidityEvent liq state  
  POLDeployed pol -> processPOLEvent pol state

-- | Process swap events and update pool analytics
processSwapEvent :: _ -> IndexerState -> Effect IndexerState
processSwapEvent swap state = do
  -- Find or create pool analytics
  let poolAnalytics = findOrCreatePool swap.pool state.pools
  
  -- Update pool-level metrics (not individual tick metrics)
  let updatedAnalytics = poolAnalytics
        { currentPrice = sqrtPriceToPrice swap.sqrtPrice
        , volume24h = poolAnalytics.volume24h + swap.amountIn
        , priceChange24h = calculatePriceChange poolAnalytics.currentPrice (sqrtPriceToPrice swap.sqrtPrice)
        , uniqueTraders24h = poolAnalytics.uniqueTraders24h + 1 -- Simplified
        , averageTradeSize = calculateNewAverage poolAnalytics.averageTradeSize swap.amountIn poolAnalytics.volume24h
        }
        
  pure state { pools = updatePool updatedAnalytics state.pools }

-- | Process liquidity events 
processLiquidityEvent :: _ -> IndexerState -> Effect IndexerState
processLiquidityEvent liq state = do
  -- Update pool TVL and liquidity metrics
  pure state -- Simplified for now

--------------------------------------------------------------------------------
-- Pool-Level Analysis (POL analysis moved to POL module)
--------------------------------------------------------------------------------

-- | Find hot zones by grouping adjacent high-activity ticks
findHotZones :: Array TickMetrics -> Array HotZone
findHotZones ticks =
  let
    -- Group adjacent ticks with high temperature (>70)
    hotTicks = filter (\t -> t.temperature > 70.0) ticks
    zones = groupAdjacentTicks hotTicks
    
  in map createHotZone zones
  where
    groupAdjacentTicks _ = [] -- Simplified - would group by adjacent tick numbers
    createHotZone zone = 
      { tickLower: 0
      , tickUpper: 10
      , temperature: 85.0
      , volumeConcentration: 15.0
      } -- Simplified

-- | Find support zones by analyzing tick support scores
findSupportZones :: Array TickMetrics -> Array Int  
findSupportZones ticks =
  let
    -- Find ticks with strong support (>70% support ratio)
    supportTicks = filter (\t -> t.support > 70.0) ticks
  in map _.tick supportTicks -- Extract tick numbers

--------------------------------------------------------------------------------
-- Pool-Level Helper Functions  
--------------------------------------------------------------------------------

-- | Calculate sample variance for pool price history
calculateVariance :: Array Number -> Number
calculateVariance xs = 
  let n = length xs
  in if n <= 1 then 0.0
     else
       let mean = sum xs / toNumber n
           squaredDiffs = map (\x -> (x - mean) * (x - mean)) xs
       in sum squaredDiffs / toNumber (n - 1)  -- Use sample variance (n-1)

-- | Calculate price change percentage
calculatePriceChange :: Number -> Number -> Number
calculatePriceChange oldPrice newPrice =
  if oldPrice > 0.0
    then ((newPrice - oldPrice) / oldPrice) * 100.0
    else 0.0

-- | Calculate new running average
calculateNewAverage :: Number -> Number -> Number -> Number
calculateNewAverage currentAvg newValue totalVolume =
  if totalVolume > 0.0
    then (currentAvg * (totalVolume - newValue) + newValue) / totalVolume
    else newValue

-- | Convert sqrt price to regular price
sqrtPriceToPrice :: Number -> Number
sqrtPriceToPrice sqrtPrice = sqrtPrice * sqrtPrice

--------------------------------------------------------------------------------
-- Placeholder Functions (would be implemented based on specific requirements)
--------------------------------------------------------------------------------

findOrCreatePool :: String -> Array PoolAnalytics -> PoolAnalytics
findOrCreatePool poolId pools = 
  case filter (\p -> p.poolId == poolId) pools of
    [pool] -> pool
    _ -> createEmptyPool poolId
  where
    createEmptyPool id = unsafeCoerce { poolId: id } -- Simplified

updatePool :: PoolAnalytics -> Array PoolAnalytics -> Array PoolAnalytics
updatePool updatedPool pools = 
  map (\p -> if p.poolId == updatedPool.poolId then updatedPool else p) pools

updateActiveTicks :: TickMetrics -> Array TickMetrics -> Array TickMetrics
updateActiveTicks newTick ticks = newTick : take 99 ticks -- Keep last 100 ticks
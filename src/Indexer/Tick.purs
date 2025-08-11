-- | Individual tick analytics and metrics
-- | Rich metrics calculated from on-chain events for specific ticks
module Indexer.Tick
  ( -- Core Types
    TickMetrics
  , TickAnalytics
  , TickState(..)
  , TickEvent
  , Event
  , EventType(..)
  -- Analysis Functions  
  , calculateMetrics
  , analyzeTickHistory
  , isFloorCandidate
  -- Individual Metric Calculations
  , calculateTemperature
  , calculateSupport
  , calculateReliability
  , calculateVelocity
  , calculateMomentum
  , calculateDepth
  -- Helper Functions
  , determineTickState
  , getSwapEvent
  , getMintEvent
  , getBurnEvent
  , getLiquidityChange
  , calculateVolume
  , getCurrentLiquidity
  , calculateAvgTradeSize
  ) where

import Prelude
import Data.Array ((:), take, drop, filter, length, head, last, elemIndex)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Function (identity)
import Data.Foldable (sum, foldl)
import Data.Int as Int
import Data.Ord (min, max, abs)
import Data.Number (isNaN, isFinite)

--------------------------------------------------------------------------------
-- Tick-Specific Types
--------------------------------------------------------------------------------

-- | Rich tick analytics maintained off-chain
type TickAnalytics =
  { tick :: Int                    -- Tick index
  , price :: Number                -- Price at this tick
  , volume24h :: Number            -- Rolling 24h volume
  , volume7d :: Number             -- Rolling 7d volume
  , events :: Array Event          -- Recent activity history
  , metrics :: TickMetrics         -- Derived metrics
  , state :: TickState             -- Analytical state
  }

-- | Comprehensive metrics calculated from tick event history
type TickMetrics =
  { -- Core metrics
    support :: Number              -- Buy volume - sell volume (normalized %)
  , temperature :: Number          -- Activity level (0 = cold, 100 = hot)
  , reliability :: Number          -- Consistency of liquidity provision
  , velocity :: Number             -- Rate of liquidity change
  , momentum :: Number             -- Directional bias
  , depth :: Number                -- Liquidity depth score
  
  -- Extended metrics for pool analysis
  , liquidity :: Number            -- Current tick liquidity
  , volume24h :: Number            -- 24h volume through this tick
  , volume7d :: Number             -- 7d volume through this tick
  , feeRevenue24h :: Number        -- Fee revenue generated
  
  -- Historical data
  , priceHistory :: Array Number
  , volumeHistory :: Array Number
  , eventLog :: Array TickEvent
  }

-- | Detailed event tracking for ticks
type TickEvent =
  { timestamp :: Number
  , eventType :: String
  , volume :: Number
  , liquidity :: Number
  , priceImpact :: Number
  }

-- | Analytical tick states
data TickState
  = Active                         -- Normal trading activity
  | FloorCandidate                 -- Potential floor support level
  | ResistanceLevel                -- Potential resistance
  | HighActivity                   -- Hot zone
  | LowActivity                    -- Cold zone

derive instance eqTickState :: Eq TickState

instance showTickState :: Show TickState where
  show Active = "Active"
  show FloorCandidate = "FloorCandidate"
  show ResistanceLevel = "ResistanceLevel"
  show HighActivity = "HighActivity"
  show LowActivity = "LowActivity"

-- | Historical event data
type Event =
  { timestamp :: Number
  , block :: Int
  , eventType :: EventType
  , amount :: Number
  , price :: Number
  , gasUsed :: Number              -- For efficiency tracking
  , sender :: String               -- For unique trader analysis
  }

-- | Event types from on-chain logs
data EventType
  = Swap { zeroForOne :: Boolean, amountIn :: Number, amountOut :: Number }
  | Mint { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
  | Burn { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
  | Flash { amount0 :: Number, amount1 :: Number }

--------------------------------------------------------------------------------
-- Comprehensive Tick Analysis
--------------------------------------------------------------------------------

-- | Calculate comprehensive metrics from event history
calculateMetrics :: Array Event -> TickMetrics
calculateMetrics events =
  let
    volume24h = calculateVolume events 86400000.0
    volume7d = calculateVolume events 604800000.0
    liquidity = getCurrentLiquidity events
    feeRevenue24h = volume24h * 0.003 -- 0.3% fee
    
    -- Convert events to tick events for historical tracking
    tickEvents = map eventToTickEvent events
    priceHistory = map _.price events
    volumeHistory = map _.amount events
    
  in { support: calculateSupport events
     , temperature: calculateTemperature events
     , reliability: calculateReliability events
     , velocity: calculateVelocity events
     , momentum: calculateMomentum events
     , depth: calculateDepth events
     , liquidity: liquidity
     , volume24h: volume24h
     , volume7d: volume7d
     , feeRevenue24h: feeRevenue24h
     , priceHistory: priceHistory
     , volumeHistory: volumeHistory
     , eventLog: tickEvents
     }

-- | Comprehensive tick analysis
analyzeTickHistory :: Int -> Number -> Array Event -> TickAnalytics
analyzeTickHistory tickIndex price events =
  let
    metrics = calculateMetrics events
    state = determineTickState metrics
    volume24h = calculateVolume events 86400000.0
    volume7d = calculateVolume events 604800000.0
    
  in { tick: tickIndex
     , price: price
     , volume24h: volume24h
     , volume7d: volume7d
     , events: take 1000 events  -- Keep last 1000 for analysis
     , metrics: metrics
     , state: state
     }

-- | Determine analytical state from metrics
determineTickState :: TickMetrics -> TickState
determineTickState metrics =
  if metrics.temperature > 80.0
    then HighActivity
  else if metrics.temperature < 20.0
    then LowActivity
  else if metrics.support > 50.0 && metrics.reliability > 70.0
    then FloorCandidate
  else if metrics.support < -50.0 && metrics.reliability > 70.0
    then ResistanceLevel
  else Active

-- | Check if tick qualifies as floor support
isFloorCandidate :: TickAnalytics -> Boolean
isFloorCandidate analytics =
  analytics.metrics.support > 40.0 &&
  analytics.metrics.reliability > 65.0 &&
  analytics.metrics.temperature > 30.0 &&
  analytics.state == FloorCandidate

--------------------------------------------------------------------------------
-- Individual Metric Calculations
--------------------------------------------------------------------------------

-- | Calculate net support (buy vs sell pressure)
calculateSupport :: Array Event -> Number
calculateSupport events =
  let 
    swapEvents = Array.mapMaybe getSwapEvent events
    
    buyVolume = sum $ map _.amountIn $ filter _.zeroForOne swapEvents
    sellVolume = sum $ map _.amountIn $ filter (not <<< _.zeroForOne) swapEvents
    
    netSupport = buyVolume - sellVolume
    totalVolume = buyVolume + sellVolume
    
    -- Normalize to percentage
    supportRatio = if totalVolume > 0.0
      then (netSupport / totalVolume) * 100.0
      else 0.0
      
  in supportRatio

-- | Calculate activity temperature (0-100 scale)
calculateTemperature :: Array Event -> Number
calculateTemperature events =
  let
    now = 1000000.0  -- TODO: Use Effect.Now.now when needed
    
    -- Time-weighted activity scoring
    recentEvents = filter (\e -> e.timestamp > now - 3600000.0) events  -- Last hour
    mediumEvents = filter (\e -> e.timestamp > now - 86400000.0) events -- Last day
    
    -- Weight recent activity more heavily
    recentScore = Int.toNumber (length recentEvents) * 10.0
    mediumScore = Int.toNumber (length mediumEvents) * 1.0
    
    -- Volume component
    recentVolume = sum $ map _.amount recentEvents
    volumeScore = min 50.0 (recentVolume / 10000.0)  -- Cap at 50 points
    
    -- Combine scores
    rawTemp = recentScore + mediumScore + volumeScore
    
  in min 100.0 rawTemp

-- | Calculate liquidity provision reliability
calculateReliability :: Array Event -> Number
calculateReliability events =
  let
    mintEvents = Array.mapMaybe getMintEvent events
    burnEvents = Array.mapMaybe getBurnEvent events
    
    -- Consistency metrics
    totalMints = length mintEvents
    totalBurns = length burnEvents
    
    -- Liquidity stability ratio
    stabilityRatio = if totalMints > 0
      then Int.toNumber totalMints / Int.toNumber (totalMints + totalBurns)
      else 0.0
      
    -- Duration analysis
    avgLiquidityDuration = calculateAvgLiquidityDuration events
    durationScore = min 1.0 (avgLiquidityDuration / 604800000.0)  -- 1 week baseline
    
  in (stabilityRatio * 0.6 + durationScore * 0.4) * 100.0

-- | Calculate rate of liquidity change
calculateVelocity :: Array Event -> Number
calculateVelocity events =
  let
    recentEvents = take 50 events
    liquidityChanges = Array.mapMaybe getLiquidityChange recentEvents
    
    totalChange = sum $ map _.change liquidityChanges
    timeSpan = case head events, last events of
      Just first, Just last -> abs (first.timestamp - last.timestamp)
      _, _ -> 1.0
      
  in if timeSpan > 0.0
     then totalChange / timeSpan * 1000.0  -- Per second
     else 0.0

-- | Calculate directional momentum
calculateMomentum :: Array Event -> Number
calculateMomentum events =
  let
    recentSwaps = take 20 $ Array.mapMaybe getSwapEvent events
    
    -- Calculate weighted direction using proper fold
    weightedSum = foldl (\acc swap ->
      let weight = 1.0 / (1.0 + Int.toNumber (fromMaybe 20 (elemIndex swap recentSwaps)))
          direction = if swap.zeroForOne then 1.0 else -1.0
      in acc + direction * weight * swap.amountIn
    ) 0.0 recentSwaps
    
  in weightedSum / 1000.0  -- Normalize

-- | Calculate liquidity depth score
calculateDepth :: Array Event -> Number
calculateDepth events =
  let
    activeLiquidity = getCurrentLiquidity events
    avgTradeSize = calculateAvgTradeSize events
    
    -- Depth relative to typical trade size
    depthRatio = if avgTradeSize > 0.0
      then activeLiquidity / avgTradeSize
      else 0.0
      
  in min 100.0 (depthRatio * 10.0)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

getSwapEvent :: Event -> Maybe { zeroForOne :: Boolean, amountIn :: Number, amountOut :: Number }
getSwapEvent event = case event.eventType of
  Swap s -> Just s
  _ -> Nothing

getMintEvent :: Event -> Maybe { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
getMintEvent event = case event.eventType of
  Mint m -> Just m
  _ -> Nothing

getBurnEvent :: Event -> Maybe { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
getBurnEvent event = case event.eventType of
  Burn b -> Just b
  _ -> Nothing

getLiquidityChange :: Event -> Maybe { change :: Number, timestamp :: Number }
getLiquidityChange event = case event.eventType of
  Mint m -> Just { change: m.liquidity, timestamp: event.timestamp }
  Burn b -> Just { change: -b.liquidity, timestamp: event.timestamp }
  _ -> Nothing

calculateVolume :: Array Event -> Number -> Number
calculateVolume events timeWindow =
  let
    now = 1000000.0  -- TODO: Use Effect.Now.now when needed
    recentEvents = filter (\e -> e.timestamp > now - timeWindow) events
    swaps = Array.mapMaybe getSwapEvent recentEvents
  in sum $ map _.amountIn swaps

getCurrentLiquidity :: Array Event -> Number
getCurrentLiquidity events =
  let
    changes = Array.mapMaybe getLiquidityChange events
  in sum $ map _.change changes

calculateAvgTradeSize :: Array Event -> Number
calculateAvgTradeSize events =
  let
    swaps = Array.mapMaybe getSwapEvent events
    totalVolume = sum $ map _.amountIn swaps
    numSwaps = length swaps
  in if numSwaps > 0
     then totalVolume / Int.toNumber numSwaps
     else 0.0

calculateAvgLiquidityDuration :: Array Event -> Number
calculateAvgLiquidityDuration _ = 
  432000000.0  -- 5 days placeholder

-- | Convert generic event to tick-specific event
eventToTickEvent :: Event -> TickEvent
eventToTickEvent event =
  { timestamp: event.timestamp
  , eventType: case event.eventType of
      Swap _ -> "swap"
      Mint _ -> "mint"
      Burn _ -> "burn"  
      Flash _ -> "flash"
  , volume: event.amount
  , liquidity: getCurrentLiquidityFromEvent event
  , priceImpact: calculatePriceImpact event
  }
  where
    getCurrentLiquidityFromEvent _ = 0.0  -- Simplified
    calculatePriceImpact _ = 0.0  -- Simplified
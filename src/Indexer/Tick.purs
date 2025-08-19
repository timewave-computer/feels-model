-- | Tick-Level Analytics and Metrics Engine
-- |
-- | This indexer module provides comprehensive analytics for individual price ticks
-- | within AMM pools. It processes on-chain events to calculate sophisticated metrics
-- | including support/resistance levels, trading activity, and liquidity patterns.
-- |
-- | Key Features:
-- | - Real-time calculation of tick-specific trading metrics
-- | - Support and resistance level identification
-- | - Activity temperature scoring for hot/cold zone detection
-- | - Liquidity provision reliability analysis
-- | - Momentum and velocity calculations for trend analysis
-- | - Historical event tracking and pattern recognition
-- |
-- | Metrics Pipeline:
-- | 1. Process raw on-chain events (swaps, mints, burns)
-- | 2. Calculate core metrics (support, temperature, reliability)
-- | 3. Determine tick analytical state (active, floor, resistance)
-- | 4. Track historical patterns and trends
-- | 5. Generate tick-specific insights for liquidity optimization
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
-- TICK ANALYTICS DATA STRUCTURES
--------------------------------------------------------------------------------
-- Comprehensive data types for tick-level analysis and metrics tracking

-- | Complete tick analytics combining real-time data with historical insights
-- | Maintains comprehensive view of individual tick performance and characteristics
type TickAnalytics =
  { tick :: Int                    -- Price tick index (log base 1.0001)
  , price :: Number                -- Exact price at this tick level
  , volume24h :: Number            -- Rolling 24-hour volume through tick
  , volume7d :: Number             -- Rolling 7-day volume through tick
  , events :: Array Event          -- Historical activity event log
  , metrics :: TickMetrics         -- Calculated performance metrics
  , state :: TickState             -- Current analytical classification
  }

-- | Comprehensive metrics derived from tick event history and activity patterns
-- | Provides multi-dimensional analysis of tick performance and characteristics
type TickMetrics =
  { -- Tick identification
    tick :: Int                    -- The tick index this metrics data represents
  
  -- Core trading metrics
  , support :: Number              -- Net buy/sell pressure (normalized percentage)
  , temperature :: Number          -- Activity intensity (0=cold, 100=hot)
  , reliability :: Number          -- Consistency of liquidity provision
  , velocity :: Number             -- Rate of liquidity change over time
  , momentum :: Number             -- Directional trading bias
  , depth :: Number                -- Available liquidity depth score
  
  -- Volume and revenue metrics
  , liquidity :: Number            -- Current active liquidity at tick
  , volume24h :: Number            -- 24-hour trading volume
  , volume7d :: Number             -- 7-day trading volume
  , feeRevenue24h :: Number        -- Estimated fee revenue generated
  
  -- Historical tracking data
  , priceHistory :: Array Number   -- Historical price movements
  , volumeHistory :: Array Number  -- Historical volume patterns
  , eventLog :: Array TickEvent    -- Processed event history
  }

-- | Structured event data optimized for tick-level analysis
-- | Processed from raw on-chain events with tick-specific calculations
type TickEvent =
  { timestamp :: Number            -- Event timestamp
  , eventType :: String            -- Event classification
  , volume :: Number               -- Volume involved in event
  , liquidity :: Number            -- Liquidity impact
  , priceImpact :: Number          -- Calculated price impact
  }

--------------------------------------------------------------------------------
-- TICK STATE CLASSIFICATION SYSTEM
--------------------------------------------------------------------------------
-- Analytical states for categorizing tick behavior and characteristics

-- | Tick classification based on trading patterns and liquidity characteristics
-- | Used for identifying optimal liquidity deployment zones and trading patterns
data TickState
  = Active                         -- Normal trading activity with regular volume
  | FloorCandidate                 -- Strong support level with consistent buying
  | ResistanceLevel                -- Strong resistance with consistent selling
  | HighActivity                   -- Hot zone with elevated trading activity
  | LowActivity                    -- Cold zone with minimal trading activity

derive instance eqTickState :: Eq TickState

instance showTickState :: Show TickState where
  show Active = "Active"
  show FloorCandidate = "FloorCandidate"
  show ResistanceLevel = "ResistanceLevel"
  show HighActivity = "HighActivity"
  show LowActivity = "LowActivity"

--------------------------------------------------------------------------------
-- ON-CHAIN EVENT DATA STRUCTURES
--------------------------------------------------------------------------------
-- Raw event data structures for processing blockchain activity

-- | Raw on-chain event data with metadata for comprehensive analysis
-- | Contains all information needed for tick-level metric calculations
type Event =
  { timestamp :: Number            -- Block timestamp
  , block :: Int                   -- Block number for ordering
  , eventType :: EventType         -- Specific event type and parameters
  , amount :: Number               -- Primary amount involved
  , price :: Number                -- Price at time of event
  , gasUsed :: Number              -- Gas consumption for efficiency analysis
  , sender :: String               -- Sender address for unique trader counting
  }

-- | Categorized on-chain event types with specific parameters
-- | Each type captures the essential data needed for metric calculations
data EventType
  = Swap                           -- Token swap transaction
    { zeroForOne :: Boolean        -- Swap direction
    , amountIn :: Number           -- Input amount
    , amountOut :: Number          -- Output amount received
    }
  | Mint                           -- Liquidity position creation
    { tickLower :: Int             -- Position lower tick bound
    , tickUpper :: Int             -- Position upper tick bound
    , liquidity :: Number          -- Liquidity amount added
    }
  | Burn                           -- Liquidity position removal
    { tickLower :: Int             -- Position lower tick bound
    , tickUpper :: Int             -- Position upper tick bound
    , liquidity :: Number          -- Liquidity amount removed
    }
  | Flash                          -- Flash loan transaction
    { amount0 :: Number            -- Token0 flash loan amount
    , amount1 :: Number            -- Token1 flash loan amount
    }

--------------------------------------------------------------------------------
-- COMPREHENSIVE TICK METRICS CALCULATION
--------------------------------------------------------------------------------
-- Core engine for calculating all tick-level performance metrics

-- | Calculate comprehensive tick metrics from historical event data
-- | Processes all available events to generate complete metric profile
calculateMetrics :: Int -> Array Event -> TickMetrics
calculateMetrics tickIndex events =
  let
    -- Calculate volume metrics over different time windows
    volume24h = calculateVolume events 86400000.0   -- 24 hours in milliseconds
    volume7d = calculateVolume events 604800000.0    -- 7 days in milliseconds
    liquidity = getCurrentLiquidity events
    feeRevenue24h = volume24h * 0.003                -- 0.3% standard AMM fee
    
    -- Process events for historical tracking
    tickEvents = map eventToTickEvent events
    priceHistory = map _.price events
    volumeHistory = map _.amount events
    
  in { tick: tickIndex  -- Add the tick index
     , support: calculateSupport events
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

-- | Generate complete tick analytics from historical data
-- | Combines all metrics, classifications, and insights into unified analytics
analyzeTickHistory :: Number -> Int -> Number -> Array Event -> TickAnalytics
analyzeTickHistory now tickIndex price events =
  let
    -- Calculate comprehensive metrics
    metrics = calculateMetrics tickIndex events
    state = determineTickState metrics
    volume24h = calculateVolume events 86400000.0
    volume7d = calculateVolume events 604800000.0
    
  in { tick: tickIndex
     , price: price
     , volume24h: volume24h
     , volume7d: volume7d
     , events: take 1000 events  -- Maintain last 1000 events for analysis
     , metrics: metrics
     , state: state
     }

-- | Determine tick analytical state based on calculated metrics
-- | Uses multi-criteria decision making to classify tick behavior
determineTickState :: TickMetrics -> TickState
determineTickState metrics =
  if metrics.temperature > 80.0
    then HighActivity                    -- Hot zone with high trading activity
  else if metrics.temperature < 20.0
    then LowActivity                     -- Cold zone with minimal activity
  else if metrics.support > 50.0 && metrics.reliability > 70.0
    then FloorCandidate                  -- Strong support with reliable liquidity
  else if metrics.support < -50.0 && metrics.reliability > 70.0
    then ResistanceLevel                 -- Strong resistance with reliable selling
  else Active                            -- Normal trading activity

-- | Validate if tick meets criteria for floor support classification
-- | Requires strong support, reliability, activity, and proper state
isFloorCandidate :: TickAnalytics -> Boolean
isFloorCandidate analytics =
  analytics.metrics.support > 40.0 &&
  analytics.metrics.reliability > 65.0 &&
  analytics.metrics.temperature > 30.0 &&
  analytics.state == FloorCandidate

--------------------------------------------------------------------------------
-- INDIVIDUAL METRIC CALCULATION ALGORITHMS
--------------------------------------------------------------------------------
-- Specialized functions for calculating specific tick performance metrics

-- | Calculate net support level from buy vs sell pressure analysis
-- | Returns normalized percentage indicating directional bias
calculateSupport :: Array Event -> Number
calculateSupport events =
  let 
    swapEvents = Array.mapMaybe getSwapEvent events
    
    -- Separate buy and sell volumes based on swap direction
    buyVolume = sum $ map _.amountIn $ filter _.zeroForOne swapEvents
    sellVolume = sum $ map _.amountIn $ filter (not <<< _.zeroForOne) swapEvents
    
    -- Calculate net support and total activity
    netSupport = buyVolume - sellVolume
    totalVolume = buyVolume + sellVolume
    
    -- Normalize to percentage (-100% to +100%)
    supportRatio = if totalVolume > 0.0
      then (netSupport / totalVolume) * 100.0
      else 0.0
      
  in supportRatio

-- | Calculate activity temperature using time-weighted scoring (0-100 scale)
-- | Higher values indicate hotter trading zones with more recent activity
calculateTemperature :: Array Event -> Number
calculateTemperature events =
  let
    -- Time-weighted activity analysis with decay functions
    recentEvents = filter (\e -> e.timestamp > now - 3600000.0) events  -- Last hour
    mediumEvents = filter (\e -> e.timestamp > now - 86400000.0) events -- Last day
    
    -- Apply higher weights to more recent activity
    recentScore = Int.toNumber (length recentEvents) * 10.0
    mediumScore = Int.toNumber (length mediumEvents) * 1.0
    
    -- Include volume-based component for activity intensity
    recentVolume = sum $ map _.amount recentEvents
    volumeScore = min 50.0 (recentVolume / 10000.0)  -- Capped at 50 points
    
    -- Combine all scoring components
    rawTemp = recentScore + mediumScore + volumeScore
    
  in min 100.0 rawTemp  -- Cap maximum temperature at 100
  where
    now = 0.0  -- Would be passed as parameter in production

-- | Calculate reliability of liquidity provision at this tick
-- | Measures consistency and duration of liquidity commitments
calculateReliability :: Array Event -> Number
calculateReliability events =
  let
    mintEvents = Array.mapMaybe getMintEvent events
    burnEvents = Array.mapMaybe getBurnEvent events
    
    -- Basic consistency metrics
    totalMints = length mintEvents
    totalBurns = length burnEvents
    
    -- Calculate liquidity stability ratio
    stabilityRatio = if totalMints > 0
      then Int.toNumber totalMints / Int.toNumber (totalMints + totalBurns)
      else 0.0
      
    -- Analyze average duration of liquidity provision
    avgLiquidityDuration = calculateAvgLiquidityDuration events
    durationScore = min 1.0 (avgLiquidityDuration / 604800000.0)  -- 1 week baseline
    
    -- Weighted composite reliability score
  in (stabilityRatio * 0.6 + durationScore * 0.4) * 100.0

-- | Calculate velocity of liquidity changes over time
-- | Measures the rate at which liquidity is being added or removed
calculateVelocity :: Array Event -> Number
calculateVelocity events =
  let
    recentEvents = take 50 events
    liquidityChanges = Array.mapMaybe getLiquidityChange recentEvents
    
    -- Sum total liquidity changes
    totalChange = sum $ map _.change liquidityChanges
    
    -- Calculate time span of analysis window
    timeSpan = case head events, last events of
      Just first, Just last -> abs (first.timestamp - last.timestamp)
      _, _ -> 1.0
      
    -- Return rate of change (per second)
  in if timeSpan > 0.0
     then totalChange / timeSpan * 1000.0  -- Convert to per-second rate
     else 0.0

-- | Calculate directional momentum from recent trading activity
-- | Weights recent swaps more heavily to identify trending behavior
calculateMomentum :: Array Event -> Number
calculateMomentum events =
  let
    recentSwaps = take 20 $ Array.mapMaybe getSwapEvent events
    
    -- Calculate time-weighted directional bias
    weightedSum = foldl (\acc swap ->
      let weight = 1.0 / (1.0 + Int.toNumber (fromMaybe 20 (elemIndex swap recentSwaps)))
          direction = if swap.zeroForOne then 1.0 else -1.0
          volumeWeight = swap.amountIn
      in acc + direction * weight * volumeWeight
    ) 0.0 recentSwaps
    
  in weightedSum / 1000.0  -- Normalize to reasonable scale

-- | Calculate liquidity depth score relative to trading activity
-- | Measures how much liquidity is available relative to typical trade sizes
calculateDepth :: Array Event -> Number
calculateDepth events =
  let
    activeLiquidity = getCurrentLiquidity events
    avgTradeSize = calculateAvgTradeSize events
    
    -- Calculate depth relative to typical trade sizes
    depthRatio = if avgTradeSize > 0.0
      then activeLiquidity / avgTradeSize
      else 0.0
      
    -- Scale and cap depth score
  in min 100.0 (depthRatio * 10.0)

--------------------------------------------------------------------------------
-- EVENT PROCESSING AND ANALYSIS UTILITIES
--------------------------------------------------------------------------------
-- Helper functions for extracting and analyzing specific event types

-- | Extract swap event data from generic event structure
getSwapEvent :: Event -> Maybe { zeroForOne :: Boolean, amountIn :: Number, amountOut :: Number }
getSwapEvent event = case event.eventType of
  Swap s -> Just s
  _ -> Nothing

-- | Extract mint event data for liquidity addition analysis
getMintEvent :: Event -> Maybe { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
getMintEvent event = case event.eventType of
  Mint m -> Just m
  _ -> Nothing

-- | Extract burn event data for liquidity removal analysis
getBurnEvent :: Event -> Maybe { tickLower :: Int, tickUpper :: Int, liquidity :: Number }
getBurnEvent event = case event.eventType of
  Burn b -> Just b
  _ -> Nothing

-- | Extract liquidity change data for velocity calculations
getLiquidityChange :: Event -> Maybe { change :: Number, timestamp :: Number }
getLiquidityChange event = case event.eventType of
  Mint m -> Just { change: m.liquidity, timestamp: event.timestamp }
  Burn b -> Just { change: -b.liquidity, timestamp: event.timestamp }
  _ -> Nothing

--------------------------------------------------------------------------------
-- VOLUME AND LIQUIDITY CALCULATION UTILITIES
--------------------------------------------------------------------------------
-- Mathematical functions for computing tick-level volume and liquidity metrics

-- | Calculate total volume within specified time window
calculateVolume :: Array Event -> Number -> Number
calculateVolume events timeWindow =
  let
    -- Filter events within time window (simplified - would use current time)
    recentEvents = filter (\e -> e.timestamp > 0.0 - timeWindow) events
    swaps = Array.mapMaybe getSwapEvent recentEvents
  in sum $ map _.amountIn swaps

-- | Calculate current net liquidity from all historical changes
getCurrentLiquidity :: Array Event -> Number
getCurrentLiquidity events =
  let
    changes = Array.mapMaybe getLiquidityChange events
  in sum $ map _.change changes

-- | Calculate average trade size from all swap events
calculateAvgTradeSize :: Array Event -> Number
calculateAvgTradeSize events =
  let
    swaps = Array.mapMaybe getSwapEvent events
    totalVolume = sum $ map _.amountIn swaps
    numSwaps = length swaps
  in if numSwaps > 0
     then totalVolume / Int.toNumber numSwaps
     else 0.0

-- | Calculate average duration of liquidity provision
-- | Placeholder implementation - would analyze mint/burn patterns
calculateAvgLiquidityDuration :: Array Event -> Number
calculateAvgLiquidityDuration _ = 
  432000000.0  -- 5 days placeholder for demonstration

-- | Convert generic blockchain event to tick-specific event format
-- | Processes raw events for tick-level analysis and tracking
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
    getCurrentLiquidityFromEvent _ = 0.0  -- Simplified - would calculate actual impact
    calculatePriceImpact _ = 0.0  -- Simplified - would calculate actual price impact
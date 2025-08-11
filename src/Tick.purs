-- | Tick Module - Living ticks with local state and metrics
-- |
-- | This module implements ticks as living entities that maintain their own state
-- | and metrics, enabling emergent floor discovery and dynamic liquidity management.
module Tick
  ( Tick
  , TickMetrics
  , TickState(..)
  , Event
  , EventType(..)
  , RingBuffer
  , createTick
  , updateTick
  , calculateMetrics
  , executeTrade
  , isFloorCandidate
  ) where

import Prelude
import Data.Array ((:), take, drop, filter, length)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Functor (map)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Ord (min, max)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Living tick with local state
type Tick =
  { price :: Number
  , liquidity :: Number            -- Total liquidity at this price
  , volume24h :: Number            -- Rolling 24h volume
  , events :: RingBuffer Event     -- Recent activity (last 100 events)
  , metrics :: TickMetrics         -- Derived local metrics
  , state :: TickState            -- Current tick state
  }

-- | Metrics calculated from tick history
type TickMetrics =
  { support :: Number              -- Buy volume - sell volume
  , temperature :: Number          -- Activity level (0 = cold, 1 = hot)
  , reliability :: Number          -- Consistency of liquidity provision
  , velocity :: Number             -- Rate of liquidity change
  }

-- | Tick state machine
data TickState
  = Active                         -- Normal trading
  | FloorLocked                    -- Locked as floor support
  | FloorBoundary                  -- Potential floor level

derive instance eqTickState :: Eq TickState

instance showTickState :: Show TickState where
  show Active = "Active"
  show FloorLocked = "FloorLocked"
  show FloorBoundary = "FloorBoundary"

-- | Ring buffer for recent events (fixed size)
type RingBuffer a = Array a

-- | Market event at this tick
type Event =
  { timestamp :: Number
  , eventType :: EventType
  , amount :: Number
  , impact :: Number               -- Price impact of this event
  }

-- | Types of events
data EventType
  = Buy
  | Sell
  | AddLiquidity
  | RemoveLiquidity

derive instance eqEventType :: Eq EventType

instance showEventType :: Show EventType where
  show Buy = "Buy"
  show Sell = "Sell"
  show AddLiquidity = "AddLiquidity"
  show RemoveLiquidity = "RemoveLiquidity"

--------------------------------------------------------------------------------
-- Tick Creation
--------------------------------------------------------------------------------

-- | Create a new tick at a price level
createTick :: Number -> Number -> Tick
createTick price initialLiquidity =
  { price
  , liquidity: initialLiquidity
  , volume24h: 0.0
  , events: []
  , metrics: 
    { support: 0.0
    , temperature: 0.0
    , reliability: 1.0
    , velocity: 0.0
    }
  , state: Active
  }

--------------------------------------------------------------------------------
-- Tick Updates
--------------------------------------------------------------------------------

-- | Update tick with new event
updateTick :: Event -> Tick -> Tick
updateTick event tick =
  let -- Add event to ring buffer (keep last 100)
      newEvents = take 100 (event : tick.events)
      
      -- Update liquidity based on event
      newLiquidity = case event.eventType of
        AddLiquidity -> tick.liquidity + event.amount
        RemoveLiquidity -> max 0.0 (tick.liquidity - event.amount)
        _ -> tick.liquidity
      
      -- Recalculate metrics
      newMetrics = calculateMetrics newEvents
      
      -- Update state based on metrics
      newState = determineState newMetrics tick.price
      
  in tick
    { events = newEvents
    , liquidity = newLiquidity
    , metrics = newMetrics
    , state = newState
    }

-- | Execute a trade at this tick
executeTrade :: EventType -> Number -> Number -> Tick -> Tick
executeTrade tradeType amount currentTime tick =
  let impact = calculateImpact amount tick.liquidity
      event = 
        { timestamp: currentTime
        , eventType: tradeType
        , amount: amount
        , impact: impact
        }
  in updateTick event tick

--------------------------------------------------------------------------------
-- Metrics Calculation
--------------------------------------------------------------------------------

-- | Calculate metrics from event history
calculateMetrics :: RingBuffer Event -> TickMetrics
calculateMetrics events =
  let -- Calculate support (buy volume - sell volume)
      support = calculateSupport events
      
      -- Calculate temperature (activity level)
      temperature = calculateTemperature events
      
      -- Calculate reliability (consistency)
      reliability = calculateReliability events
      
      -- Calculate velocity (rate of change)
      velocity = calculateVelocity events
      
  in { support, temperature, reliability, velocity }

-- | Calculate net support at this tick
calculateSupport :: RingBuffer Event -> Number
calculateSupport events =
  let sumByType eventType = 
        events
          # Array.filter (\e -> e.eventType == eventType)
          # map _.amount
          # sum
      
      buyVolume = sumByType Buy
      sellVolume = sumByType Sell
      
  in buyVolume - sellVolume

-- | Calculate activity temperature (0 = cold, 1 = hot)
calculateTemperature :: RingBuffer Event -> Number
calculateTemperature events =
  case events of
    [] -> 0.0
    _ ->
      let recentCount = Array.length $ Array.filter (\_e -> true) $ take 10 events
          maxActivity = 10.0
      in min 1.0 (Int.toNumber recentCount / maxActivity)

-- | Calculate liquidity provision reliability
calculateReliability :: RingBuffer Event -> Number
calculateReliability events =
  let liquidityEvents = Array.filter (\e -> 
        e.eventType == AddLiquidity || e.eventType == RemoveLiquidity
      ) events
      
      -- Look for consistent liquidity provision
      additions = Array.filter (\e -> e.eventType == AddLiquidity) liquidityEvents
      removals = Array.filter (\e -> e.eventType == RemoveLiquidity) liquidityEvents
      
      ratio = if Array.length additions > 0
              then Int.toNumber (Array.length additions) / Int.toNumber (Array.length additions + Array.length removals)
              else 0.5
              
  in ratio

-- | Calculate rate of liquidity change
calculateVelocity :: RingBuffer Event -> Number
calculateVelocity events =
  case events of
    [] -> 0.0
    _ -> 
      let liquidityChanges = events
            # filter (\e -> e.eventType == AddLiquidity || e.eventType == RemoveLiquidity)
            # take 5
            # map (\e -> if e.eventType == AddLiquidity then e.amount else -e.amount)
            # sum
          
          -- Normalize by time window (simplified)
          timeWindow = 3600000.0  -- 1 hour in ms
          
      in liquidityChanges / timeWindow

--------------------------------------------------------------------------------
-- State Determination
--------------------------------------------------------------------------------

-- | Determine tick state based on metrics
determineState :: TickMetrics -> Number -> TickState
determineState metrics _price =
  if metrics.support > 1000.0 && metrics.reliability > 0.8
  then FloorBoundary
  else if metrics.temperature > 0.8
  then Active
  else Active

-- | Check if tick is a floor candidate
isFloorCandidate :: Tick -> Boolean
isFloorCandidate tick =
  tick.metrics.support > 500.0 &&
  tick.metrics.reliability > 0.7 &&
  tick.state == FloorBoundary

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Calculate price impact of a trade
calculateImpact :: Number -> Number -> Number
calculateImpact tradeAmount liquidity =
  if liquidity > 0.0
  then min 1.0 (tradeAmount / liquidity * 0.1)  -- Simplified impact model
  else 1.0
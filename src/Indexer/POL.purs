-- | Protocol-Owned Liquidity Analytics and Optimization Engine
-- |
-- | This indexer module provides comprehensive analysis and optimization for
-- | Protocol-Owned Liquidity (POL) deployment across AMM pools. It processes
-- | on-chain events, analyzes market conditions, and generates actionable
-- | recommendations for automated POL management.
-- |
-- | Key Features:
-- | - Real-time POL performance analysis and scoring
-- | - Automated keeper action generation for POL deployment and rebalancing
-- | - Market condition analysis for optimal POL range selection
-- | - Risk assessment and emergency support detection
-- | - Hot zone identification for concentrated liquidity optimization
-- |
-- | Analysis Pipeline:
-- | 1. Process pool events to extract POL-relevant metrics
-- | 2. Analyze tick-level activity patterns and hot zones
-- | 3. Score potential POL deployment ranges
-- | 4. Generate keeper actions based on market conditions
-- | 5. Continuously update POL efficiency metrics
module Indexer.POL
  ( -- POL Types
    KeeperAction(..)
  -- POL Analysis Functions
  , findOptimalPOLRange
  , scorePOLRange
  , generatePOLRanges
  , generateKeeperActions
  -- POL Decision Functions
  , shouldDeployPOL
  , shouldRebalancePOL
  , needsEmergencySupport
  , calculateUrgency
  , calculateEmergencyAmount
  -- POL Scoring Functions
  , calculateVolumeCoverage
  , calculateSupportAlignment
  , estimateFeeGeneration
  , calculateRiskFactor
  -- Event Processing
  , processPOLEvent
  -- Helper Functions
  , getCurrentPOLRange
  -- selectBetter removed - using maxBy from Data.Ord
  ) where

import Prelude
import Data.Array ((:), take, filter, foldl, length, partition, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (sum, maximum, minimum)
import Data.Int (toNumber, floor)
import Data.Ord (max, min, abs, comparing)
import Data.Function (on)
import FFI (log)
import Effect (Effect)

-- Import analytics from other indexer modules
import Indexer.Tick (TickMetrics)
import Protocol.Pool (PoolEvent(..))
import Protocol.POL (POLTriggerType(..))

--------------------------------------------------------------------------------
-- KEEPER ACTION TYPES
--------------------------------------------------------------------------------
-- Actionable recommendations for automated POL management systems

-- | Comprehensive set of actions that automated keepers can execute
-- | Each action includes all necessary parameters and urgency metrics
data KeeperAction
  = DeployPOL                       -- Deploy new POL to a specific range
    { amount :: Number              -- Amount of POL to deploy
    , range :: { lower :: Int, upper :: Int }  -- Target tick range
    , urgency :: Number             -- Urgency score (0-1)
    }
  | RebalancePOL                    -- Rebalance existing POL position
    { currentRange :: { lower :: Int, upper :: Int }  -- Current POL range
    , newRange :: { lower :: Int, upper :: Int }      -- Optimal new range
    , reason :: String              -- Explanation for rebalancing
    }
  | HarvestFees                     -- Harvest accumulated fees
    { amount :: Number              -- Expected fee amount
    , pool :: String                -- Pool identifier
    }
  | EmergencyLiquidity              -- Deploy emergency liquidity support
    { pool :: String                -- Pool requiring support
    , amount :: Number              -- Emergency support amount
    }

--------------------------------------------------------------------------------
-- POOL ANALYTICS DATA STRUCTURES
--------------------------------------------------------------------------------
-- Comprehensive pool metrics for POL decision making

-- | Aggregated pool analytics optimized for POL analysis
-- | Contains all metrics needed for intelligent POL deployment decisions
type PoolAnalytics =
  { poolId :: String                -- Unique pool identifier
  , volume24h :: Number             -- 24-hour trading volume
  , totalValueLocked :: Number      -- Current TVL in the pool
  , polEfficiency :: Number         -- POL performance score (0-100)
  , optimalPolRange :: { lower :: Int, upper :: Int }  -- Recommended POL range
  , recommendedPolAmount :: Number  -- Optimal POL deployment amount
  , historicalVolatility :: Number  -- Price volatility metric for risk assessment
  , activeTicks :: Array TickMetrics -- Detailed tick-level activity data
  }

-- | High-activity zones within pools where concentrated trading occurs
-- | Used to guide optimal POL placement for maximum fee capture
type HotZone =
  { tickLower :: Int                -- Lower boundary of hot zone
  , tickUpper :: Int                -- Upper boundary of hot zone
  , temperature :: Number           -- Activity intensity score (0-100)
  , volumeConcentration :: Number   -- Percentage of total volume in this zone
  }

--------------------------------------------------------------------------------
-- POL RANGE OPTIMIZATION ENGINE
--------------------------------------------------------------------------------
-- Core algorithms for finding optimal POL deployment ranges

-- | Find the optimal POL deployment range using comprehensive tick analysis
-- | Evaluates all active ticks to determine the most profitable POL placement
findOptimalPOLRange :: PoolAnalytics -> { lower :: Int, upper :: Int, score :: Number }
findOptimalPOLRange analytics =
  let
    -- Identify high-activity zones and support levels
    hotZones = findHotZones analytics.activeTicks
    supportZones = findSupportZones analytics.activeTicks
    
    -- Generate multiple candidate ranges for evaluation
    candidateRanges = generatePOLRanges analytics.volume24h -- Volume as price proxy
    scoredRanges = map (scorePOLRange analytics hotZones supportZones) candidateRanges
    
    -- Select highest-scoring range using multi-criteria optimization
    bestRange = foldl (\acc curr -> if curr.score > acc.score then curr else acc) 
                      { lower: 0, upper: 0, score: 0.0 } 
                      scoredRanges
    
  in bestRange

-- | Score POL deployment range using weighted multi-factor analysis
-- | Combines volume capture, support alignment, fee potential, and risk factors
scorePOLRange :: PoolAnalytics -> Array HotZone -> Array Int -> _ -> _
scorePOLRange analytics hotZones supportZones range =
  let
    -- Volume capture: How much trading activity will POL capture
    volumeScore = calculateVolumeCoverage range analytics.activeTicks
    
    -- Support alignment: How well range aligns with price support levels
    supportScore = calculateSupportAlignment range supportZones
    
    -- Fee generation: Expected fee income from POL deployment
    feeScore = estimateFeeGeneration range analytics
    
    -- Risk adjustment: Volatility-based risk factor
    riskFactor = calculateRiskFactor range analytics.historicalVolatility
    
    -- Weighted composite score (40% volume, 30% support, 30% fees)
    totalScore = (volumeScore * 0.4 + supportScore * 0.3 + feeScore * 0.3) * riskFactor
    
  in { lower: range.lower, upper: range.upper, score: totalScore }

-- | Generate comprehensive keeper action recommendations
-- | Analyzes pool conditions to suggest optimal POL management actions
generateKeeperActions :: PoolAnalytics -> Array KeeperAction
generateKeeperActions analytics =
  let
    actions = []
    
    -- Evaluate POL deployment opportunity
    polAction = if shouldDeployPOL analytics
      then DeployPOL 
        { amount: analytics.recommendedPolAmount
        , range: analytics.optimalPolRange
        , urgency: calculateUrgency analytics
        } : actions
      else actions
      
    -- Evaluate POL rebalancing need
    rebalanceAction = if shouldRebalancePOL analytics
      then RebalancePOL
        { currentRange: getCurrentPOLRange analytics
        , newRange: analytics.optimalPolRange
        , reason: "Pool efficiency metrics indicate rebalancing needed"
        } : actions
      else actions
      
    -- Evaluate emergency support requirement
    emergencyAction = if needsEmergencySupport analytics
      then EmergencyLiquidity
        { pool: analytics.poolId
        , amount: calculateEmergencyAmount analytics
        } : actions
      else actions
      
  in polAction <> rebalanceAction <> emergencyAction

--------------------------------------------------------------------------------
-- POL DEPLOYMENT DECISION ENGINE
--------------------------------------------------------------------------------
-- Algorithmic decision making for automated POL management

-- | Evaluate whether new POL should be deployed to a pool
-- | Based on volume thresholds and current efficiency metrics
shouldDeployPOL :: PoolAnalytics -> Boolean
shouldDeployPOL analytics = 
  analytics.volume24h > 100000.0 && analytics.polEfficiency < 50.0

-- | Determine if existing POL positions should be rebalanced
-- | Triggers when efficiency falls below acceptable thresholds
shouldRebalancePOL :: PoolAnalytics -> Boolean  
shouldRebalancePOL analytics = 
  analytics.polEfficiency < 30.0

-- | Detect pools requiring emergency liquidity support
-- | Identifies severely undercapitalized pools relative to trading activity
needsEmergencySupport :: PoolAnalytics -> Boolean
needsEmergencySupport analytics = 
  analytics.totalValueLocked < analytics.volume24h * 0.1 -- TVL-to-volume ratio too low

-- | Calculate urgency score for POL deployment actions
-- | Higher scores indicate time-sensitive deployment opportunities
calculateUrgency :: PoolAnalytics -> Number
calculateUrgency analytics = 
  if analytics.volume24h > 1000000.0 then 0.9 else 0.5

-- | Calculate appropriate emergency support amount
-- | Based on percentage of daily trading volume
calculateEmergencyAmount :: PoolAnalytics -> Number
calculateEmergencyAmount analytics = 
  analytics.volume24h * 0.05 -- 5% of daily volume for liquidity support

--------------------------------------------------------------------------------
-- POL PERFORMANCE SCORING ALGORITHMS
--------------------------------------------------------------------------------
-- Mathematical models for evaluating POL deployment effectiveness

-- | Calculate percentage of trading volume that POL range will capture
-- | Higher coverage indicates better fee generation potential
calculateVolumeCoverage :: _ -> Array TickMetrics -> Number
calculateVolumeCoverage range ticks = 
  -- TODO: TickMetrics doesn't have tick field
  let relevantTicks = ticks -- filter (\t -> t.tick >= range.lower && t.tick <= range.upper) ticks
      rangeVolume = sum $ map _.volume24h relevantTicks
      totalVolume = sum $ map _.volume24h ticks
  in if totalVolume > 0.0 then rangeVolume / totalVolume else 0.0

-- | Measure alignment between POL range and identified support levels
-- | Better alignment indicates more stable POL positioning
calculateSupportAlignment :: _ -> Array Int -> Number
calculateSupportAlignment range supportZones = 
  let zonesInRange = filter (\tick -> tick >= range.lower && tick <= range.upper) supportZones
  in toNumber (length zonesInRange) / max 1.0 (toNumber (length supportZones))

-- | Estimate potential fee generation from POL deployment
-- | Based on volume capture and standard AMM fee structure
estimateFeeGeneration :: _ -> PoolAnalytics -> Number
estimateFeeGeneration range analytics = 
  analytics.volume24h * 0.003 * 0.5 -- 0.3% fee rate with 50% capture efficiency

-- | Calculate risk adjustment factor based on pool volatility
-- | Higher volatility reduces effective POL performance
calculateRiskFactor :: _ -> Number -> Number
calculateRiskFactor range volatility = 
  max 0.1 (1.0 - volatility / 100.0) -- Inverse relationship with volatility

--------------------------------------------------------------------------------
-- POL EVENT PROCESSING ENGINE
--------------------------------------------------------------------------------
-- Real-time processing of POL-related on-chain events

-- | Process POL deployment events and update analytics
-- | Maintains rolling efficiency metrics based on deployment outcomes
processPOLEvent :: PoolEvent -> PoolAnalytics -> Effect PoolAnalytics
processPOLEvent event state = case event of
  POLDeployed { pool, amount, tickLower, tickUpper, triggerType, slot } -> do
    -- Analyze deployment characteristics for efficiency calculation
    let rangeWidth = toNumber (tickUpper - tickLower)
        -- Narrower ranges capture fees more efficiently
        rangeEfficiency = 1.0 / (1.0 + rangeWidth / 100.0)
        
        -- Trigger-type-specific efficiency baselines
        triggerEfficiency = case triggerType of
          VolumeTrigger -> 0.9           -- High-volume deployments are most efficient
          PriceDeviationTrigger -> 0.8   -- Price stability support deployments
          LiquidityDepthTrigger -> 0.7   -- Liquidity depth support deployments
          _ -> 0.5                        -- Other deployments less optimal
        
        -- Calculate composite efficiency score
        newEfficiency = rangeEfficiency * triggerEfficiency
        
        -- Update exponential moving average of POL efficiency
        updatedEfficiency = (state.polEfficiency * 0.9) + (newEfficiency * 0.1)
        
    pure $ state { polEfficiency = updatedEfficiency }
    
  _ -> pure state  -- Non-POL events don't affect metrics

--------------------------------------------------------------------------------
-- POL RANGE GENERATION AND ANALYSIS UTILITIES
--------------------------------------------------------------------------------
-- Supporting functions for POL optimization and hot zone identification

-- | Generate multiple candidate POL ranges for evaluation
-- | Creates ranges of varying widths around current price for optimal selection
generatePOLRanges :: Number -> Array { lower :: Int, upper :: Int }
generatePOLRanges volume24h =
  let currentPrice = volume24h / 100000.0 -- Volume-based price approximation
  in
    -- Generate ranges with different risk/reward profiles
    [ { lower: priceToTick (currentPrice * 0.95), upper: priceToTick (currentPrice * 1.05) }  -- Tight range (±5%)
    , { lower: priceToTick (currentPrice * 0.90), upper: priceToTick (currentPrice * 1.10) }  -- Medium range (±10%)
    , { lower: priceToTick (currentPrice * 0.85), upper: priceToTick (currentPrice * 1.15) }  -- Wide range (±15%)
    ]
  where
    priceToTick price = floor (log price / log 1.0001) -- Standard concentrated liquidity tick conversion

-- | Retrieve current POL range from analytics data
-- | Used for rebalancing decisions and range comparison
getCurrentPOLRange :: PoolAnalytics -> _
getCurrentPOLRange analytics = analytics.optimalPolRange

--------------------------------------------------------------------------------
-- HOT ZONE IDENTIFICATION AND ANALYSIS
--------------------------------------------------------------------------------
-- Advanced algorithms for identifying high-activity trading zones

-- | Identify and analyze hot zones where trading activity is concentrated
-- | Groups adjacent high-activity ticks into coherent trading zones
findHotZones :: Array TickMetrics -> Array HotZone
findHotZones ticks =
  let
    -- Filter for high-temperature ticks (activity score > 70)
    hotTicks = filter (\t -> t.temperature > 70.0) ticks
    zones = groupAdjacentTicks hotTicks
    
  in map createHotZone zones
  where
    -- Group adjacent ticks into coherent zones (within 10 tick spacing)
    groupAdjacentTicks :: Array TickMetrics -> Array (Array TickMetrics)
    groupAdjacentTicks ticks = case ticks of
      [] -> []
      _ -> case uncons ticks of
        Nothing -> []
        Just { head: x, tail: xs } ->
          let
            -- Identify ticks within proximity threshold
            -- TODO: TickMetrics doesn't have tick field
            -- Need to refactor to pass tick-indexed data
            result = partition (\t -> false) xs
            adjacent = result.yes
            rest = result.no
            group = x : adjacent
          in group : groupAdjacentTicks rest
    
    -- Create hot zone summary from grouped ticks
    createHotZone :: Array TickMetrics -> HotZone
    createHotZone zone = 
      let
        -- TODO: TickMetrics doesn't have tick field
        ticks = []
        minTick = fromMaybe 0 (minimum ticks)
        maxTick = fromMaybe 0 (maximum ticks)
        avgTemp = sum (map _.temperature zone) / toNumber (length zone)
        totalVolume = sum (map _.volume24h zone)
        avgVolume = totalVolume / toNumber (length zone)
        volumeConcentration = (avgVolume / 1000.0) * 100.0  -- Volume percentage
      in
        { tickLower: minTick
        , tickUpper: maxTick
        , temperature: avgTemp
        , volumeConcentration: min 100.0 volumeConcentration
        }

-- | Identify price support zones based on tick metrics
-- | Returns tick numbers where strong price support has been observed
findSupportZones :: Array TickMetrics -> Array Int  
findSupportZones _ =
  -- TODO: TickMetrics doesn't include tick number
  -- Need to refactor to pass tick-indexed data structure
  []
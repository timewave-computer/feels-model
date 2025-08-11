-- | Protocol-Owned Liquidity (POL) optimization and management
-- | Extracted POL-related code from Pool and Tick modules
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
import Data.Array ((:), take, filter, foldl, length)
import Data.Maybe (Maybe(..))
import Data.Foldable (sum, maximum)
import Data.Int (toNumber, floor)
import Data.Ord (max, maxBy)
import Data.Function (on)
import Math (log)
import Effect (Effect)

-- Import analytics from other indexer modules
import Indexer.Tick (TickMetrics)
import Protocol.Pool (PoolEvent(..), POLTriggerType(..))

--------------------------------------------------------------------------------
-- POL Types (extracted from Pool.purs)
--------------------------------------------------------------------------------

-- | Actionable recommendations for automated keepers
data KeeperAction
  = DeployPOL { amount :: Number, range :: { lower :: Int, upper :: Int }, urgency :: Number }
  | RebalancePOL { currentRange :: { lower :: Int, upper :: Int }, newRange :: { lower :: Int, upper :: Int }, reason :: String }
  | HarvestFees { amount :: Number, pool :: String }
  | EmergencySupport { pool :: String, amount :: Number }

-- | Pool analytics type (simplified to just what POL needs)
type PoolAnalytics =
  { poolId :: String
  , volume24h :: Number
  , totalValueLocked :: Number
  , polEfficiency :: Number
  , optimalPolRange :: { lower :: Int, upper :: Int }
  , recommendedPolAmount :: Number
  , historicalVolatility :: Number
  , activeTicks :: Array TickMetrics
  }

-- | Hot zones type (extracted from Pool.purs) 
type HotZone =
  { tickLower :: Int
  , tickUpper :: Int
  , temperature :: Number
  , volumeConcentration :: Number
  }

--------------------------------------------------------------------------------
-- POL Analysis Functions (extracted from Pool.purs)
--------------------------------------------------------------------------------

-- | Find optimal POL deployment range across all active ticks
findOptimalPOLRange :: PoolAnalytics -> { lower :: Int, upper :: Int, score :: Number }
findOptimalPOLRange analytics =
  let
    -- Analyze cross-tick patterns
    hotZones = findHotZones analytics.activeTicks
    supportZones = findSupportZones analytics.activeTicks
    
    -- Generate and score potential POL ranges
    candidateRanges = generatePOLRanges analytics.volume24h -- Using volume24h as proxy for currentPrice
    scoredRanges = map (scorePOLRange analytics hotZones supportZones) candidateRanges
    
    -- Select the best range based on multiple criteria
    bestRange = foldl (maxBy (_.score)) { lower: 0, upper: 0, score: 0.0 } scoredRanges
    
  in bestRange

-- | Score a POL range based on pool-wide metrics
scorePOLRange :: PoolAnalytics -> Array HotZone -> Array Int -> _ -> _
scorePOLRange analytics hotZones supportZones range =
  let
    -- Volume capture potential across ticks
    volumeScore = calculateVolumeCoverage range analytics.activeTicks
    
    -- Alignment with support levels
    supportScore = calculateSupportAlignment range supportZones
    
    -- Fee generation potential across the range
    feeScore = estimateFeeGeneration range analytics
    
    -- Risk adjustment based on pool volatility
    riskFactor = calculateRiskFactor range analytics.historicalVolatility
    
    -- Weighted composite score
    totalScore = (volumeScore * 0.4 + supportScore * 0.3 + feeScore * 0.3) * riskFactor
    
  in { lower: range.lower, upper: range.upper, score: totalScore }

-- | Generate actionable recommendations for pool keepers
generateKeeperActions :: PoolAnalytics -> Array KeeperAction
generateKeeperActions analytics =
  let
    actions = []
    
    -- Check if POL should be deployed based on pool metrics
    polAction = if shouldDeployPOL analytics
      then DeployPOL 
        { amount: analytics.recommendedPolAmount
        , range: analytics.optimalPolRange
        , urgency: calculateUrgency analytics
        } : actions
      else actions
      
    -- Check if existing POL should be rebalanced
    rebalanceAction = if shouldRebalancePOL analytics
      then RebalancePOL
        { currentRange: getCurrentPOLRange analytics
        , newRange: analytics.optimalPolRange
        , reason: "Pool efficiency metrics indicate rebalancing needed"
        } : actions
      else actions
      
    -- Check if emergency support is needed
    emergencyAction = if needsEmergencySupport analytics
      then EmergencySupport
        { pool: analytics.poolId
        , amount: calculateEmergencyAmount analytics
        } : actions
      else actions
      
  in polAction <> rebalanceAction <> emergencyAction

--------------------------------------------------------------------------------
-- POL Decision Functions (extracted from Pool.purs)
--------------------------------------------------------------------------------

shouldDeployPOL :: PoolAnalytics -> Boolean
shouldDeployPOL analytics = 
  analytics.volume24h > 100000.0 && analytics.polEfficiency < 50.0

shouldRebalancePOL :: PoolAnalytics -> Boolean  
shouldRebalancePOL analytics = 
  analytics.polEfficiency < 30.0

needsEmergencySupport :: PoolAnalytics -> Boolean
needsEmergencySupport analytics = 
  analytics.totalValueLocked < analytics.volume24h * 0.1 -- TVL too low relative to volume

calculateUrgency :: PoolAnalytics -> Number
calculateUrgency analytics = 
  if analytics.volume24h > 1000000.0 then 0.9 else 0.5

calculateEmergencyAmount :: PoolAnalytics -> Number
calculateEmergencyAmount analytics = 
  analytics.volume24h * 0.05 -- 5% of daily volume

--------------------------------------------------------------------------------
-- POL Scoring Functions (extracted from Pool.purs)
--------------------------------------------------------------------------------

calculateVolumeCoverage :: _ -> Array TickMetrics -> Number
calculateVolumeCoverage range ticks = 
  let relevantTicks = filter (\t -> t.tick >= range.lower && t.tick <= range.upper) ticks
      rangeVolume = sum $ map _.volume24h relevantTicks
      totalVolume = sum $ map _.volume24h ticks
  in if totalVolume > 0.0 then rangeVolume / totalVolume else 0.0

calculateSupportAlignment :: _ -> Array Int -> Number
calculateSupportAlignment range supportZones = 
  let zonesInRange = filter (\tick -> tick >= range.lower && tick <= range.upper) supportZones
  in toNumber (length zonesInRange) / max 1.0 (toNumber (length supportZones))

estimateFeeGeneration :: _ -> PoolAnalytics -> Number
estimateFeeGeneration range analytics = 
  analytics.volume24h * 0.003 * 0.5 -- 0.3% fee, 50% capture rate estimate

calculateRiskFactor :: _ -> Number -> Number
calculateRiskFactor range volatility = 
  max 0.1 (1.0 - volatility / 100.0) -- Higher volatility = lower risk factor

--------------------------------------------------------------------------------
-- Event Processing (extracted from Pool.purs)
--------------------------------------------------------------------------------

-- | Process POL deployment events
processPOLEvent :: _ -> _ -> Effect _
processPOLEvent pol state = do
  -- Update POL efficiency metrics
  pure state -- Simplified for now

--------------------------------------------------------------------------------
-- Helper Functions (extracted from Pool.purs)
--------------------------------------------------------------------------------

-- | Generate candidate POL ranges around current price
generatePOLRanges :: Number -> Array { lower :: Int, upper :: Int }
generatePOLRanges volume24h =
  let currentPrice = volume24h / 100000.0 -- Simple approximation
  in
    -- Generate ranges at different widths around current price
    [ { lower: priceToTick (currentPrice * 0.95), upper: priceToTick (currentPrice * 1.05) }
    , { lower: priceToTick (currentPrice * 0.90), upper: priceToTick (currentPrice * 1.10) }
    , { lower: priceToTick (currentPrice * 0.85), upper: priceToTick (currentPrice * 1.15) }
    ]
  where
    priceToTick price = floor (log price * 10000.0) -- Simplified tick conversion

getCurrentPOLRange :: PoolAnalytics -> _
getCurrentPOLRange analytics = analytics.optimalPolRange

-- selectBetter function replaced with maxBy from Data.Ord

-- Helper functions from Pool.purs
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

findSupportZones :: Array TickMetrics -> Array Int  
findSupportZones ticks =
  let
    -- Find ticks with strong support (>70% support ratio)
    supportTicks = filter (\t -> t.support > 70.0) ticks
  in map _.tick supportTicks -- Extract tick numbers

-- Math helpers now imported from standard libraries
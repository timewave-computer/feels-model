module RiskManagement
  ( StressLevel(..)
  , StressResponse
  , ProtocolStress
  , HealthFactor
  , calculateStressLevel
  , getStressResponse
  , adjustBandForStress
  , adjustLeverageForStress
  , adjustFeesForStress
  , calculateProtocolStress
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Position (AdaptiveBand, BandTier(..))

--------------------------------------------------------------------------------
-- Stress Types
--------------------------------------------------------------------------------

-- Health factor type
type HealthFactor = Number

-- Protocol stress levels
data StressLevel
  = Normal         -- Health > 1.5
  | Mild           -- Health 1.2 - 1.5
  | Moderate       -- Health 1.1 - 1.2
  | Severe         -- Health 1.0 - 1.1
  | Critical       -- Health < 1.0

derive instance eqStressLevel :: Eq StressLevel
derive instance ordStressLevel :: Ord StressLevel

instance showStressLevel :: Show StressLevel where
  show Normal = "Normal"
  show Mild = "Mild"
  show Moderate = "Moderate"
  show Severe = "Severe"
  show Critical = "Critical"

-- Stress response configuration
type StressResponse =
  { bandWidening :: Number      -- Multiplier for band width
  , leverageReduction :: Number -- Multiplier for max leverage
  , feeIncrease :: Number       -- Multiplier for fees
  , polAllocation :: Number     -- Increased POL allocation
  }

-- Protocol stress metrics
type ProtocolStress =
  { level :: StressLevel
  , healthFactor :: HealthFactor
  , response :: StressResponse
  , triggeredAt :: Maybe Number
  }

--------------------------------------------------------------------------------
-- Stress Calculation
--------------------------------------------------------------------------------

-- Calculate stress level from health factor
calculateStressLevel :: HealthFactor -> StressLevel
calculateStressLevel health
  | health >= 1.5 = Normal
  | health >= 1.2 = Mild
  | health >= 1.1 = Moderate
  | health >= 1.0 = Severe
  | otherwise = Critical

-- Get graduated stress response
getStressResponse :: StressLevel -> StressResponse
getStressResponse level = case level of
  Normal -> 
    { bandWidening: 1.0      -- No change
    , leverageReduction: 1.0 -- No change
    , feeIncrease: 1.0       -- No change
    , polAllocation: 1.0     -- Normal POL
    }
  
  Mild ->
    { bandWidening: 1.2      -- 20% wider bands
    , leverageReduction: 0.9 -- 10% less leverage
    , feeIncrease: 1.1       -- 10% higher fees
    , polAllocation: 1.2     -- 20% more POL
    }
  
  Moderate ->
    { bandWidening: 1.5      -- 50% wider bands
    , leverageReduction: 0.75 -- 25% less leverage
    , feeIncrease: 1.25      -- 25% higher fees
    , polAllocation: 1.5     -- 50% more POL
    }
  
  Severe ->
    { bandWidening: 2.0      -- 100% wider bands
    , leverageReduction: 0.5 -- 50% less leverage
    , feeIncrease: 1.5       -- 50% higher fees
    , polAllocation: 2.0     -- 100% more POL
    }
  
  Critical ->
    { bandWidening: 3.0      -- 200% wider bands
    , leverageReduction: 0.2 -- 80% less leverage
    , feeIncrease: 2.0       -- 100% higher fees
    , polAllocation: 3.0     -- 200% more POL
    }

--------------------------------------------------------------------------------
-- Stress Adjustments
--------------------------------------------------------------------------------

-- Adjust band width based on stress
adjustBandForStress :: StressLevel -> AdaptiveBand -> AdaptiveBand
adjustBandForStress stress band =
  let response = getStressResponse stress
      -- Automatically enable adaptive width under stress
      enableAdaptive = stress > Normal
  in band 
    { adaptiveWidth = band.adaptiveWidth || enableAdaptive
    -- Band tier would be widened during the updateAdaptiveBand calculation
    -- using the stress multiplier
    }

-- Get stress-adjusted band tier
getStressAdjustedTier :: StressLevel -> BandTier -> BandTier
getStressAdjustedTier stress currentTier = case stress of
  Normal -> currentTier
  Mild -> case currentTier of
    TightBand -> MediumBand
    _ -> currentTier
  Moderate -> case currentTier of
    TightBand -> WideBand
    MediumBand -> WideBand
    _ -> currentTier
  _ -> WideBand  -- Severe and Critical always use wide bands

-- Adjust maximum leverage based on stress
adjustLeverageForStress :: StressLevel -> Number -> Number
adjustLeverageForStress stress targetLeverage =
  let response = getStressResponse stress
  in targetLeverage * response.leverageReduction

-- Adjust fees based on stress
adjustFeesForStress :: StressLevel -> Number -> Number
adjustFeesForStress stress baseFee =
  let response = getStressResponse stress
  in baseFee * response.feeIncrease

--------------------------------------------------------------------------------
-- Protocol Stress Calculation
--------------------------------------------------------------------------------

-- Calculate overall protocol stress
calculateProtocolStress :: 
  { healthFactor :: HealthFactor
  , currentTime :: Number
  , previousStress :: Maybe ProtocolStress
  } -> ProtocolStress
calculateProtocolStress params =
  let level = calculateStressLevel params.healthFactor
      response = getStressResponse level
      
      -- Track when stress was first triggered
      triggeredAt = case params.previousStress of
        Nothing -> if level > Normal then Just params.currentTime else Nothing
        Just prev -> 
          if level > Normal 
          then prev.triggeredAt <|> Just params.currentTime
          else Nothing
      
  in { level
     , healthFactor: params.healthFactor
     , response
     , triggeredAt
     }

--------------------------------------------------------------------------------
-- Seniority-Based Prioritization
--------------------------------------------------------------------------------

-- Calculate return priority based on position age
calculateSeniorityMultiplier :: 
  { positionAge :: Number      -- Time since creation in ms
  , stressLevel :: StressLevel
  } -> Number
calculateSeniorityMultiplier params =
  case params.stressLevel of
    Normal -> 1.0  -- No prioritization when healthy
    Mild -> 
      let dayMs = 86400000.0
          days = params.positionAge / dayMs
      in 1.0 + (min 0.1 (days * 0.01))  -- Up to 10% bonus
    Moderate ->
      let dayMs = 86400000.0
          days = params.positionAge / dayMs
      in 1.0 + (min 0.2 (days * 0.02))  -- Up to 20% bonus
    _ ->  -- Severe/Critical
      let dayMs = 86400000.0
          days = params.positionAge / dayMs
      in 1.0 + (min 0.5 (days * 0.05))  -- Up to 50% bonus

-- Apply seniority to returns during stress
applySeniorityPrioritization ::
  { baseReturn :: Number
  , positionAge :: Number
  , stressLevel :: StressLevel
  } -> Number
applySeniorityPrioritization params =
  let multiplier = calculateSeniorityMultiplier 
        { positionAge: params.positionAge
        , stressLevel: params.stressLevel
        }
  in params.baseReturn * multiplier

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Check if protocol is under stress
isUnderStress :: StressLevel -> Boolean
isUnderStress level = level > Normal

-- Get stress duration
getStressDuration :: ProtocolStress -> Number -> Maybe Number
getStressDuration stress currentTime = case stress.triggeredAt of
  Nothing -> Nothing
  Just triggered -> Just (currentTime - triggered)

-- Calculate stress recovery rate
-- Gradual recovery to prevent oscillation
calculateRecoveryRate :: StressLevel -> Number
calculateRecoveryRate currentLevel = case currentLevel of
  Normal -> 1.0      -- Instant
  Mild -> 0.5        -- 50% per update
  Moderate -> 0.3    -- 30% per update
  Severe -> 0.2      -- 20% per update
  Critical -> 0.1    -- 10% per update
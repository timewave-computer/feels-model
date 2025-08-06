-- Risk assessment module for the unified lending protocol.
-- Calculates risk scores based on lending terms to inform fee calculations.
-- Provides a comprehensive risk framework for all lending operations.
module Risk
  ( RiskScore
  , RiskComponents
  , calculateLendingRisk
  , getRiskMultiplier
  , getRiskLevel
  , RiskLevel(..)
  ) where

import Prelude
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), unbondingPeriodToDays)
import Data.Int as Int
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- Risk Types
--------------------------------------------------------------------------------

-- Risk score from 0 (safest) to 1 (riskiest)
type RiskScore = Number

-- Components of risk assessment
type RiskComponents =
  { termsRisk :: RiskScore       -- Risk from lending terms
  , sizeRisk :: RiskScore        -- Risk from position size
  , totalRisk :: RiskScore       -- Combined risk score
  }

-- Risk levels for display/logic
data RiskLevel = Low | Medium | High | VeryHigh

derive instance eqRiskLevel :: Eq RiskLevel

instance showRiskLevel :: Show RiskLevel where
  show Low = "Low"
  show Medium = "Medium"
  show High = "High"
  show VeryHigh = "Very High"

--------------------------------------------------------------------------------
-- Risk Calculation
--------------------------------------------------------------------------------

-- Calculate risk components for a lending record
calculateLendingRisk :: LendingRecord -> RiskComponents
calculateLendingRisk record =
  let termsRisk = calculateTermsRisk record.terms
      sizeRisk = calculateSizeRisk record.lendAmount
      -- Weight: 60% terms, 40% size
      totalRisk = termsRisk * 0.6 + sizeRisk * 0.4
  in { termsRisk, sizeRisk, totalRisk }

--------------------------------------------------------------------------------
-- Component Risk Calculations
--------------------------------------------------------------------------------

-- Calculate risk from lending terms
calculateTermsRisk :: LendingTerms -> RiskScore
calculateTermsRisk terms = case terms of
  -- Swaps have low risk (perpetual, no yield expectations)
  SwapTerms -> 0.1
  
  -- Staking risk increases with unbonding period
  StakingTerms period ->
    let days = Int.toNumber (unbondingPeriodToDays period)
        -- Linear scaling: 30 days = 0.3, 60 days = 0.5, 90 days = 0.7
        baseRisk = 0.1 + (days / 150.0)
    in min 0.7 baseRisk
  
  -- Leverage risk scales with multiple
  LeverageTerms multiple ->
    -- Exponential scaling for leverage
    -- 2x = 0.4, 3x = 0.6, 5x = 0.85, 10x = 1.0
    let normalizedLeverage = (multiple - 1.0) / 9.0  -- Normalize to 0-1 range
        riskScore = normalizedLeverage * normalizedLeverage * 0.9 + 0.1
    in min 1.0 riskScore

-- Calculate risk from position size (relative to some reference)
calculateSizeRisk :: Number -> RiskScore
calculateSizeRisk amount =
  let referenceAmount = 1000.0  -- Reference "normal" position size
      relativeSize = amount / referenceAmount
  in if relativeSize <= 1.0
     then 0.1  -- Small positions have minimal size risk
     else if relativeSize <= 10.0
          then 0.1 + (relativeSize - 1.0) * 0.05  -- Linear scaling up to 10x
          else if relativeSize <= 100.0
               then 0.55 + (relativeSize - 10.0) * 0.005  -- Slower scaling 10x-100x
               else 1.0  -- Maximum risk for very large positions

--------------------------------------------------------------------------------
-- Risk Level Classification
--------------------------------------------------------------------------------

-- Get risk level from risk score
getRiskLevel :: RiskScore -> RiskLevel
getRiskLevel score
  | score <= 0.25 = Low
  | score <= 0.5 = Medium
  | score <= 0.75 = High
  | otherwise = VeryHigh

--------------------------------------------------------------------------------
-- Risk-Based Adjustments
--------------------------------------------------------------------------------

-- Convert risk score to fee multiplier
getRiskMultiplier :: RiskComponents -> Number
getRiskMultiplier components =
  let base = 1.0
      -- Each risk component adds to multiplier
      -- Terms risk gets 60% weight, size risk gets 40% weight
      termsMult = 1.0 + components.termsRisk * 0.6
      sizeMult = 1.0 + components.sizeRisk * 0.4
      -- Compound multipliers
      totalMult = base * termsMult * sizeMult
  -- Cap at 3x to prevent excessive fees
  in min 3.0 totalMult
  
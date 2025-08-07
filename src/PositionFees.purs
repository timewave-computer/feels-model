module PositionFees
  ( FeeStructure
  , FeeComponents
  , calculatePositionFee
  , initFeeStructure
  , getGranularityMultiplier
  , getTermMultiplier
  , getLeverageMultiplier
  ) where

import Prelude
import Position (Position, PriceStrategy(..), TermCommitment(..), isSpot)

--------------------------------------------------------------------------------
-- Fee Types
--------------------------------------------------------------------------------

-- Fee structure configuration
type FeeStructure =
  { baseFeeRate :: Number               -- Base fee rate (e.g., 0.003 = 0.3%)
  , granularityDiscount :: Number       -- Discount for band-aligned (e.g., 0.1 = 10%)
  , hybridDiscount :: Number            -- Discount for band-constrained (e.g., 0.05 = 5%)
  , hourlyTermBase :: Number            -- Baseline for hourly terms
  , dailyDiscount :: Number             -- Discount for daily terms (e.g., 0.05 = 5%)
  , weeklyDiscount :: Number            -- Discount for weekly terms (e.g., 0.1 = 10%)
  , spotPremium :: Number               -- Premium for spot positions (e.g., 0.05 = 5%)
  , leverageRiskFactor :: Number        -- Risk factor per unit leverage (e.g., 0.1)
  }

-- Fee calculation components
type FeeComponents =
  { baseFee :: Number
  , granularityMultiplier :: Number
  , termMultiplier :: Number
  , leverageMultiplier :: Number
  , totalFee :: Number
  , effectiveRate :: Number             -- As percentage of position amount
  }

--------------------------------------------------------------------------------
-- Fee Initialization
--------------------------------------------------------------------------------

-- Initialize default fee structure
initFeeStructure :: FeeStructure
initFeeStructure =
  { baseFeeRate: 0.003                  -- 0.3% base rate
  , granularityDiscount: 0.1            -- 10% discount for band-aligned
  , hybridDiscount: 0.05                -- 5% discount for band-constrained
  , hourlyTermBase: 1.0                 -- Baseline multiplier
  , dailyDiscount: 0.05                 -- 5% discount
  , weeklyDiscount: 0.1                 -- 10% discount  
  , spotPremium: 0.05                   -- 5% premium
  , leverageRiskFactor: 0.1             -- 10% per leverage unit above 1x
  }

--------------------------------------------------------------------------------
-- Fee Calculation
--------------------------------------------------------------------------------

-- Calculate fee for a position
calculatePositionFee :: FeeStructure -> Position -> FeeComponents
calculatePositionFee feeStruct pos =
  let baseFee = pos.amount * feeStruct.baseFeeRate
      
      -- Calculate multipliers
      granularityMult = getGranularityMultiplier feeStruct pos.priceStrategy
      termMult = getTermMultiplier feeStruct pos.term
      leverageMult = getLeverageMultiplier feeStruct pos.leverageConfig.targetLeverage
      
      -- Total fee with multiplicative effects
      totalFee = baseFee * granularityMult * termMult * leverageMult
      
      -- Effective rate as percentage
      effectiveRate = if pos.amount > 0.0 
                      then (totalFee / pos.amount) * 100.0
                      else 0.0
      
  in { baseFee
     , granularityMultiplier: granularityMult
     , termMultiplier: termMult
     , leverageMultiplier: leverageMult
     , totalFee
     , effectiveRate
     }

--------------------------------------------------------------------------------
-- Multiplier Calculations
--------------------------------------------------------------------------------

-- Get granularity multiplier based on price strategy
getGranularityMultiplier :: FeeStructure -> PriceStrategy -> Number
getGranularityMultiplier feeStruct strategy = case strategy of
  BandAligned _ -> 1.0 - feeStruct.granularityDiscount        -- 10% discount
  BandConstrained _ _ -> 1.0 - feeStruct.hybridDiscount       -- 5% discount
  TickSpecific _ -> 1.0                                       -- Full rate

-- Get term multiplier based on commitment
getTermMultiplier :: FeeStructure -> TermCommitment -> Number
getTermMultiplier feeStruct term = case term of
  Spot -> 1.0 + feeStruct.spotPremium                         -- 5% premium
  Hourly _ -> feeStruct.hourlyTermBase                        -- Baseline (1.0)
  Daily _ -> 1.0 - feeStruct.dailyDiscount                    -- 5% discount
  Weekly _ -> 1.0 - feeStruct.weeklyDiscount                  -- 10% discount

-- Get leverage multiplier based on target leverage
getLeverageMultiplier :: FeeStructure -> Number -> Number
getLeverageMultiplier feeStruct targetLeverage =
  1.0 + ((targetLeverage - 1.0) * feeStruct.leverageRiskFactor)

--------------------------------------------------------------------------------
-- Fee Analysis Functions
--------------------------------------------------------------------------------

-- Check if position qualifies for fee incentives
qualifiesForIncentive :: Position -> Boolean
qualifiesForIncentive pos =
  -- Band-aligned positions get incentives
  case pos.priceStrategy of
    BandAligned _ -> true
    BandConstrained _ _ -> true
    _ -> false

-- Calculate fee discount percentage
calculateDiscountPercentage :: FeeComponents -> Number
calculateDiscountPercentage components =
  let totalMultiplier = components.granularityMultiplier * 
                       components.termMultiplier * 
                       components.leverageMultiplier
  in if totalMultiplier < 1.0
     then (1.0 - totalMultiplier) * 100.0
     else 0.0

-- Calculate fee premium percentage  
calculatePremiumPercentage :: FeeComponents -> Number
calculatePremiumPercentage components =
  let totalMultiplier = components.granularityMultiplier * 
                       components.termMultiplier * 
                       components.leverageMultiplier
  in if totalMultiplier > 1.0
     then (totalMultiplier - 1.0) * 100.0
     else 0.0
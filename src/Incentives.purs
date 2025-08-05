-- Incentive mechanisms for the Everything is Lending protocol.
-- Unifies fee and return calculations into a single adaptive system where
-- borrower fees and lender returns are two sides of the same market mechanism.
-- Uses oracle data and system metrics to dynamically adjust rates.
module Incentives
  ( MarketDynamics
  , DynamicsConfig
  , DynamicsResult
  , BaseRate
  , Spread
  , RateComponents
  , initMarketDynamics
  , calculateDynamics
  , getBaseRate
  , getSpread
  , updateConfiguration
  , simulateDynamics

  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingTerms(..), LendingSide(..), LendingStatus(..), unbondingPeriodToDays)
import Oracle (Oracle, MarketMetrics, observeMarket)
import NFV (NFVState)

import Data.Int as Int

--------------------------------------------------------------------------------
-- Market Dynamics Types
--------------------------------------------------------------------------------

-- Base lending rate (what lenders receive)
type BaseRate = Number

-- Spread between borrower and lender rates (protocol revenue)
type Spread = Number

-- Components of rate calculation
type RateComponents =
  { baseRate :: BaseRate            -- Fundamental lending rate
  , riskPremium :: Number           -- Additional rate for risk
  , marketAdjustment :: Number      -- Dynamic market-based adjustment
  , protocolSpread :: Spread        -- Protocol revenue spread
  }

-- Result of dynamics calculation
type DynamicsResult =
  { lenderRate :: Number            -- Rate lenders receive (annualized)
  , borrowerRate :: Number          -- Rate borrowers pay (annualized)
  , effectiveSpread :: Spread       -- Actual spread after adjustments
  , nfvFlow :: Number              -- Portion flowing to NFV
  , components :: RateComponents    -- Breakdown for transparency
  , operationType :: LendingTerms  -- Type of operation
  }

-- Configuration for market dynamics
type DynamicsConfig =
  { -- Base rates by operation type
    baseRates ::
      { swap :: BaseRate           -- Base rate for swaps
      , staking :: BaseRate        -- Base rate for staking
      , leverage :: BaseRate       -- Base rate for leverage
      }
  , -- Spread configuration
    spreads ::
      { minSpread :: Spread        -- Minimum protocol spread
      , maxSpread :: Spread        -- Maximum protocol spread
      , targetSpread :: Spread     -- Target spread in normal conditions
      }
  , -- NFV allocation rates (portion of spread going to NFV)
    nfvAllocation ::
      { swap :: Number             -- 0.0-1.0
      , staking :: Number
      , leverage :: Number
      }
  , -- Adjustment factors
    adjustments ::
      { volatilityWeight :: Number    -- How much volatility affects rates
      , utilizationWeight :: Number   -- How much utilization affects rates
      , healthWeight :: Number        -- How much system health affects rates
      , crossRiskFactor :: Number     -- Risk multiplication across types
      }
  , -- Bounds
    bounds ::
      { minLenderRate :: Number       -- Minimum rate for lenders
      , maxBorrowerRate :: Number     -- Maximum rate for borrowers
      }
  }

-- Market dynamics state
type MarketDynamics =
  { config :: Ref DynamicsConfig
  , oracle :: Oracle
  , nfvState :: NFVState
  }

--------------------------------------------------------------------------------
-- Default Configuration
--------------------------------------------------------------------------------

-- Conservative default configuration
defaultDynamicsConfig :: DynamicsConfig
defaultDynamicsConfig =
  { baseRates:
    { swap: 0.02         -- 2% base for swaps
    , staking: 0.04      -- 4% base for staking
    , leverage: 0.06     -- 6% base for leverage
    }
  , spreads:
    { minSpread: 0.001   -- 0.1% minimum
    , maxSpread: 0.05    -- 5% maximum
    , targetSpread: 0.01 -- 1% target
    }
  , nfvAllocation:
    { swap: 0.15         -- 15% of spread to NFV
    , staking: 0.25      -- 25% of spread to NFV
    , leverage: 0.30     -- 30% of spread to NFV
    }
  , adjustments:
    { volatilityWeight: 0.3
    , utilizationWeight: 0.4
    , healthWeight: 0.2
    , crossRiskFactor: 0.1
    }
  , bounds:
    { minLenderRate: 0.001      -- 0.1% minimum
    , maxBorrowerRate: 0.50     -- 50% maximum
    }
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize market dynamics system
initMarketDynamics :: Oracle -> NFVState -> Effect MarketDynamics
initMarketDynamics oracle nfvState = do
  config <- new defaultDynamicsConfig
  pure { config, oracle, nfvState }

--------------------------------------------------------------------------------
-- Core Calculation
--------------------------------------------------------------------------------

-- Calculate market dynamics for a lending record
calculateDynamics :: MarketDynamics -> LendingRecord -> Effect DynamicsResult
calculateDynamics dynamics record = do
  config <- read dynamics.config
  metrics <- observeMarket dynamics.oracle
  
  -- Calculate base rate for operation type  
  let baseRate = getBaseRateForType config record.terms
  
  -- Calculate risk premium
  let riskPremium = calculateRiskPremium record.terms
  
  -- Calculate market adjustment based on current conditions
  let marketAdjustment = calculateMarketAdjustment config metrics record.terms
  
  -- Calculate dynamic spread
  let protocolSpread = calculateDynamicSpread config metrics
  
  -- Build components
  let components = { baseRate, riskPremium, marketAdjustment, protocolSpread }
  
  -- Calculate final rates
  let lenderRate = baseRate + riskPremium + marketAdjustment
      borrowerRate = lenderRate + protocolSpread
      
  -- Apply bounds
  let boundedLenderRate = max config.bounds.minLenderRate lenderRate
      boundedBorrowerRate = min config.bounds.maxBorrowerRate borrowerRate
      effectiveSpread = boundedBorrowerRate - boundedLenderRate
      
  -- Calculate NFV flow
  let nfvAllocationRate = getNFVAllocationRate config record.terms
      nfvFlow = effectiveSpread * nfvAllocationRate
  
  pure { lenderRate: boundedLenderRate
       , borrowerRate: boundedBorrowerRate
       , effectiveSpread
       , nfvFlow
       , components
               , operationType: record.terms
       }

--------------------------------------------------------------------------------
-- Rate Calculations
--------------------------------------------------------------------------------

-- Get base rate for operation type and terms
getBaseRateForType :: DynamicsConfig -> LendingTerms -> BaseRate
getBaseRateForType config terms = 
  let baseRate = case terms of
        SwapTerms -> config.baseRates.swap
        StakingTerms _ -> config.baseRates.staking
        LeverageTerms _ -> config.baseRates.leverage
  in case terms of
    -- Adjust base rate for specific terms
    StakingTerms period ->
      let days = Int.toNumber (unbondingPeriodToDays period)
          -- Higher rates for longer lock periods
          multiplier = 1.0 + (days - 30.0) / 180.0  -- +0.33% per 60 days
      in baseRate * multiplier
    
    LeverageTerms multiple ->
      -- Higher base rate for higher leverage
      baseRate * (1.0 + (multiple - 1.0) * 0.2)  -- +20% per leverage unit
    
    _ -> baseRate

-- Calculate risk premium based on lending terms
calculateRiskPremium :: LendingTerms -> Number
calculateRiskPremium terms = case terms of
  SwapTerms -> 
    0.0  -- No additional risk premium for simple swaps
  
  StakingTerms period ->
    let days = Int.toNumber (unbondingPeriodToDays period)
        -- Duration risk premium
        durationRisk = days / 365.0 * 0.02  -- 2% per year of lock
    in durationRisk
  
  LeverageTerms multiple ->
    -- Exponential risk for leverage
    let leverageRisk = (multiple - 1.0) * (multiple - 1.0) * 0.01  -- Quadratic
    in min 0.1 leverageRisk  -- Cap at 10%

-- Calculate market adjustment based on current conditions
calculateMarketAdjustment :: DynamicsConfig -> MarketMetrics -> LendingTerms -> Number
calculateMarketAdjustment config metrics terms =
  let -- Volatility adjustment
      volatilityAdj = metrics.volatility * config.adjustments.volatilityWeight
      
      -- Utilization adjustment
      utilizationAdj = 
        if metrics.utilizationRate < 0.3
        then -0.01  -- Discount for low utilization
        else if metrics.utilizationRate > 0.8
             then 0.02  -- Premium for high utilization
             else 0.0
      
      -- System health adjustment
      healthAdj = 
        if metrics.nfvGrowthRate < 0.0
        then 0.01  -- Higher rates if NFV shrinking
        else 0.0
      
      -- Cross-risk adjustment (higher risk operations pay for system risk)
      crossRiskAdj = case terms of
        SwapTerms -> 0.0
        StakingTerms _ -> metrics.volatility * config.adjustments.crossRiskFactor * 0.5
        LeverageTerms _ -> metrics.volatility * config.adjustments.crossRiskFactor
      
  in (volatilityAdj + utilizationAdj + healthAdj + crossRiskAdj) * config.adjustments.utilizationWeight

-- Calculate dynamic spread based on market conditions
calculateDynamicSpread :: DynamicsConfig -> MarketMetrics -> Spread
calculateDynamicSpread config metrics =
  let targetSpread = config.spreads.targetSpread
      
      -- Widen spread in volatile markets
      volatilityMultiplier = 1.0 + metrics.volatility
      
      -- Tighten spread in efficient markets
      efficiencyDiscount = metrics.marketEfficiency * 0.5
      
      -- Adjust for liquidity
      liquidityAdj = 
        if metrics.liquidityDepth < 10000.0  -- Low liquidity
        then 0.5  -- 50% wider spread
        else 0.0
      
      spread = targetSpread * volatilityMultiplier * (1.0 - efficiencyDiscount) * (1.0 + liquidityAdj)
      
  in clamp config.spreads.minSpread config.spreads.maxSpread spread

-- Get NFV allocation rate for operation type
getNFVAllocationRate :: DynamicsConfig -> LendingTerms -> Number
getNFVAllocationRate config terms = case terms of
  SwapTerms -> config.nfvAllocation.swap
  StakingTerms _ -> config.nfvAllocation.staking
  LeverageTerms _ -> config.nfvAllocation.leverage

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Get current base rate
getBaseRate :: MarketDynamics -> LendingTerms -> Effect BaseRate
getBaseRate dynamics terms = do
  config <- read dynamics.config
  pure $ case terms of
    SwapTerms -> config.baseRates.swap
    StakingTerms _ -> config.baseRates.staking
    LeverageTerms _ -> config.baseRates.leverage

-- Get current spread configuration
getSpread :: MarketDynamics -> Effect { min :: Spread, max :: Spread, target :: Spread }
getSpread dynamics = do
  config <- read dynamics.config
  pure { min: config.spreads.minSpread, max: config.spreads.maxSpread, target: config.spreads.targetSpread }

-- Update configuration
updateConfiguration :: MarketDynamics -> DynamicsConfig -> Effect Unit
updateConfiguration dynamics newConfig = write newConfig dynamics.config

-- Simulate dynamics for different market conditions
simulateDynamics :: MarketDynamics -> LendingTerms -> Effect DynamicsResult
simulateDynamics dynamics terms = do
  -- Create a mock record for simulation
  let mockRecord =
        { id: 0
        , side: Borrower
        , owner: "simulation"
        , lendAsset: FeelsSOL
        , lendAmount: 1000.0
        , collateralAsset: JitoSOL
        , collateralAmount: 1000.0
        , terms: terms
        , status: Active
        , createdAt: 0.0
        , matchedWith: Nothing
        , executedAt: Nothing
        }
  
  -- Temporarily override oracle metrics
  -- In production, this would create a separate simulation context
  calculateDynamics dynamics mockRecord

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- Example calculation for 60-day stake:
-- Base rate: 4% (staking)
-- Duration adjustment: +0.33% (60 days)
-- Risk premium: 0.33% (60/365 * 2%)
-- Market adjustment: +0.5% (moderate volatility)
-- Total lender rate: 5.16%
-- Protocol spread: 1.5% (volatile market)
-- Borrower rate: 6.66%
-- NFV flow: 0.375% (25% of 1.5%)

-- Example calculation for 3x leverage:
-- Base rate: 8.4% (6% * 1.4 for 3x)
-- Risk premium: 4% (quadratic risk)
-- Market adjustment: +1.2% (high volatility * cross-risk)
-- Total lender rate: 13.6%
-- Protocol spread: 2.5% (high risk)
-- Borrower rate: 16.1%
-- NFV flow: 0.75% (30% of 2.5%)

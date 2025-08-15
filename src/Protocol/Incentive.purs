-- | Incentives Module - Fee mechanism for pool-based system
-- |
-- | This module handles fee calculations for different position types and durations.
-- | Fees vary based on duration (Flash, Spot, Monthly) and contribute to POL growth.
-- | In the pool-centric system, incentives are handled through tranche returns 
-- | and pool fees, not lending rates.
module Protocol.Incentive
  ( MarketDynamics
  , DynamicsConfig
  , FeeComponents
  , initMarketDynamics
  , calculateFeeForDuration
  , calculatePOLContribution
  , getBaseFeeRate
  , getFlashLoanFeeRate
  , collectFeesFromPools
  , PoolFeeData
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new)
import Effect.Console (log)
import Data.Ord (min)
import Data.Foldable (foldM)
import Control.Monad (when)
import Protocol.Oracle (Oracle)
import Protocol.Position (Duration(..))
import Protocol.POL (POLState, contribute)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Configuration for market dynamics
type DynamicsConfig =
  { baseRate :: Number    -- Base fee rate for the protocol
  , spread :: Number      -- Protocol spread between tranches
  }

-- | Components of fee calculation
type FeeComponents =
  { baseFee :: Number        -- Base fee amount
  , durationMultiplier :: Number  -- Multiplier based on duration
  , totalFee :: Number       -- Total fee after multipliers
  }

-- | Market dynamics state for fee calculations
type MarketDynamics =
  { config :: Ref DynamicsConfig
  , oracle :: Oracle
  , polState :: POLState
  }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Initialize market dynamics with default configuration
initMarketDynamics :: Oracle -> POLState -> Effect MarketDynamics
initMarketDynamics oracle polState = do
  config <- new { baseRate: 0.003, spread: 0.001 }  -- 0.3% base rate, 0.1% spread
  pure { config, oracle, polState }

--------------------------------------------------------------------------------
-- FEE RATE FUNCTIONS
--------------------------------------------------------------------------------

-- | Base fee rates by duration (in basis points)
-- | Flash loans have lower base fee but higher volume
-- | Spot positions have standard fee
-- | Monthly positions have reduced fee to incentivize locking
getBaseFeeRate :: Duration -> Number
getBaseFeeRate Flash = 5.0      -- 0.05% for flash loans (5 bps)
getBaseFeeRate Spot = 30.0      -- 0.30% for spot positions (30 bps)
getBaseFeeRate Monthly = 20.0   -- 0.20% for monthly positions (20 bps)

-- | Get flash loan specific fee rate
getFlashLoanFeeRate :: Number
getFlashLoanFeeRate = 5.0  -- 0.05% (5 basis points)

--------------------------------------------------------------------------------
-- FEE CALCULATION FUNCTIONS
--------------------------------------------------------------------------------

-- | Calculate fee components for a given duration and amount
calculateFeeForDuration :: Duration -> Number -> FeeComponents
calculateFeeForDuration duration amount =
  let
    baseFeeRate = getBaseFeeRate duration
    
    -- Duration multipliers to balance fee revenue
    durationMultiplier = case duration of
      Flash -> 1.0    -- Standard multiplier for flash
      Spot -> 1.0     -- Standard multiplier for spot
      Monthly -> 1.2  -- Slight premium for monthly to offset lower rate
    
    -- Calculate base fee
    baseFee = amount * baseFeeRate / 10000.0
    
    -- Apply multipliers
    totalFee = baseFee * durationMultiplier
    
  in { baseFee, durationMultiplier, totalFee }

-- | Calculate POL contribution from fee components
-- | Different position types contribute different percentages to POL
calculatePOLContribution :: FeeComponents -> Duration -> Number
calculatePOLContribution fees duration =
  let
    -- POL contribution rates by duration
    -- Flash loans contribute more to POL due to high frequency
    polRate = case duration of
      Flash -> 0.40    -- 40% of flash loan fees to POL
      Spot -> 0.15     -- 15% of swap fees to POL
      Monthly -> 0.25  -- 25% of monthly fees to POL
      
  in fees.totalFee * polRate

--------------------------------------------------------------------------------
-- FEE COLLECTION FROM POOLS
--------------------------------------------------------------------------------

-- | Simple pool fee data needed for POL fee collection
type PoolFeeData =
  { poolId :: String
  , liquidity :: Number
  , feeGrowthGlobal0X128 :: Number
  , feeGrowthGlobal1X128 :: Number
  }

-- | Collect accumulated swap fees from all pools and contribute to POL
-- | Takes a POL state reference and array of pool fee data
collectFeesFromPools :: POLState -> Array PoolFeeData -> Effect Unit
collectFeesFromPools polRef pools = do
  -- Calculate total fees to collect
  totalFees <- foldM collectPoolFees 0.0 pools
  
  -- Contribute collected fees to POL if any
  when (totalFees > 0.0) do
    log $ "Collecting " <> show totalFees <> " FeelsSOL in swap fees for POL"
    contribute polRef totalFees
  
  where
    -- Collect fees from a single pool
    collectPoolFees :: Number -> PoolFeeData -> Effect Number
    collectPoolFees accFees pool = do
      -- Calculate actual fees from fee growth globals
      let totalFeeGrowth = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
          -- Convert fee growth to actual fees
          rawFees = if pool.liquidity > 0.0 && totalFeeGrowth > 0.0
                    then totalFeeGrowth * pool.liquidity / 1000000.0
                    else 0.0
          
          -- Calculate POL contribution for collected fees
          -- Assume Spot duration for pool fees (most common case)
          feeComponents = calculateFeeForDuration Spot rawFees
          collectedFees = calculatePOLContribution feeComponents Spot
      
      when (collectedFees > 0.0) do
        log $ "Pool " <> pool.poolId <> " collected " <> show collectedFees <> " FeelsSOL in fees"
      
      pure (accFees + collectedFees)


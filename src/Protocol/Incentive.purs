-- | Incentives Module - Fee mechanism for pool-based system
-- |
-- | In the pool-centric system, incentives are handled through
-- | tranche returns and pool fees, not lending rates.
module Protocol.Incentive
  ( MarketDynamics
  , DynamicsConfig
  , initMarketDynamics
  , collectAndContributePoolFees
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new)
import Effect.Console (log)
import Data.Tuple (Tuple(..))
import Data.Foldable (foldM)
import Data.Ord (min)
import Control.Monad (when)
import Protocol.Oracle (Oracle)
import Protocol.POL (POLState, contribute)
import UI.PoolRegistry (PoolRegistry, getAllPools)
import Protocol.Pool (PoolState)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Configuration for market dynamics
type DynamicsConfig =
  { baseRate :: Number    -- Base fee rate for the protocol
  , spread :: Number      -- Protocol spread between tranches
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
  config <- new { baseRate: 0.05, spread: 0.01 }
  pure { config, oracle, polState }

--------------------------------------------------------------------------------
-- FEE COLLECTION
--------------------------------------------------------------------------------

-- | Collect accumulated swap fees from all pools and contribute to POL
-- | This represents the protocol's fee harvesting mechanism that runs each block
collectAndContributePoolFees :: forall r. { poolRegistry :: PoolRegistry, polState :: POLState | r } -> Effect Unit
collectAndContributePoolFees state = do
  -- Get all pools from the registry
  pools <- getAllPools state.poolRegistry
  
  -- Calculate total fees to collect
  totalFees <- foldM collectPoolFees 0.0 pools
  
  -- Contribute collected fees to POL if any
  when (totalFees > 0.0) do
    log $ "Collecting " <> show totalFees <> " FeelsSOL in swap fees for POL"
    contribute state.polState totalFees
  
  where
    -- Collect fees from a single pool
    collectPoolFees :: Number -> Tuple String PoolState -> Effect Number
    collectPoolFees accFees (Tuple poolId pool) = do
      -- Calculate actual fees from fee growth globals
      -- Fee growth tracks cumulative fees per unit of liquidity
      let totalFeeGrowth = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
          -- Convert fee growth to actual fees (simplified calculation)
          -- In reality, this would track the delta since last collection
          collectedFees = if pool.liquidity > 0.0 && totalFeeGrowth > 0.0
                         then min (totalFeeGrowth * pool.liquidity / 1000000.0) (pool.liquidity * 0.01)
                         else 0.0
      
      when (collectedFees > 0.0) do
        log $ "Pool " <> poolId <> " collected " <> show collectedFees <> " FeelsSOL in fees"
        -- Note: In a real implementation, we'd reset fee growth after collection
        -- For simulation purposes, we'll let fees accumulate
      
      pure (accFees + collectedFees)
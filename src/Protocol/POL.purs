-- | POL Module - Protocol-Owned Liquidity management
-- |
-- | This module manages the global POL state and allocations to individual pools.
-- | It handles fee contributions, pool allocations, and rebalancing strategies.
module Protocol.POL
  ( POLState
  , POLMetrics
  , POLAllocation
  , AllocationStrategy(..)
  , initPOL
  , getPOLMetrics
  , contribute
  , allocateToPool
  , withdrawFromPool
  , getPoolAllocation
  , getTotalPOL
  , getUnallocatedPOL
  , getAllAllocations
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | POL state with pool allocations
type POLState = Ref
  { totalPOL :: Number                         -- Total POL across all sources
  , unallocated :: Number                      -- POL not yet allocated to pools
  , poolAllocations :: Map String POLAllocation  -- Pool-specific allocations
  , allocationStrategy :: AllocationStrategy     -- How to allocate POL to pools
  }

-- | POL metrics
type POLMetrics =
  { totalValue :: Number
  , utilizationRate :: Number
  , allocated :: Number      -- Total allocated to pools
  , unallocated :: Number    -- Available for allocation
  , poolCount :: Int         -- Number of pools with allocations
  }

-- | POL allocation for a specific pool
type POLAllocation =
  { poolId :: String        -- Pool identifier (e.g., "FeelsSOL/BONK")
  , allocated :: Number     -- Amount allocated to this pool
  , utilized :: Number      -- Amount currently in use
  , performance :: Number   -- Pool performance score (for reallocation)
  }

-- | Strategy for allocating POL to pools
data AllocationStrategy
  = ProportionalToVolume    -- Allocate based on trading volume
  | ProportionalToTVL       -- Allocate based on total value locked
  | FixedAllocation         -- Fixed amounts per pool
  | PerformanceBased        -- Based on pool performance metrics

derive instance eqAllocationStrategy :: Eq AllocationStrategy

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize POL state with allocation strategy
initPOL :: Effect POLState
initPOL = new
  { totalPOL: 0.0
  , unallocated: 0.0
  , poolAllocations: Map.empty
  , allocationStrategy: ProportionalToVolume  -- Default strategy
  }

--------------------------------------------------------------------------------
-- Core Operations
--------------------------------------------------------------------------------

-- | Contribute to POL (adds to unallocated)
contribute :: POLState -> Number -> Effect Unit
contribute polRef amount = do
  modify_ (\pol -> pol 
    { totalPOL = pol.totalPOL + amount
    , unallocated = pol.unallocated + amount
    }) polRef

-- | Allocate POL to a specific pool
allocateToPool :: POLState -> String -> Number -> Effect Boolean
allocateToPool polRef poolId amount = do
  pol <- read polRef
  if amount > pol.unallocated
    then pure false  -- Not enough unallocated POL
    else do
      -- Update state with allocation
      modify_ (\p -> p
        { unallocated = p.unallocated - amount
        , poolAllocations = Map.alter (updateAllocation amount) poolId p.poolAllocations
        }) polRef
      pure true
  where
    updateAllocation amt Nothing = Just 
      { poolId
      , allocated: amt
      , utilized: 0.0
      , performance: 1.0
      }
    updateAllocation amt (Just alloc) = Just $ alloc 
      { allocated = alloc.allocated + amt }

-- | Withdraw POL from a pool (for reallocation)
withdrawFromPool :: POLState -> String -> Number -> Effect Boolean
withdrawFromPool polRef poolId amount = do
  pol <- read polRef
  case Map.lookup poolId pol.poolAllocations of
    Nothing -> pure false
    Just alloc ->
      if amount > (alloc.allocated - alloc.utilized)
        then pure false  -- Can't withdraw utilized POL
        else do
          -- Update allocation and return to unallocated
          modify_ (\p -> p
            { unallocated = p.unallocated + amount
            , poolAllocations = Map.insert poolId (alloc { allocated = alloc.allocated - amount }) p.poolAllocations
            }) polRef
          pure true

--------------------------------------------------------------------------------
-- Query Operations
--------------------------------------------------------------------------------

-- | Get total POL
getTotalPOL :: POLState -> Effect Number
getTotalPOL polRef = do
  pol <- read polRef
  pure pol.totalPOL

-- | Get unallocated POL
getUnallocatedPOL :: POLState -> Effect Number
getUnallocatedPOL polRef = do
  pol <- read polRef
  pure pol.unallocated

-- | Get POL metrics
getPOLMetrics :: POLState -> Effect POLMetrics
getPOLMetrics polRef = do
  pol <- read polRef
  let allocations = fromFoldable $ Map.values pol.poolAllocations
      allocated = foldl (\acc alloc -> acc + alloc.allocated) 0.0 allocations
      utilization = if pol.totalPOL > 0.0 then allocated / pol.totalPOL else 0.0
  pure
    { totalValue: pol.totalPOL
    , utilizationRate: utilization
    , allocated: allocated
    , unallocated: pol.unallocated
    , poolCount: Map.size pol.poolAllocations
    }

-- | Get allocation for a specific pool
getPoolAllocation :: POLState -> String -> Effect (Maybe POLAllocation)
getPoolAllocation polRef poolId = do
  pol <- read polRef
  pure $ Map.lookup poolId pol.poolAllocations

-- | Get all pool allocations
getAllAllocations :: POLState -> Effect (Array POLAllocation)
getAllAllocations polRef = do
  pol <- read polRef
  pure $ fromFoldable $ Map.values pol.poolAllocations
-- | Protocol-Owned Liquidity (POL) Management System
-- |
-- | This module manages the protocol's autonomous liquidity provision system.
-- | POL represents the protocol's own capital deployed to provide liquidity,
-- | earn fees, and stabilize markets across all trading pools.
-- |
-- | Key Features:
-- | - Dynamic allocation of POL across multiple pools based on performance
-- | - Automated deployment triggers based on market conditions
-- | - Comprehensive tracking of utilization, performance, and growth metrics
-- | - Flexible allocation strategies for optimizing capital efficiency
-- |
-- | POL Lifecycle:
-- | 1. Fee contributions accumulate in unallocated POL
-- | 2. POL is allocated to high-performing pools based on strategy
-- | 3. Allocated POL is deployed to AMM positions when triggers activate
-- | 4. Performance tracking enables rebalancing and optimization
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
  , calculateGrowthRate24h
  -- POL deployment types and functions
  , POLTrigger
  , POLTriggerType(..)
  , checkPOLTrigger
  , deployPOL
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Array (fromFoldable, filter, length, drop)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl, sum)
import FFI (currentTime)
import Data.Int (toNumber)
import Data.Ord (abs, min)
import FFI (sqrt)

--------------------------------------------------------------------------------
-- POL STATE MANAGEMENT TYPES
--------------------------------------------------------------------------------
-- Core data structures for tracking POL state and allocations

-- | Main POL state containing all protocol liquidity data
-- | Uses Ref for efficient updates during frequent operations
type POLState = Ref
  { totalPOL :: Number                           -- Total POL across all sources
  , unallocated :: Number                        -- POL available for allocation
  , poolAllocations :: Map String POLAllocation  -- Per-pool allocation tracking
  , allocationStrategy :: AllocationStrategy     -- Allocation decision strategy
  , contributionHistory :: Array { timestamp :: Number, amount :: Number }  -- Growth tracking data
  }

-- | Performance and utilization metrics for POL system
-- | Provides comprehensive view of POL efficiency and usage
type POLMetrics =
  { totalValue :: Number        -- Total POL value across system
  , utilizationRate :: Number   -- Percentage of POL actively deployed
  , allocated :: Number         -- Total allocated to pools
  , unallocated :: Number       -- Available for new allocations
  , poolCount :: Int            -- Number of pools with allocations
  }

-- | Detailed allocation tracking for individual pools
-- | Tracks both allocation and deployment status with performance metrics
type POLAllocation =
  { poolId :: String            -- Pool identifier (e.g., "FeelsSOL/BONK")
  , allocated :: Number         -- Total amount allocated to pool
  , utilized :: Number          -- Amount currently deployed in AMM
  , performance :: Number       -- Performance score for reallocation decisions
  , deployedAmount :: Number    -- Actual amount in AMM positions
  , tickLower :: Int           -- Lower bound of current POL position
  , tickUpper :: Int           -- Upper bound of current POL position
  , lastDeployment :: Number   -- Timestamp of most recent deployment
  }

--------------------------------------------------------------------------------
-- POL ALLOCATION STRATEGIES
--------------------------------------------------------------------------------
-- Different approaches for distributing POL across pools

-- | Strategy for allocating POL to pools
-- | Currently supports manual allocation, with automatic strategies planned
data AllocationStrategy
  = ManualAllocation    -- Manual allocation via allocateToPool function

derive instance eqAllocationStrategy :: Eq AllocationStrategy

--------------------------------------------------------------------------------
-- POL DEPLOYMENT TRIGGER SYSTEM
--------------------------------------------------------------------------------
-- Automated conditions and types for deploying allocated POL

-- | Conditions that trigger automatic POL deployment
-- | All conditions must be evaluatable in a single on-chain instruction
type POLTrigger =
  { volumeThreshold :: Number      -- Minimum 24h volume requirement
  , priceDeviation :: Number       -- Maximum deviation from TWAP (basis points)
  , liquidityDepth :: Number       -- Minimum liquidity requirement
  , activityDecay :: Int           -- Maximum blocks since last trade
  }

-- | Categories of POL deployment triggers
-- | Each type represents different market conditions requiring POL intervention
data POLTriggerType
  = VolumeTrigger           -- High volume requiring additional liquidity
  | PriceDeviationTrigger   -- Price instability needing stabilization
  | LiquidityDepthTrigger   -- Low liquidity requiring immediate support
  | EmergencySupport        -- Critical market conditions

derive instance eqPOLTriggerType :: Eq POLTriggerType

-- | Pool metrics required for trigger evaluation
-- | Contains all data needed to assess whether POL deployment is needed
type PoolMetrics = 
  { volume24h :: Number         -- 24h trading volume
  , liquidity :: Number         -- Current liquidity depth
  , sqrtPrice :: Number         -- Current sqrt price
  , twap :: Number              -- Time-weighted average price
  , lastUpdateSlot :: Int       -- Last update slot number
  }

--------------------------------------------------------------------------------
-- POL SYSTEM INITIALIZATION
--------------------------------------------------------------------------------
-- Functions for setting up the POL system

-- | Initialize POL state with default configuration
-- | Starts with empty allocations and manual allocation strategy
initPOL :: Effect POLState
initPOL = new
  { totalPOL: 0.0
  , unallocated: 0.0
  , poolAllocations: Map.empty
  , allocationStrategy: ManualAllocation
  , contributionHistory: []
  }

--------------------------------------------------------------------------------
-- POL CONTRIBUTION AND ALLOCATION
--------------------------------------------------------------------------------
-- Core operations for managing POL funds and pool allocations

-- | Add new funds to the POL system from protocol fees
-- | All contributions are initially unallocated and require manual allocation
contribute :: POLState -> Number -> Effect Unit
contribute polRef amount = do
  timestamp <- currentTime
  modify_ (\pol -> 
    let newHistory = pol.contributionHistory <> [{ timestamp, amount }]
        -- Maintain circular buffer of last 100 contributions for efficiency
        trimmedHistory = if length newHistory > 100
                        then drop (length newHistory - 100) newHistory
                        else newHistory
    in pol 
      { totalPOL = pol.totalPOL + amount
      , unallocated = pol.unallocated + amount
      , contributionHistory = trimmedHistory
      }) polRef

-- | Allocate POL to a specific pool for potential deployment
-- | Returns false if insufficient unallocated POL available
allocateToPool :: POLState -> String -> Number -> Effect Boolean
allocateToPool polRef poolId amount = do
  pol <- read polRef
  if amount > pol.unallocated
    then pure false  -- Insufficient unallocated POL
    else do
      modify_ (\p -> p
        { unallocated = p.unallocated - amount
        , poolAllocations = Map.alter (updateAllocation amount) poolId p.poolAllocations
        }) polRef
      pure true
  where
    -- Create new allocation or add to existing one
    updateAllocation amt Nothing = Just 
      { poolId
      , allocated: amt
      , utilized: 0.0
      , performance: 1.0           -- Default performance score
      , deployedAmount: 0.0
      , tickLower: 0
      , tickUpper: 0
      , lastDeployment: 0.0
      }
    updateAllocation amt (Just alloc) = Just $ alloc 
      { allocated = alloc.allocated + amt }

-- | Withdraw POL from a pool allocation back to unallocated funds
-- | Only allows withdrawal of non-utilized POL (not actively deployed)
withdrawFromPool :: POLState -> String -> Number -> Effect Boolean
withdrawFromPool polRef poolId amount = do
  pol <- read polRef
  case Map.lookup poolId pol.poolAllocations of
    Nothing -> pure false
    Just alloc ->
      if amount > (alloc.allocated - alloc.utilized)
        then pure false  -- Cannot withdraw utilized POL
        else do
          modify_ (\p -> p
            { unallocated = p.unallocated + amount
            , poolAllocations = Map.insert poolId (alloc { allocated = alloc.allocated - amount }) p.poolAllocations
            }) polRef
          pure true

--------------------------------------------------------------------------------
-- POL QUERY AND METRICS OPERATIONS
--------------------------------------------------------------------------------
-- Functions for retrieving POL state information and calculating metrics

-- | Get total POL value across the entire system
getTotalPOL :: POLState -> Effect Number
getTotalPOL polRef = do
  pol <- read polRef
  pure pol.totalPOL

-- | Get amount of POL available for new allocations
getUnallocatedPOL :: POLState -> Effect Number
getUnallocatedPOL polRef = do
  pol <- read polRef
  pure pol.unallocated

-- | Calculate comprehensive POL system metrics
-- | Provides utilization rates, allocation status, and pool distribution
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

-- | Get allocation details for a specific pool
getPoolAllocation :: POLState -> String -> Effect (Maybe POLAllocation)
getPoolAllocation polRef poolId = do
  pol <- read polRef
  pure $ Map.lookup poolId pol.poolAllocations

-- | Get all current pool allocations as an array
getAllAllocations :: POLState -> Effect (Array POLAllocation)
getAllAllocations polRef = do
  pol <- read polRef
  pure $ fromFoldable $ Map.values pol.poolAllocations

-- | Calculate POL growth rate over the last 24 hours
-- | Returns percentage growth based on contribution history
calculateGrowthRate24h :: POLState -> Effect Number
calculateGrowthRate24h polRef = do
  pol <- read polRef
  now <- currentTime
  let dayAgo = now - 86400000.0  -- 24 hours in milliseconds
      recentContributions = filter (\c -> c.timestamp > dayAgo) pol.contributionHistory
      totalGrowth = sum $ map _.amount recentContributions
  -- Return growth as percentage of total POL
  pure $ if pol.totalPOL > 0.0 
         then (totalGrowth / pol.totalPOL) * 100.0
         else 0.0

--------------------------------------------------------------------------------
-- POL DEPLOYMENT AND TRIGGER EVALUATION
--------------------------------------------------------------------------------
-- Automated deployment system for deploying allocated POL to AMM positions

-- | Evaluate whether POL should be automatically deployed to a pool
-- | All checks are designed to execute efficiently in on-chain environment
checkPOLTrigger :: PoolMetrics -> POLTrigger -> Int -> Boolean
checkPOLTrigger pool trigger currentSlot =
  let
    -- Volume-based trigger: Deploy when high volume indicates demand
    volumeCheck = pool.volume24h > trigger.volumeThreshold
    
    -- Price stability trigger: Deploy when price deviates significantly from TWAP
    priceCheck = 
      let deviation = abs (pool.sqrtPrice - pool.twap) / pool.twap * 10000.0
      in deviation > trigger.priceDeviation
      
    -- Liquidity depth trigger: Deploy when liquidity falls below threshold
    liquidityCheck = pool.liquidity > trigger.liquidityDepth
    
    -- Activity trigger: Only deploy to recently active pools
    activityCheck = (currentSlot - pool.lastUpdateSlot) < trigger.activityDecay
    
  in volumeCheck && priceCheck && liquidityCheck && activityCheck

-- | Deploy allocated POL to an AMM position in the specified tick range
-- | Updates allocation tracking with deployment details and position bounds
deployPOL :: POLState -> String -> Number -> Int -> Int -> POLTriggerType -> Effect Boolean
deployPOL polRef poolId amount tickLower tickUpper triggerType = do
  pol <- read polRef
  case Map.lookup poolId pol.poolAllocations of
    Nothing -> pure false  -- Pool allocation not found
    Just alloc -> 
      if amount > (alloc.allocated - alloc.utilized)
        then pure false  -- Insufficient allocated POL for deployment
        else do
          now <- currentTime
          -- Record deployment in allocation tracking
          let updatedAlloc = alloc 
                { utilized = alloc.utilized + amount
                , deployedAmount = alloc.deployedAmount + amount
                , tickLower = tickLower
                , tickUpper = tickUpper
                , lastDeployment = now
                }
          modify_ (\p -> p
            { poolAllocations = Map.insert poolId updatedAlloc p.poolAllocations
            }) polRef
          pure true

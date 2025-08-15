# POL and Pool Architecture

## Overview

The relationship between Protocol-Owned Liquidity (POL) and individual Pools has been clarified to establish a clear hierarchy and flow of funds.

## Architecture

### 1. Global POL Management

**POL Module** serves as the centralized authority for all protocol-owned liquidity:

```purescript
type POLState = Ref
  { totalPOL :: Number                         -- Total POL across all sources
  , unallocated :: Number                      -- POL not yet allocated to pools
  , poolAllocations :: Map String POLAllocation  -- Pool-specific allocations
  , allocationStrategy :: AllocationStrategy     -- How to allocate POL to pools
  , contributionHistory :: Array { timestamp :: Number, amount :: Number }  -- History of contributions
  }
```

**Key Responsibilities:**
- Receives contributions from protocol fees (FeelsSOL minting/burning fees, trading fees, etc.)
- Manages allocation of POL to individual pools
- Tracks performance and rebalances allocations
- Maintains a reserve of unallocated POL for new opportunities

### 2. Pool-Level POL Usage

-- The concept of "ManagedLiquidity" is handled by "POLAllocation" in Protocol.POL.purs
-- and "TrancheState" in Protocol.Pool.purs. It is not explicitly defined as a top-level record.

**Key Points:**
- `polAllocation`: Amount allocated from global POL to this pool (part of `POLAllocation` in `Protocol.POL.purs`)
- `polUtilized`: Amount currently deployed in liquidity positions (part of `POLAllocation` in `Protocol.POL.purs`)
- Pools cannot exceed their allocation without requesting more from POLManager (enforced by `deployPOL` in `Protocol.POL.purs`)

### 3. Flow of Funds

```
Protocol Fees → Global POL → Pool Allocations → Liquidity Positions
                    ↑                               ↓
                    └─────── Rebalancing ───────────┘
```

1. **Fee Collection**: FeelsSOL minting/burning fees, trading fees, and staking rewards flow to global POL
2. **Allocation**: POLManager allocates POL to pools based on strategy (volume, TVL, performance)
3. **Deployment**: Pools deploy their allocated POL into liquidity positions
4. **Rebalancing**: Underperforming pools can have POL withdrawn and reallocated (conceptual process orchestrated by keepers/off-chain logic)

### 4. Allocation Strategies

```purescript
data AllocationStrategy
  = ManualAllocation    -- Manual allocation via allocateToPool function
```

-- Other allocation strategies (ProportionalToVolume, ProportionalToTVL, FixedAllocation, PerformanceBased)
-- are conceptual and planned for future implementation.

### 5. Benefits

1. **Clear Ownership**: Global POL is protocol-owned, pool allocations are pool-managed
2. **Flexibility**: POL can be rebalanced between pools based on performance
3. **Accountability**: Each pool's POL usage is tracked and measured
4. **Scalability**: New pools can receive allocations from the global reserve
5. **Unified Interface**: Single POL module handles both global state and pool allocations

## Example Usage

```purescript
-- FeelsSOL system contributes fees to global POL
contribute polState feeAmount

-- Allocate POL to a high-performing pool
allocateToPool polState "FeelsSOL/BONK" 1000.0

-- Pool uses its allocation for liquidity
deployPOL polState poolId amount tickLower tickUpper triggerType

-- Rebalance from underperforming pool
withdrawFromPool polState "FeelsSOL/COPE" 500.0
allocateToPool polState "FeelsSOL/WIF" 500.0
```

## Migration Path

1. Replace `POL.purs` stub with enhanced POL module ✓ Completed
2. Update Pool initialization to request POL allocations from unified POL module (Conceptual/Planned)
3. Implement rebalancing logic based on pool metrics (Conceptual/Planned)
4. Add POL allocation strategy configuration to system initialization (Conceptual/Planned)
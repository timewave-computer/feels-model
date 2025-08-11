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
  , tokenBalances :: Array { token :: TokenType, balance :: Number }  -- Legacy field
  }
```

**Key Responsibilities:**
- Receives contributions from protocol fees (Gateway fees, trading fees, etc.)
- Manages allocation of POL to individual pools
- Tracks performance and rebalances allocations
- Maintains a reserve of unallocated POL for new opportunities

### 2. Pool-Level POL Usage

Each **Pool** has allocated POL that it can deploy:

```purescript
type ManagedLiquidity =
  { total :: Number                -- Total protocol-managed liquidity
  , senior :: TrancheBucket        -- Senior positions (1x exposure)
  , junior :: TrancheBucket        -- Junior positions (up to 3x exposure)
  , polAllocation :: Number        -- POL allocated to this pool (from global POL)
  , polUtilized :: Number          -- POL currently deployed in positions
  , distribution :: Distribution   -- How it's distributed across ticks
  , termBuckets :: Map TermType TermBucket  -- Organize by term
  }
```

**Key Points:**
- `polAllocation`: Amount allocated from global POL to this pool
- `polUtilized`: Amount currently deployed in liquidity positions
- Pools cannot exceed their allocation without requesting more from POLManager

### 3. Flow of Funds

```
Protocol Fees → Global POL → Pool Allocations → Liquidity Positions
                    ↑                               ↓
                    └─────── Rebalancing ───────────┘
```

1. **Fee Collection**: Gateway fees, trading fees, and staking rewards flow to global POL
2. **Allocation**: POLManager allocates POL to pools based on strategy (volume, TVL, performance)
3. **Deployment**: Pools deploy their allocated POL into liquidity positions
4. **Rebalancing**: Underperforming pools can have POL withdrawn and reallocated

### 4. Allocation Strategies

```purescript
data AllocationStrategy
  = ProportionalToVolume    -- More volume = more POL
  = ProportionalToTVL       -- Larger pools get more POL
  = FixedAllocation         -- Fixed amounts per pool
  = PerformanceBased        -- Better performing pools get more
```

### 5. Benefits

1. **Clear Ownership**: Global POL is protocol-owned, pool allocations are pool-managed
2. **Flexibility**: POL can be rebalanced between pools based on performance
3. **Accountability**: Each pool's POL usage is tracked and measured
4. **Scalability**: New pools can receive allocations from the global reserve
5. **Unified Interface**: Single POL module handles both global state and pool allocations

## Example Usage

```purescript
-- Gateway contributes fees to global POL
contributeToTokenPOL polState JitoSOL feeAmount

-- Allocate POL to a high-performing pool
allocateToPool polState "FeelsSOL/BONK" 1000.0

-- Pool uses its allocation for liquidity
deployPOLLiquidity pool amount

-- Rebalance from underperforming pool
withdrawFromPool polState "FeelsSOL/COPE" 500.0
allocateToPool polState "FeelsSOL/WIF" 500.0
```

## Migration Path

1. ~~Replace `POL.purs` stub with enhanced POL module~~ ✓ Completed
2. Update Pool initialization to request POL allocations from unified POL module
3. Implement rebalancing logic based on pool metrics
4. Add POL allocation strategy configuration to system initialization
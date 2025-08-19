# Hybrid Routing Approach for 3D AMM on Solana

## Overview

This document outlines a hybrid routing strategy for the Feels Protocol 3D AMM that balances decentralization, efficiency, and user experience. The approach leverages Solana's performance advantages while respecting its computational constraints.

## Background

### Current Landscape

**Uniswap V3**: 
- Off-chain routing computation (Auto Router)
- On-chain execution of pre-computed paths
- Multiple sequential pool interactions

**Balancer**: 
- Off-chain Smart Order Router (SOR)
- Batch solving for gas efficiency
- Single on-chain transaction

### Our Challenge

The Feels Protocol operates in 3D space (rate × duration × leverage), making routing exponentially more complex than 2D AMMs:
- Single-dimension swaps: Simple, direct
- Multi-dimensional swaps: Multiple paths possible
- Composite swaps: Complex optimization required

## Hybrid Architecture

### Tier 1: On-Chain Direct Routing

Handles simple, common swap patterns directly on-chain for maximum decentralization.

**Supported Operations**:
1. **Single-dimension swaps** (most common)
   - Rate-only: Changing interest rates
   - Duration-only: Flash ↔ Monthly transitions
   - Leverage-only: Senior ↔ Junior rebalancing

2. **Adjacent 2-hop routes**
   - Pre-computed common paths
   - Limited to 2 dimension changes

**Implementation**:
```rust
pub fn route_swap_simple(
    ctx: Context<SwapRoute>,
    start_tick: TickCoordinate,
    end_tick: TickCoordinate,
    amount_in: u64,
    min_amount_out: u64,
) -> Result<()> {
    // Check if single dimension swap
    let dims_changed = count_dimension_changes(&start_tick, &end_tick);
    
    if dims_changed <= 1 {
        // Direct swap through single pool
        return execute_direct_swap(ctx, start_tick, end_tick, amount_in);
    }
    
    if dims_changed == 2 {
        // Try cached 2-hop route
        if let Some(path) = get_cached_route(&start_tick, &end_tick) {
            return execute_path(ctx, path, amount_in);
        }
    }
    
    // Fallback to off-chain routing
    return Err(ErrorCode::RequiresOffchainRouting);
}
```

### Tier 2: Off-Chain Solver with On-Chain Verification

Complex routing computed off-chain with cryptographic proofs for trustless execution.

**Features**:
1. **Multi-dimensional optimization**
   - Full 3D pathfinding
   - Liquidity aggregation
   - Slippage optimization

2. **Proof-based execution**
   - Solver provides merkle proof of computation
   - On-chain verification of route validity
   - Atomic execution with guarantees

**Architecture**:
```
┌─────────────┐     ┌──────────────┐     ┌─────────────┐
│   Client    │────▶│ Route Solver │────▶│   Solana    │
│             │     │   (Off-chain) │     │  Program    │
└─────────────┘     └──────────────┘     └─────────────┘
      │                     │                     │
      │  1. Request route   │  3. Submit proof   │
      │                     │     + path         │
      │◀────────────────────┼─────────────────────┤
         2. Signed quote       4. Execute swap
```

**Solver Implementation**:
```typescript
interface RouteSolution {
  path: SwapStep[];
  expectedOutput: bigint;
  maxSlippage: number;
  proof: MerkleProof;
  solver: PublicKey;
  timestamp: number;
  signature: Signature;
}

interface SwapStep {
  fromTick: TickCoordinate;
  toTick: TickCoordinate;
  pool: PublicKey;
  liquidity: bigint;
  priceImpact: number;
}
```

## On-Chain Routing Constraints

### Compute Budget
- **Limit**: ~1.4M compute units per transaction
- **Simple swap**: ~50k CUs
- **2-hop route**: ~150k CUs  
- **Path finding**: ~500k CUs (limited depth)

### Optimization Strategies

1. **Tick Data Aggregation**
   ```rust
   pub struct TickBundle {
       pub start_tick: i32,
       pub end_tick: i32,
       pub total_liquidity: u128,
       pub weighted_price: u64,
   }
   ```

2. **Heuristic Pruning**
   - Skip low-liquidity paths
   - Prefer direct dimension changes
   - Limit search depth by gas

3. **Caching Common Routes**
   ```rust
   pub struct RouteCache {
       pub routes: HashMap<RouteKey, CachedPath>,
       pub last_update: i64,
   }
   ```

## Implementation Phases

### Phase 1: Direct Swaps Only
- Single-dimension on-chain routing
- Off-chain solver for complex routes
- Basic verification

### Phase 2: Enhanced On-Chain
- 2-hop pathfinding
- Route caching
- Liquidity-aware routing

### Phase 3: Optimistic Solving
- Solver reputation system
- Optimistic execution
- Slashing for bad routes

## Security Considerations

### On-Chain Routing
- **Sandwich protection**: Slippage checks
- **MEV resistance**: Commit-reveal for large swaps
- **DoS prevention**: Compute limits on pathfinding

### Off-Chain Solver
- **Proof verification**: Ensure solution matches quote
- **Timeout protection**: Quotes expire after N slots
- **Solver accountability**: Stake-based reputation

## Performance Benchmarks

### Target Metrics
| Operation | Compute Units | Latency |
|-----------|---------------|---------|
| Direct swap | 50k | < 1s |
| 2-hop route | 150k | < 2s |
| Off-chain solve | N/A | < 3s |
| Proof verification | 200k | < 1s |

### Comparison
| Feature | Uniswap V3 | Balancer | Feels 3D |
|---------|------------|----------|----------|
| On-chain routing | No | No | Partial |
| Dimensions | 1 | 2 | 3 |
| Solver model | Client-side | SOR | Hybrid |
| Decentralization | High | Medium | High |

## Future Optimizations

1. **ZK-Proof Routing**: Verify complex routes with zero-knowledge proofs
2. **Cross-Program Invocation**: Compose with other Solana protocols
3. **Parallel Route Execution**: Leverage Solana's parallel runtime
4. **AI-Powered Heuristics**: ML models for route prediction

## Conclusion

The hybrid approach provides the best of both worlds:
- **Decentralization** for common operations
- **Efficiency** for complex routing
- **Flexibility** to evolve with Solana's capabilities

By starting simple and progressively moving computation on-chain, we can deliver a superior user experience while maintaining the protocol's decentralized ethos.
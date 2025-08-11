# Solana POL Architecture

## Design Principles
1. **Instant POL reactions stay on-chain** - Critical market events trigger immediate POL deployment
2. **Complex metrics move off-chain** - Temperature, support, reliability calculated by indexers
3. **Efficient state management** - Minimal on-chain storage, maximum event emission
4. **No backwards compatibility** - Clean architecture from the start

## On-Chain POL System

### 1. Minimal State Structure
```rust
// Only store what's needed for instant decisions
pub struct Pool {
    // Core swap state
    pub sqrt_price: u128,
    pub tick: i32,
    pub liquidity: u128,
    
    // Instant POL triggers (single values, no arrays)
    pub volume_spike_ratio: u16,    // Current vs 24h avg (basis points)
    pub price_volatility: u16,      // Recent price swings (basis points)
    pub liquidity_ratio: u16,       // Current vs baseline (basis points)
    pub last_action_slot: u64,      // For rate limiting
    
    // Active POL
    pub pol_reserves: u64,
    pub pol_deployed: u64,
}
```

### 2. Instant Trigger System
```rust
// On-chain POL triggers that execute in single instruction
pub fn check_instant_pol_trigger(pool: &Pool) -> Option<InstantPOLAction> {
    // Panic conditions - deploy POL immediately
    if pool.volume_spike_ratio > 1000 {  // 10x spike
        return Some(InstantPOLAction::PanicSupport);
    }
    
    if pool.price_volatility > 500 {  // 5% move
        return Some(InstantPOLAction::VolatilityDampening);
    }
    
    if pool.liquidity_ratio < 50 {  // 50% drained
        return Some(InstantPOLAction::LiquiditySupport);
    }
    
    None
}
```

### 3. Efficient POL Deployment
```rust
// Simple POL deployment - no complex calculations
pub fn deploy_instant_pol(
    pool: &mut Pool,
    action: InstantPOLAction,
) -> Result<()> {
    let amount = match action {
        InstantPOLAction::PanicSupport => pool.pol_reserves / 10,  // 10%
        InstantPOLAction::VolatilityDampening => pool.pol_reserves / 20,  // 5%
        InstantPOLAction::LiquiditySupport => pool.pol_reserves / 5,   // 20%
    };
    
    // Simple range around current tick
    let range_width = match action {
        InstantPOLAction::PanicSupport => 200,        // Wide
        InstantPOLAction::VolatilityDampening => 100, // Medium
        InstantPOLAction::LiquiditySupport => 300,    // Very wide
    };
    
    pool.pol_deployed += amount;
    pool.pol_reserves -= amount;
    
    emit!(POLDeployed {
        pool: pool.key(),
        amount,
        tick_lower: pool.tick - range_width,
        tick_upper: pool.tick + range_width,
        trigger: action,
    });
    
    Ok(())
}
```

## Off-Chain POL Optimization

### 1. Event Processing
```typescript
// Index all on-chain events
export class POLIndexer {
  async processEvent(event: PoolEvent) {
    switch (event.type) {
      case 'Swap':
        await this.updateTickMetrics(event);
        await this.calculateTemperature(event.tick);
        break;
      case 'POLDeployed':
        await this.trackPOLEfficiency(event);
        break;
    }
  }
}
```

### 2. Complex Metrics Calculation
```typescript
// Calculate sophisticated metrics off-chain
export class MetricsEngine {
  calculateTickTemperature(tick: number, events: SwapEvent[]): number {
    const recentEvents = events.filter(e => e.timestamp > Date.now() - 3600);
    const volume = recentEvents.reduce((sum, e) => sum + e.volume, 0);
    const frequency = recentEvents.length;
    
    // Complex calculation without gas concerns
    const volumeScore = Math.min(1, volume / 100000);
    const frequencyScore = Math.min(1, frequency / 100);
    const recencyDecay = this.calculateRecencyDecay(recentEvents);
    
    return (volumeScore * 0.4 + frequencyScore * 0.3 + recencyDecay * 0.3) * 100;
  }
  
  findOptimalPOLRange(pool: PoolAnalytics): Range {
    // Run expensive optimization off-chain
    const ranges = this.generateCandidateRanges(pool);
    const scores = ranges.map(r => this.scoreRange(r, pool));
    return ranges[scores.indexOf(Math.max(...scores))];
  }
}
```

### 3. Keeper Bot Strategy
```typescript
// Keeper decides when to trigger optimized POL
export class POLKeeper {
  async checkPool(poolId: string) {
    const analytics = await this.indexer.getPoolAnalytics(poolId);
    
    // Complex decision logic
    if (this.shouldDeployOptimizedPOL(analytics)) {
      const range = this.metricsEngine.findOptimalPOLRange(analytics);
      const amount = this.calculateOptimalAmount(analytics);
      
      // Submit transaction only if profitable
      if (this.isProfitable(amount, analytics)) {
        await this.deployPOL(poolId, amount, range);
      }
    }
  }
}
```

## Key Benefits

1. **Gas Efficiency**: On-chain code only handles simple comparisons and arithmetic
2. **Instant Reactions**: Critical events trigger POL without waiting for keeper
3. **Rich Analytics**: Complex metrics calculated off-chain without constraints
4. **Flexible Strategy**: Keepers can implement sophisticated strategies
5. **Clean Separation**: No mixing of on-chain and off-chain concerns

## Implementation Order

1. **Phase 1**: Build minimal on-chain POL triggers
2. **Phase 2**: Deploy event indexer infrastructure  
3. **Phase 3**: Implement keeper network
4. **Phase 4**: Add advanced off-chain analytics
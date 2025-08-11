# On-Chain Architecture Design

## Core Principles
- Minimal state storage
- Event emission for off-chain indexing
- Critical POL decisions remain on-chain
- Stateless ticks with only essential data

## On-Chain Components

### 1. Stateless Tick Structure
```rust
// Minimal tick state - only what's needed for swaps
pub struct Tick {
    pub liquidity_net: i128,      // Net liquidity change when crossing
    pub liquidity_gross: u128,    // Total liquidity referencing this tick
    pub fee_growth_outside: u128, // Fee growth on other side of tick
}
```

### 2. Efficient POL Triggers
```rust
// On-chain POL trigger conditions (minimal computation)
pub struct POLTrigger {
    pub volume_threshold: u64,      // Simple volume check
    pub price_deviation: u16,       // Basis points from TWAP
    pub liquidity_depth: u128,      // Minimum liquidity requirement
    pub time_weighted_activity: u32 // Simple decay counter
}

// Lightweight POL decision
pub fn check_pol_trigger(
    pool: &Pool,
    trigger: &POLTrigger,
    current_price: u128
) -> bool {
    // Simple checks that can execute in single instruction
    pool.volume_24h > trigger.volume_threshold &&
    calculate_deviation(current_price, pool.twap) > trigger.price_deviation &&
    pool.liquidity > trigger.liquidity_depth
}
```

### 3. Event Emission Pattern
```rust
// Events for off-chain indexing
pub enum ProtocolEvent {
    Swap {
        pool: Pubkey,
        amount_in: u64,
        amount_out: u64,
        sqrt_price: u128,
        tick: i32,
        fee: u64,
    },
    LiquidityChange {
        pool: Pubkey,
        tick_lower: i32,
        tick_upper: i32,
        liquidity_delta: i128,
    },
    POLDeployed {
        pool: Pubkey,
        amount: u64,
        tick: i32,
        trigger_type: POLTriggerType,
    },
}
```

### 4. Minimal Pool State
```rust
pub struct Pool {
    // Essential swap state
    pub sqrt_price: u128,
    pub tick: i32,
    pub liquidity: u128,
    
    // Minimal metrics for POL
    pub volume_24h: u64,         // Rolling volume (single value)
    pub fee_growth_global: u128,
    pub twap: u128,              // Time-weighted average price
    pub last_update_slot: u64,
    
    // POL state
    pub pol_amount: u64,
    pub pol_tick_lower: i32,
    pub pol_tick_upper: i32,
}
```

## Off-Chain Components

### 1. Metrics Indexer
- Processes all swap events
- Calculates complex metrics (temperature, support, reliability)
- Maintains historical data
- Provides API for UI and analytics

### 2. POL Optimizer
- Analyzes off-chain metrics
- Simulates POL deployment strategies
- Generates recommendations
- Can trigger on-chain POL via keeper

### 3. Keeper Network
- Monitors for POL opportunities
- Executes complex POL strategies
- Rebalances positions
- Harvests fees

## Implementation Strategy

### Phase 1: Core Refactor
1. Strip tick state to minimum
2. Move metrics calculation off-chain
3. Implement event emission

### Phase 2: POL Optimization
1. Design efficient on-chain triggers
2. Build off-chain optimizer
3. Implement keeper infrastructure

### Phase 3: Advanced Features
1. Multi-level POL strategies
2. Cross-pool coordination
3. Dynamic parameter adjustment
# 3D Tick Architecture

## Overview

The Feels Protocol extends the traditional AMM tick concept from one dimension (price) to three dimensions:
- **Price**: Traditional price levels (in basis points)
- **Duration**: Time commitment (Flash, Monthly, Spot)
- **Leverage**: Risk preference (Senior 1x, Junior 3x)

## Key Concepts

### 3D Ticks as Discrete Order Points

Just as traditional AMMs use price ticks to discretize the continuous price space, our system uses 3D ticks to discretize the order space:

```
Traditional AMM:         3D OrderBook:
Price → Tick            (Price, Duration, Leverage) → 3D Tick
```

### Tick Coordinates

Each tick has a coordinate in 3D space:
```purescript
type TickCoordinate =
  { priceTick :: Int      -- e.g., 100 = 1.01 price
  , durationTick :: Int   -- 0=Flash, 1=Monthly, 2=Spot  
  , leverageTick :: Int   -- 0=Senior, 1=Junior
  }
```

### Orders as Tick Liquidity

When users place orders, they're adding liquidity at specific 3D ticks:

1. **Traditional AMM**: LP adds liquidity between price ticks 100-200
2. **3D OrderBook**: User places order at tick (100, 1, 0) = (1.01 price, Spot duration, Senior leverage)

## Example Flow

### 1. User Places Order
```
User wants to lend 1000 FeelsSOL at:
- Price: 1.05
- Duration: Monthly (28 days)
- Leverage: Senior (1x)

This maps to 3D tick coordinate: (487, 2, 0)
```

### 2. Order Added to Tick
```
OrderBook updates:
- Order stored at tick (487, 2, 0)
- Tick liquidity increased by 1000
- Tick marked as active
```

### 3. Matching Across Dimensions
```
Borrower wants 500 FeelsSOL at:
- Price: 1.05 or better
- Duration: Monthly
- Leverage: Senior

System finds match at tick (487, 2, 0)
Orders are partially matched
```

## Benefits of Tick-Based Design

### 1. Efficient Storage
- Orders at same terms share a tick
- Sparse storage - only active ticks stored
- Natural aggregation of liquidity

### 2. Fast Matching
- Direct lookup by tick coordinate
- Neighboring tick search for price ranges
- Pre-aggregated liquidity at each tick

### 3. Familiar Mental Model
- LPs understand tick-based liquidity
- Natural extension of Uniswap V3 concepts
- Clear discrete choices for users

### 4. Composability
- Vaults can target specific tick ranges
- Strategies can optimize across dimensions
- Clear liquidity distribution visibility

## Comparison with Pure Orderbook

| Feature | Pure Orderbook | 3D Tick System |
|---------|---------------|----------------|
| Order Placement | Continuous prices | Discrete ticks |
| Storage | Every order separate | Orders aggregated by tick |
| Matching | Compare all orders | Check specific ticks |
| Mental Model | Traditional trading | AMM-like with dimensions |
| Liquidity View | Order depth | Tick liquidity |

## Position Vault Integration

The PositionVault acts as an aggregator that:
1. Takes user deposits
2. Analyzes tick liquidity across dimensions
3. Places orders at optimal ticks
4. Provides CFMM-like quotes

This gives users the simplicity of an AMM interface while leveraging the expressiveness of the 3D tick system.

## Future Extensions

The tick system can be extended with:
- More leverage tiers (2x, 4x, etc.)
- Additional duration options (Weekly, Daily)
- Dynamic tick spacing based on volatility
- Cross-tick liquidity aggregation strategies
# Pool System Specification

## Overview

The Pool system in `src/Protocol/Pool.purs` implements a sophisticated 3D Automated Market Maker (AMM) with concentrated liquidity across three dimensions: rate, duration, and leverage. This document provides a complete technical specification of the Pool module's architecture, operations, and mathematical foundations.

## Core Concepts

### 3D AMM Model

The pool operates in a three-dimensional space where:
- **Rate (R)**: Interest rate dimension representing yield
- **Duration (D)**: Time dimension for position maturity  
- **Leverage (L)**: Risk dimension for capital efficiency

The pool maintains a constant product invariant:
```
R^wr × D^wd × L^wl = K
```

Where:
- `wr`, `wd`, `wl` are the weights for each dimension
- `K` is the constant product invariant

### Concentrated Liquidity

Liquidity is concentrated within specific tick ranges across all three dimensions, allowing:
- Capital efficiency through focused liquidity provision
- Custom risk profiles via tick range selection
- Dynamic fee capture based on trading activity

## Data Structures

### Pool State

```purescript
type Pool = 
  { id :: PoolId
  , token :: Token
  , totalLiquidity :: Number
  , sqrtPrice :: Number
  , currentTick :: TickCoordinate
  , feeGrowthGlobal :: FeeGrowth
  , protocolFees :: ProtocolFees
  , tickData :: Map TickCoordinate TickData
  , positions :: Map PositionId Position
  , oracle :: Maybe Oracle
  , lastUpdateBlock :: BlockNumber
  , poolParameters :: PoolConfig
  , eventEmitter :: EventEmitter
  , isActive :: Boolean
  }
```

### Virtual Balances

The pool maintains virtual balances for each dimension:

```purescript
type VirtualBalances = 
  { rateBalance :: Number      -- Virtual rate balance
  , durationBalance :: Number   -- Virtual duration balance  
  , leverageBalance :: Number   -- Virtual leverage balance
  }
```

### Tick System

```purescript
type TickCoordinate = 
  { rateTick :: Int       -- Tick position in rate dimension
  , durationTick :: Int   -- Tick position in duration dimension
  , leverageTick :: Int   -- Tick position in leverage dimension
  }

type TickData = 
  { coordinate :: TickCoordinate
  , liquidityGross :: Number
  , liquidityNet :: Number
  , feeGrowthOutside :: FeeGrowth
  , initialized :: Boolean
  }
```

## Core Operations

### 1. Swap Operations

#### Multi-Dimensional Swaps

Swaps can occur across any combination of dimensions:

```purescript
swap :: SwapParams -> Pool -> Effect SwapResult
```

**Swap Types:**
- **Rate Swap**: Exchange between different interest rates
- **Duration Swap**: Roll positions between maturities
- **Leverage Swap**: Adjust risk exposure
- **Cross-Dimensional**: Simultaneous changes across multiple dimensions

**Swap Execution Flow:**
1. Calculate path-dependent swap amount using the swap solver
2. Check oracle price deviation (if oracle configured)
3. Update virtual balances maintaining invariant
4. Cross ticks and update fee growth
5. Apply swap fees with weighted distribution
6. Emit swap event for indexing

### 2. Liquidity Operations

#### Add Liquidity

```purescript
addLiquidity :: LiquidityParams -> Pool -> Effect AddLiquidityResult
```

**Process:**
1. Validate tick range across all dimensions
2. Calculate liquidity amount for target range
3. Update position or create new one
4. Mint position NFT for new positions
5. Update tick data at boundaries
6. Emit liquidity event

#### Remove Liquidity

```purescript
removeLiquidity :: PositionId -> Number -> Pool -> Effect RemoveLiquidityResult
```

**Process:**
1. Validate position ownership
2. Calculate pro-rata share removal
3. Update tick liquidity
4. Collect accumulated fees
5. Burn position NFT if fully withdrawn
6. Emit removal event

### 3. Fee Management

#### Fee Structure

Fees are collected on swaps and distributed based on:
- **Base Fee Rate**: Configurable per pool (default 0.3%)
- **Dynamic Fees**: Adjusted based on volatility
- **Protocol Share**: Portion allocated to protocol

#### Weighted Fee Distribution

```purescript
distributeSwapFee :: Number -> VirtualBalances -> Pool -> Effect Pool
```

Fees are distributed proportionally to liquidity contribution:
- Active liquidity in swap range receives fees
- Distribution weighted by dimension contribution
- Protocol fees separated for collection

### 4. Position Management

#### Position Structure

```purescript
type Position = 
  { id :: PositionId
  , owner :: Address
  , pool :: PoolId
  , tickRange :: TickRange
  , liquidity :: Number
  , feeGrowthInside :: FeeGrowth
  , tokensOwed :: TokenAmounts
  , nftId :: Maybe Int
  }
```

#### NFT Integration

Each position can be represented as an NFT:
- Enables secondary market trading
- Maintains position metadata
- Tracks ownership transfers

### 5. Tick Management

#### Tick Crossing

```purescript
crossTick :: TickCoordinate -> TickDirection -> Pool -> Effect Pool
```

When price moves across tick boundaries:
1. Update global liquidity based on tick's net liquidity
2. Flip fee growth tracking for the tick
3. Update price and current tick
4. Emit tick cross event

#### Tick Initialization

Ticks are lazily initialized when first used:
- Reduces gas costs
- Maintains sparse tick structure
- Tracks liquidity changes

## Mathematical Foundations

### Price Calculations

#### Square Root Price

The pool uses square root price for precision:
```
sqrtPrice = sqrt(price)
currentPrice = sqrtPrice^2
```

#### Tick to Price Conversion

```purescript
tickToSqrtPrice :: TickCoordinate -> PoolConfig -> Number
tickToSqrtPrice tick config = 
  config.tickBase ^ 
    (toNumber tick.rateTick * config.weights.rateWeight +
     toNumber tick.durationTick * config.weights.durationWeight +  
     toNumber tick.leverageTick * config.weights.leverageWeight)
```

### Invariant Maintenance

The pool maintains its invariant through:

```purescript
maintainInvariant :: VirtualBalances -> PoolConfig -> Number
maintainInvariant balances config =
  pow balances.rateBalance config.weights.rateWeight *
  pow balances.durationBalance config.weights.durationWeight *
  pow balances.leverageBalance config.weights.leverageWeight
```

### Swap Solver

The swap solver calculates optimal swap amounts:

```purescript
calculateSwapAmount :: 
  DimensionType -> 
  Number -> 
  VirtualBalances -> 
  PoolConfig -> 
  Number
```

Uses Newton-Raphson method for finding swap amounts that maintain invariant.

## Risk Management

### Oracle Integration

```purescript
checkOraclePrice :: Number -> Pool -> Effect (Maybe PriceDeviation)
```

- Validates swaps against oracle prices
- Prevents manipulation through price limits
- Configurable maximum deviation (default 10%)

### Rebalancing

```purescript
rebalancePool :: RebalanceParams -> Pool -> Effect Pool
```

Automatic rebalancing when:
- Balance ratios exceed thresholds
- Significant price deviations detected
- Protocol-initiated adjustments

### Slippage Protection

- Maximum slippage per operation
- Path-dependent pricing for large swaps
- Dynamic fee adjustments

## Event System

### Event Types

```purescript
data PoolEvent
  = TickCrossed { tick :: TickCoordinate, direction :: TickDirection }
  | LiquidityAdded { positionId :: String, amount :: Number }
  | LiquidityRemoved { positionId :: String, amount :: Number }
  | SwapExecuted { pool :: String, amountIn :: Number, amountOut :: Number, fee :: Number, sqrtPrice :: Number }
  | FeesCollected { positionId :: String, amount0 :: Number, amount1 :: Number }
  | PoolRebalanced { oldBalances :: VirtualBalances, newBalances :: VirtualBalances, deviation :: Number }
  | PositionNFTMinted { positionId :: PositionId, nftId :: Int, owner :: String }
  | PositionNFTBurned { positionId :: PositionId, nftId :: Int }
  | POLDeployed { amount :: Number, range :: TickRange }
```

### Event Emission

Events are emitted for:
- All state changes
- User interactions
- System operations
- Protocol actions

## Configuration

### Pool Parameters

```purescript
type PoolConfig = 
  { feeRate :: Number                -- Base fee rate (e.g., 0.003 for 0.3%)
  , tickSpacing :: Int               -- Minimum tick spacing
  , maxLiquidityPerTick :: Number    -- Liquidity cap per tick
  , protocolFeeShare :: Number       -- Protocol's fee portion
  , oracleParameters :: OracleConfig -- Oracle configuration
  , rebalanceThreshold :: Number     -- Rebalance trigger threshold
  , maxSlippage :: Number            -- Maximum allowed slippage
  , weights :: DimensionWeights      -- Dimension weight configuration
  }
```

### Dimension Weights

```purescript
type DimensionWeights = 
  { rateWeight :: Number       -- Weight for rate dimension (default: 0.5)
  , durationWeight :: Number   -- Weight for duration dimension (default: 0.3)
  , leverageWeight :: Number   -- Weight for leverage dimension (default: 0.2)
  }
```

## Security Considerations

### Access Control

- Position modifications require ownership verification
- Protocol functions restricted to authorized addresses
- NFT transfers validate ownership

### Invariant Protection

- All operations verify invariant maintenance
- Atomic operations prevent partial state updates
- Reentrancy guards on external calls

### Price Manipulation

- Oracle price checks on significant swaps
- Maximum price impact limits
- Time-weighted average price tracking

## Gas Optimization

### Lazy Initialization

- Ticks initialized only when used
- Positions stored in efficient map structure
- Events batched where possible

### Storage Patterns

- Packed struct storage for tick data
- Efficient balance updates
- Minimal storage writes

## Integration Points

### External Interfaces

1. **Oracle System**: Price validation and TWAP
2. **NFT System**: Position tokenization
3. **POL System**: Protocol-owned liquidity
4. **Vault System**: Structured product integration

### Indexer Support

The pool emits comprehensive events for:
- Real-time state tracking
- Historical analysis
- Performance metrics
- User position tracking

## Future Enhancements

### Planned Features

1. **Dynamic Fee Tiers**: Volatility-based fee adjustments
2. **Concentrated Incentives**: Targeted liquidity rewards
3. **Cross-Pool Routing**: Optimal path finding
4. **Advanced Oracles**: Multi-source price aggregation

### Extensibility

The pool design supports:
- Additional dimensions
- Custom fee models
- Alternative invariants
- Protocol upgrades

## Conclusion

The Pool system provides a sophisticated 3D AMM implementation with:
- Capital efficient concentrated liquidity
- Multi-dimensional trading capabilities
- Comprehensive risk management
- Full event transparency
- Gas-optimized operations

This architecture enables complex financial products while maintaining security, efficiency, and composability within the Feels Protocol ecosystem.
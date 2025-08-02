# Feels Protocol MVP: Unified Tick-Based Market

## Overview

The Feels Protocol MVP implements a unified tick-based market system where all three monetary functions (exchange, lending, and redenomination) are parameters of a single position type. By treating duration and leverage as optional dimensions alongside price ticks, the protocol creates a seamless continuum across spot trading, lending, and leveraged positions.

## System Architecture

### Three-Layer Design

```
┌─────────────────────────────────────────────────┐
│                  User jitoSOL                   │
└─────────────────────────────────────────────────┘
                         ↓
┌─────────────────────────────────────────────────┐
│             Deposit Component                   │
│  • Accepts jitoSOL deposits                     │
│  • Tracks SOL/jitoSOL exchange rate             │
│  • Mints/burns synthetic SOL                    │
└─────────────────────────────────────────────────┘
                         ↓
┌─────────────────────────────────────────────────┐
│        Unified Tick Market                      │
│  • Single position type with 3 parameters       │
│  • Price tick + Duration + Leverage             │
│  • Synthetic SOL ↔ Feels asset pairs            │
│  • Duration > 0 creates feels assets            │
└─────────────────────────────────────────────────┘
```

## Deposit Component

### Purpose
Isolates SOL/jitoSOL exchange rate risk from the core market mechanics while allowing users to maintain exposure to jitoSOL staking yields.

### Mechanics
- **Entry**: `jitoSOL deposited × (SOL/jitoSOL rate) = synthetic SOL minted`
- **Exit**: `synthetic SOL burned / (SOL/jitoSOL rate) = jitoSOL returned`
- **Rate Tracking**: Oracle-based or time-weighted average price (TWAP)
- **Buffering**: Maintains jitoSOL reserves for withdrawals

### Benefits
- Users earn jitoSOL staking rewards throughout participation
- Clean separation between deposit risk and market risk
- Synthetic SOL provides stable internal unit of account

## Unified Tick Market

### Core Concept
Every position is a tick with three orthogonal parameters that activate different monetary functions:
- **tickRate**: Activates exchange function (liquidity at price level)
- **duration**: Activates lending function (0 = spot, >0 = time-locked)
- **leverageRate**: Activates redenomination function (0 = no leverage, >0 = leveraged)

### Unified Position Type
```purescript
-- Unified position parameters
type UnifiedTick =
  { amount :: Number          -- Size of position
  , pair :: TokenType         -- Must be Synthetic SOL ↔ Feels Asset
  , tickRate :: Number        -- Price level (exchange function)
  , duration :: Int           -- 0 = spot, >0 = lending (lending function)
  , leverageRate :: Number    -- 0 = no leverage, >0 = leveraged (redenomination function)
  }
```

### Functional Activation
- **Pure Exchange**: `{duration: 0, leverageRate: 0}` - Simple spot liquidity
- **Pure Lending**: `{duration: 30, leverageRate: 0, tickRate: market}` - Time-locked position
- **Pure Leverage**: `{duration: 0, leverageRate: 0.5, tickRate: market}` - Leveraged exposure
- **Edge Combinations**: Any two parameters active create edge positions
- **Center Position**: All three parameters active create the complete position

### Mechanics
- **Price Discovery**: Orders aggregate at discrete price levels
- **Execution**: Ticks with duration = 0 can be filled immediately
- **Time Value**: Duration > 0 earns additional fees and locks liquidity
- **Leverage**: Amplifies exposure to price movements at the tick
- **Asset Creation**: Only ticks with duration > 0 using Synthetic SOL can mint feels assets

## Position Examples

### Pure Positions (Single Function Active)

#### 1. Pure Exchange (Spot Liquidity)
```purescript
{ amount: 100.0
, pair: TokenA           -- Paired with Synthetic SOL
, tickRate: 0.98         -- Specific price level
, duration: 0            -- No time lock
, leverageRate: 0.0      -- No leverage
}
```
- **Function**: Simple spot market order
- **Risk**: Liquidity risk only
- **Fee**: Base exchange fees

#### 2. Pure Lending (Time-Locked)
```purescript
{ amount: 100.0
, pair: TokenB           -- Paired with Synthetic SOL
, tickRate: 1.0          -- At market rate
, duration: 30           -- 30-day lock
, leverageRate: 0.0      -- No leverage
}
```
- **Function**: Creates feels assets, earns time premium
- **Risk**: Credit risk only
- **Fee**: Base lending fees
- **Creates**: 75 TokenB (at 75% collateral ratio)

#### 3. Pure Leverage (Redenominated)
```purescript
{ amount: 100.0
, pair: TokenA           -- Paired with Synthetic SOL
, tickRate: 1.0          -- At market rate
, duration: 0            -- No time lock
, leverageRate: 0.5      -- 1.5x total exposure
}
```
- **Function**: Leveraged price exposure
- **Risk**: Price risk only
- **Fee**: Base leverage fees

### Edge Positions (Two Functions Active)

#### 4. Staked Liquidity (Exchange + Lending)
```purescript
{ amount: 100.0
, pair: TokenA           -- Paired with Synthetic SOL
, tickRate: 1.02         -- Above market
, duration: 60           -- 60-day lock
, leverageRate: 0.0      -- No leverage
}
```
- **Function**: Time-locked liquidity provision
- **Risk**: Liquidity + Credit risk
- **Fee**: Weighted average of exchange and lending fees

#### 5. Leveraged Lending (Lending + Leverage)
```purescript
{ amount: 100.0
, pair: TokenB           -- Paired with Synthetic SOL
, tickRate: 1.0          -- At market
, duration: 90           -- 90-day lock
, leverageRate: 1.0      -- 2x total exposure
}
```
- **Function**: Leveraged time-locked position
- **Risk**: Credit + Price risk
- **Fee**: Weighted average of lending and leverage fees

### Center Position (All Functions Active)

#### 6. Complete Position
```purescript
{ amount: 100.0
, pair: TokenA           -- Paired with Synthetic SOL
, tickRate: 0.95         -- Below market
, duration: 30           -- 30-day lock
, leverageRate: 0.5      -- 1.5x exposure
}
```
- **Function**: All three monetary functions combined
- **Risk**: Liquidity + Credit + Price risk
- **Fee**: `(exchange_fee + lending_fee + leverage_fee) / 3 × 1.2`
- **Complexity Premium**: 20% additional fee for maximum complexity

## Asset Hierarchy

```
External Layer:    jitoSOL (user deposits)
        ↓
Base Layer:        Synthetic SOL (internal currency)
        ↓
Feels Token Layer:       TokenA, TokenB, PositionA, PositionB (created via ticks) 
```

### Asset Rules
1. **Entry/Exit**: All users enter with jitoSOL, exit with jitoSOL
2. **Internal Operations**: All use Synthetic SOL as base currency
3. **Feels Creation**: Only locked Synthetic SOL ticks create feels assets
4. **Pairing Rule**: All operations must be Synthetic SOL ↔ Feels pairs
5. **No Cross-Feels**: No direct TokenA ↔ TokenB operations

## Fee Structure

### Unified Fee Model
Fees are calculated based on which parameters are active, with each function contributing to the total:

```purescript
-- Calculate unified fee based on active parameters
calculateFee :: UnifiedTick -> SystemParams -> Number
calculateFee params sysParams =
  let baseFee = 0.001
      exchangeMult = if params.tickRate /= 1.0 then 1.0 else 0.0
      lendingMult = if params.duration > 0 
                    then toNumber params.duration / 180.0 
                    else 0.0
      leverageMult = params.leverageRate
      complexityMult = case countActiveParams params of
        1 -> 1.0   -- Single function
        2 -> 1.1   -- Edge position
        _ -> 1.2   -- Center position
  in baseFee * 
     (1.0 + exchangeMult) *
     (1.0 + lendingMult) *
     (1.0 + leverageMult) *
     complexityMult

-- Count active parameters
countActiveParams :: UnifiedTick -> Int
countActiveParams params =
  (if params.tickRate /= 1.0 then 1 else 0) +
  (if params.duration > 0 then 1 else 0) +
  (if params.leverageRate > 0.0 then 1 else 0)
```

Where `complexityMultiplier`:
- 1.0 for single function (one parameter active)
- 1.1 for edge positions (two parameters active)
- 1.2 for center position (all three parameters active)

### Fee Components

| Active Parameters | Risk Types | Fee Calculation |
|------------------|------------|-----------------|
| tickRate only | Liquidity Risk | `base × (1 + 1/tick_depth) × utilization` |
| duration only | Credit Risk | `base × (1 + duration/180) × (1/total_staked)` |
| leverageRate only | Price Risk | `base × (1 + volatility) × leverage_ratio` |
| Two parameters | Combined Risks | Weighted average of components |
| All three | All Risks | Average of all three × 1.2 |

### Fee Flow
1. **Deposit/Withdraw**: Minimal fee (covers gas + oracle costs)
2. **Position Operations**: Dynamic fees based on active parameters
3. **Fee Distribution**: To protocol treasury and/or liquidity providers

## Risk Model

### Risk Types
1. **Deposit Risk**: SOL/jitoSOL rate changes (isolated to entry/exit)
2. **Liquidity Risk**: Orders may be filled at unfavorable times
3. **Credit Risk**: Time-locked positions have counterparty risk
4. **Price Risk**: Leveraged positions amplify price movements
5. **Complexity Risk**: Combined positions have emergent risks

### Risk Mitigation
- **Deposit Buffer**: Maintains jitoSOL reserves
- **No Liquidations**: Positions cannot be forcibly closed
- **Bounded Loss**: Maximum loss is initial deposit
- **Over-collateralization**: 75% ratio provides safety margin

## Example User Flows

### Conservative Yield Seeker
```purescript
-- 1. Deposit 100 jitoSOL → receive 102 synthetic SOL
deposit 100.0 depositState

-- 2. Create position
let position = 
      { amount: 102.0
      , pair: TokenA
      , tickRate: 1.05
      , duration: 30       -- Pure lending
      , leverageRate: 0.0
      }
      
-- 3. Receive: 76.5 TokenA (created at 75% collateral)
createPosition syntheticSOL position systemParams

-- 4. Earn: jitoSOL staking yield + duration fees
-- 5. After 30 days: withdraw → receive jitoSOL back + profits
withdraw syntheticAmount depositState
```

### Active Trader
```purescript
-- 1. Deposit 100 jitoSOL → receive 102 synthetic SOL
deposit 100.0 depositState

-- 2. Create two positions:
-- a) Spot liquidity:
let spotPosition = 
      { amount: 50.0
      , pair: TokenB
      , tickRate: 0.98
      , duration: 0        -- Pure exchange
      , leverageRate: 0.0
      }
      
-- b) Leveraged position:
let leveragedPosition = 
      { amount: 52.0
      , pair: TokenB
      , tickRate: 1.0
      , duration: 0
      , leverageRate: 1.0  -- Pure leverage (2x exposure)
      }
      
createPosition syntheticSOL spotPosition systemParams
createPosition syntheticSOL leveragedPosition systemParams

-- 3. Manage positions based on market movements
-- 4. Exit when profitable → receive jitoSOL
```

### Yield Farmer Maximalist
```purescript
-- 1. Deposit 100 jitoSOL → receive 102 synthetic SOL
deposit 100.0 depositState

-- 2. Create center position:
let centerPosition = 
      { amount: 102.0
      , pair: TokenA
      , tickRate: 0.95     -- Below market
      , duration: 90       -- Long lock
      , leverageRate: 0.5  -- 1.5x exposure
      }
      
-- 3. All three functions active = maximum fees + complexity premium
createPosition syntheticSOL centerPosition systemParams

-- 4. Compound rewards over time
-- 5. Unwind → exit to jitoSOL
```

## Technical Implementation

### Smart Contract Architecture
1. **DepositVault**: Handles jitoSOL ↔ synthetic SOL conversions (implemented in Deposit.purs)
2. **Oracle**: SOL/jitoSOL price feed + volatility for leverage calculations (implemented in FFI.js)
3. **UnifiedTickMarket**: Single market with three-parameter positions (implemented in Pool.purs)
4. **PositionManager**: Creates and manages unified tick positions (implemented in Position.purs with leverage functions)
5. **FeeCalculator**: Dynamic fee computation based on active parameters (implemented in Risk.purs)
6. **ValidationSystem**: Input validation and formatting utilities (implemented in Utils.purs)

### Key Invariants
- Total synthetic SOL ≤ Total jitoSOL deposited × exchange rate
- Total feels assets ≤ Total locked synthetic SOL × effective_collateral_ratio
- Effective collateral ratio = 0.75 × (1 / (1 + leverageRate))
- All positions have non-negative value
- Fee revenue is strictly positive
- Leverage does not create systemic risk (no liquidations)

## Benefits of Unified Design

1. **Conceptual Elegance**: Single position type with three orthogonal parameters
2. **Capital Efficiency**: One liquidity pool serves all functions (exchange, lending, leverage)
3. **Natural Pricing**: Each parameter contributes independently to fees
4. **True Composability**: Parameters combine mathematically without special cases
5. **User Experience**: One interface, toggle parameters to access any function
6. **Risk Clarity**: Each parameter maps to exactly one risk type
7. **No Liquidations**: Leverage is bounded by collateral, positions cannot be forcibly closed

## Future Enhancements

1. **Multi-asset Support**: Beyond SOL/jitoSOL
2. **Dynamic Collateral Ratios**: Based on market conditions
3. **Governance**: Parameter adjustment via DAO
4. **Cross-chain**: Bridge to other ecosystems
5. **Advanced Strategies**: Automated position management

## Mathematical Properties

The unified tick system exhibits several elegant properties:

1. **Parameter Independence**: Each parameter affects exactly one monetary function
2. **Risk Orthogonality**: `tickRate` → liquidity risk, `duration` → credit risk, `leverageRate` → price risk
3. **Fee Additivity**: Total fee is a product of individual parameter contributions
4. **Collateral Scaling**: Leverage inversely affects effective collateral ratio
5. **Bounded Loss**: Maximum loss = initial deposit (no liquidations possible)

## Conclusion

The Feels Protocol MVP achieves true unification by reducing all financial positions to a single type with three parameters. This design perfectly maps the theoretical triangle model (exchange, lending, redenomination) to practical implementation parameters (tickRate, duration, leverageRate). The addition of the jitoSOL deposit layer provides baseline staking yields while the unified tick system offers unprecedented composability and capital efficiency. Users can access any combination of DeFi primitives through a single, intuitive interface where complexity emerges from simplicity.
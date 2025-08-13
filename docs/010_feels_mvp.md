# Feels Protocol MVP: Tick-Based Market

## Overview

The Feels Protocol MVP implements a tick-based market system where the three core monetary functions (exchange, lending, and leverage) are expressed through orthogonal position parameters. Each position is fully defined by `price` (the tick level), `duration` (time commitment in blocks), and `leverage` (exposure multiplier). This creates a three-dimensional continuum where any combination of spot trading, time-locked lending, and leveraged exposure can be expressed as a single position type.

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
│              Tick Market                        │
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

## Tick Market

### Core Concept
Every position is defined by three orthogonal parameters that activate different monetary functions:
- **Price**: The specific price level for liquidity provision, activating the exchange function.
- **Duration**: The time commitment for the position, activating the lending function (0 = spot/perpetual, >0 = time-locked).
- **Leverage**: The exposure multiplier, activating the leverage function (1.0 = base exposure, >1.0 = amplified exposure).

### Position Type
```purescript
-- Position with three orthogonal parameters
type Position =
  { id :: PositionId               -- Unique identifier for the position
  , owner :: String                -- Owner of the position
  , amount :: Number               -- Initial capital invested
  , price :: Number                -- Price level for liquidity (tick rate)
  , duration :: Number             -- Time lock in blocks (0 = spot)
  , leverage :: Number             -- Exposure multiplier (1.0 = base)
  , rollover :: Boolean            -- Whether position auto-rolls to next term
  , shares :: ShareAmount          -- Share-based accounting within pool
  , createdAt :: BlockNumber       -- Creation block number
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount locked below floor
  }
```

### Functional Activation
- **Pure Exchange**: `{price: 1.25, duration: 0, leverage: 1.0}` - Spot liquidity at specific price.
- **Pure Lending**: `{price: 1.0, duration: 40320, leverage: 1.0}` - 28-day time lock at par.
- **Pure Leverage**: `{price: 1.0, duration: 0, leverage: 3.0}` - 3x leveraged exposure at par.
- **Edge Combinations**: Any two parameters active create hybrid positions.
- **Complete Position**: All three parameters active create maximum complexity.

### Mechanics
- **Price Discovery**: Positions specify exact price levels for liquidity provision.
- **Execution**: Positions with `duration: 0` can be filled immediately.
- **Time Value**: `duration > 0` earns additional fees proportional to lock time.
- **Exposure**: `leverage > 1.0` amplifies both gains and losses.
- **Asset Creation**: Only positions with `duration > 0` can create synthetic assets.

## Position Examples

These examples illustrate how different combinations of `price`, `duration`, and `leverage` parameters activate various monetary functions. The examples show simplified position data with key parameters.

### Pure Positions (Single Function Active)

#### 1. Pure Exchange (Spot Liquidity)
```purescript
{ amount: 100.0
, price: 1.25        -- Liquidity at 1.25 FeelsSOL per token
, duration: 0        -- No time lock (spot)
, leverage: 1.0      -- Base exposure
}
```
- **Function**: Provides liquidity at a specific price point.
- **Risk**: Impermanent loss from price movements.
- **Fee**: Base exchange fee (~0.3%).

#### 2. Pure Lending (Time-Locked)
```purescript
{ amount: 100.0
, price: 1.0         -- At par (no price risk)
, duration: 40320    -- 28-day lock (40320 blocks)
, leverage: 1.0      -- Base exposure
}
```
- **Function**: Time-locked liquidity earning duration premium.
- **Risk**: Opportunity cost from locked capital.
- **Fee**: Duration-based fee (~1% for 28 days).
- **Creates**: Synthetic assets at 75% collateral ratio.

#### 3. Pure Leverage
```purescript
{ amount: 100.0
, price: 1.0         -- At current market price
, duration: 0        -- No time lock
, leverage: 3.0      -- 3x exposure
}
```
- **Function**: Amplified price exposure without liquidation risk.
- **Risk**: Magnified gains and losses (capped at 100%).
- **Fee**: Leverage fee proportional to multiplier (~0.5% * leverage).

### Edge Positions (Two Functions Active)

#### 4. Staked Liquidity (Exchange + Lending)
```purescript
{ amount: 100.0
, price: 1.25        -- Liquidity above market
, duration: 40320    -- 28-day lock
, leverage: 1.0      -- Base exposure
}
```
- **Function**: Time-locked liquidity provision at specific price.
- **Risk**: Impermanent loss + opportunity cost.
- **Fee**: Combined exchange + duration fees (~1.3%).

#### 5. Leveraged Lending (Duration + Leverage)
```purescript
{ amount: 100.0
, price: 1.0         -- At par
, duration: 40320    -- 28-day lock
, leverage: 3.0      -- 3x exposure
}
```
- **Function**: Amplified returns on time-locked capital.
- **Risk**: Magnified opportunity cost and price risk.
- **Fee**: Duration fee + leverage fee (~2.5%).

### Complete Position (All Functions Active)

#### 6. Complete Position (All Parameters Active)
```purescript
{ amount: 100.0
, price: 1.25        -- Above market liquidity
, duration: 40320    -- 28-day lock
, leverage: 3.0      -- 3x exposure
}
```
- **Function**: Maximum capital efficiency with all three functions.
- **Risk**: Compound risks from all parameters.
- **Fee**: Multiplicative fees (~3.9% = 0.3% * 1.3 * 3.0).
- **Complexity Premium**: Additional fee for multi-parameter positions.

## Asset Hierarchy

```
External Layer:    jitoSOL (user deposits)
        ↓
Base Layer:        FeelsSOL (internal currency, synthetic SOL)
        ↓
Feels Token Layer:       User-Created Tokens (e.g., TokenA, TokenB, created via offerings)
```

### Asset Rules
1. **Entry/Exit**: All users enter with jitoSOL, exit with jitoSOL
2. **Internal Operations**: All use FeelsSOL as base currency
3. **Feels Creation**: Only positions with `duration > 0` can create synthetic assets.
4. **Pairing Rule**: All operations must be FeelsSOL ↔ User-Created Token pairs
5. **No Cross-Feels**: No direct User-Created Token ↔ User-Created Token operations

## Fee Structure

### Fee Model
Fees are calculated based on which parameters are active, with each function contributing to the total:

```purescript
-- Calculate fee based on active parameters
calculateFee :: TickParams -> SystemParams -> Number
calculateFee params sysParams =
  let baseFee = 0.001
      exchangeMult = if params.price /= 1.0 then 1.0 else 0.0
      lendingMult = if params.duration > 0 
                    then toNumber params.duration / 180.0 
                    else 0.0
      leverageMult = params.leverage
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
countActiveParams :: TickParams -> Int
countActiveParams params =
  (if params.price /= 1.0 then 1 else 0) +
  (if params.duration > 0 then 1 else 0) +
  (if params.leverage > 1.0 then 1 else 0)
```

Where `complexityMultiplier`:
- 1.0 for single function (one parameter active)
- 1.1 for edge positions (two parameters active)
- 1.2 for center position (all three parameters active)

### Fee Components

| Active Parameters | Risk Types | Fee Calculation |
|------------------|------------|-----------------|
| price only | Liquidity Risk | `base × (1 + |price - 1.0|) × utilization` |
| duration only | Credit Risk | `base × (1 + duration/180) × (1/total_staked)` |
| leverage only | Price Risk | `base × (1 + volatility) × leverage` |
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
- **Over-collateralization**: Provides safety margin

## Example User Flows

These examples illustrate user interactions with the protocol using the actual UI action functions and `Position` parameters.

### Conservative Yield Seeker
```purescript
-- Assuming 'user' and 'protocolState' are available in scope

-- 1. Deposit 100 jitoSOL → receive FeelsSOL
_ <- enterFeelsSOL user 100.0 protocolState

-- 2. Create a Senior, Monthly position (Pure Lending)
_ <- createPosition
       user
       FeelsSOL          -- lend asset
       100.0             -- amount
       JitoSOL           -- collateral asset (simplified)
       150.0             -- collateral amount (simplified, 1.5x)
       (monthlyTerm protocolState.currentBlock) -- term: Monthly
       false             -- rollover
       Nothing           -- target token for staking
       protocolState

-- 3. Earn: jitoSOL staking yield + duration fees (conceptual)
-- 4. After expiry: withdraw → receive jitoSOL back + profits
_ <- exitFeelsSOL user 100.0 protocolState -- Simplified: burning 100 FeelsSOL
```

### Active Trader
```purescript
-- Assuming 'user' and 'protocolState' are available in scope

-- 1. Deposit 100 jitoSOL → receive FeelsSOL
_ <- enterFeelsSOL user 100.0 protocolState

-- 2. Create two positions:
-- a) Spot liquidity (Senior, Spot):
_ <- createPosition
       user
       FeelsSOL
       50.0
       JitoSOL
       75.0
       spotTerm          -- term: Spot
       false
       Nothing
       protocolState

-- b) Leveraged position (Junior, Spot):
_ <- createPosition
       user
       FeelsSOL
       52.0
       JitoSOL
       78.0
       spotTerm          -- term: Spot
       false
       Nothing
       protocolState

-- 3. Manage positions based on market movements (conceptual)
-- 4. Exit when profitable → receive jitoSOL
_ <- exitFeelsSOL user 100.0 protocolState -- Simplified
```

### Yield Farmer Maximalist
```purescript
-- Assuming 'user' and 'protocolState' are available in scope

-- 1. Deposit 100 jitoSOL → receive FeelsSOL
_ <- enterFeelsOL user 100.0 protocolState

-- 2. Create a comprehensive position (Junior, Monthly):
_ <- createPosition
       user
       FeelsSOL
       102.0
       JitoSOL
       153.0
       (monthlyTerm protocolState.currentBlock) -- term: Monthly
       false
       Nothing
       protocolState

-- 3. All three functions active = maximum fees + complexity premium (conceptual)
-- 4. Compound rewards over time (conceptual)
-- 5. Unwind → exit to jitoSOL
_ <- exitFeelsSOL user 100.0 protocolState -- Simplified
```

## Technical Implementation

### Smart Contract Architecture
1. **DepositVault**: Handles jitoSOL ↔ FeelsSOL conversions (implemented in Protocol.FeelsSOL.purs)
2. **Oracle**: SOL/jitoSOL price feed + volatility for risk calculations (implemented in Protocol.Oracle.purs; FFI.js provides JavaScript interop for core math functions and external calls).
3. **TickMarket**: Single market with position parameters (implemented in Protocol.Pool.purs)
4. **PositionManager**: Creates and manages positions (implemented in Protocol.Position.purs)
5. **FeeCalculator**: Dynamic fee computation based on active parameters (planned feature, currently basic fees are in Protocol.Pool.purs and Protocol.FeelsSOL.purs)
6. **ValidationSystem**: Input validation and formatting utilities (validation logic is distributed across relevant modules like Protocol.Token.purs and UI.Action.purs)

### Key Invariants
- Total FeelsSOL ≤ Total jitoSOL deposited × exchange rate
- All positions have non-negative value
- Fee revenue is strictly positive
- Leverage does not create systemic risk (no liquidations)

## Benefits of the Design

1. **Capital Efficiency**: One liquidity pool serves all functions (exchange, lending, exposure).
2. **Natural Pricing**: (Conceptual - detailed fee model is a planned feature) Each parameter is intended to contribute independently to fees.
3. **Composability**: The three parameters (`price`, `duration`, `leverage`) combine orthogonally.
4. **User Experience**: A single interface allows users to dial in exact exposure across all three dimensions.
5. **Risk Clarity**: Each parameter conceptually maps to a specific risk type.
6. **No Liquidations**: Exposure is bounded by collateral, positions cannot be forcibly closed.

## Mathematical Properties

The tick system exhibits several elegant properties:

1. **Parameter Independence**: Each parameter affects exactly one monetary function
2. **Risk Orthogonality**: `price` → liquidity risk, `duration` → credit risk, `leverage` → price risk
3. **Fee Additivity**: Total fee is a product of individual parameter contributions
4. **Collateral Scaling**: Leverage inversely affects effective collateral ratio
5. **Bounded Loss**: Maximum loss = initial deposit (no liquidations possible)

## Conclusion

The Feels Protocol MVP reduces all financial positions to a single type with three orthogonal parameters. This design maps the theoretical triangle model (exchange, lending, leverage) to practical implementation parameters (price, duration, leverage). The addition of the jitoSOL deposit layer provides baseline staking yields while the tick-based position system offers unprecedented composability and capital efficiency. Users can access any combination of DeFi primitives through a single interface.
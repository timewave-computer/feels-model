# Feels Protocol MVP: Tick-Based Market

## Overview

The Feels Protocol MVP implements a tick-based market system where the three core monetary functions (exchange, lending, and leverage) are expressed through orthogonal position parameters. Each position is fully defined by `price` (the tick level), `duration` (a `Duration` type representing time commitment), and `leverage` (a `Leverage` type representing exposure multiplier). This creates a three-dimensional continuum where any combination of spot trading, time-locked lending, and leveraged exposure can be expressed as a single position type.

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
│              AMM Pool                           │
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

## AMM Pool

### Core Concept
Every position is defined by three orthogonal parameters that activate different monetary functions:
- **Price**: The specific price level for liquidity provision, activating the exchange function.
- **Duration**: A `Duration` type representing the time commitment for the position, activating the lending function.
- **Leverage**: A `Leverage` type representing the exposure multiplier, activating the leverage function.

### Position Type
```purescript
type Position =
  { id :: PositionId
  , owner :: String
  , amount :: Number
  , price :: Number
  , duration :: Duration
  , leverage :: Leverage
  , lendAsset :: TokenType
  , collateralAsset :: TokenType
  , rollover :: Boolean
  , shares :: ShareAmount
  , createdAt :: BlockNumber
  , value :: Number
  , lockedAmount :: Number
  , accumulatedYield :: Number
  , lastYieldClaim :: BlockNumber
  , feeGrowthInside0 :: Number
  , feeGrowthInside1 :: Number
  }
```

### Functional Activation
- **Pure Exchange**: `{price: 1.25, duration: Spot, leverage: Senior}` - Spot liquidity at specific price.
- **Pure Lending**: `{price: 1.0, duration: Monthly, leverage: Senior}` - 28-day time lock at par.
- **Pure Leverage**: `{price: 1.0, duration: Spot, leverage: Junior}` - 3x leveraged exposure at par.
- **Edge Combinations**: Any two parameters active create hybrid positions.
- **Complete Position**: All three parameters active create maximum complexity.

### Mechanics
- **Price Discovery**: Positions specify exact price levels for liquidity provision.
- **Execution**: Positions with `duration: 0` can be filled immediately.
- **Time Value**: `duration > 0` earns additional fees proportional to lock time.
- **Exposure**: `leverage > 1.0` amplifies both gains and losses.
- **Asset Creation**: Only positions with `duration > 0` can create synthetic assets (managed by the Launch system).

## Position Examples

These examples illustrate how different combinations of `price`, `duration`, and `leverage` parameters activate various monetary functions. The examples show simplified position data with key parameters.

### Pure Positions (Single Function Active)

#### 1. Pure Exchange (Spot Liquidity)
```purescript
{ amount: 100.0
, price: 1.25        -- Liquidity at 1.25 FeelsSOL per token
, duration: Spot     -- No time lock (spot)
, leverage: Senior   -- Base exposure
}
```
- **Function**: Provides liquidity at a specific price point.
- **Risk**: Impermanent loss from price movements.
- **Fee**: Base exchange fee (~0.3%).

#### 2. Pure Lending (Time-Locked)
```purescript
{ amount: 100.0
, price: 1.0         -- At par (no price risk)
, duration: Monthly  -- 28-day lock
, leverage: Senior   -- Base exposure
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
, duration: Spot     -- No time lock
, leverage: Junior   -- 3x exposure
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
, duration: Monthly  -- 28-day lock
, leverage: Senior   -- Base exposure
}
```
- **Function**: Time-locked liquidity provision at specific price.
- **Risk**: Impermanent loss + opportunity cost.
- **Fee**: Combined exchange + duration fees (~1.3%).

#### 5. Leveraged Lending (Duration + Leverage)
```purescript
{ amount: 100.0
, price: 1.0         -- At par
, duration: Monthly  -- 28-day lock
, leverage: Junior   -- 3x exposure
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
, duration: Monthly  -- 28-day lock
, leverage: Junior   -- 3x exposure
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
3. **Feels Creation**: Only positions with `duration > 0` can create synthetic assets (managed by the Launch system).
4. **Pairing Rule**: All operations must be FeelsSOL ↔ User-Created Token pairs
5. **No Cross-Feels**: No direct User-Created Token ↔ User-Created Token operations

## Fee Structure

### Fee Model
Fees are calculated based on the position's duration, leverage, and the assets involved, as well as pool volatility and utilization.

```purescript
calculateRiskBasedFee :: 
  Duration ->      -- Position duration (credit risk)
  Leverage ->      -- Position leverage (directional risk)
  TokenType ->     -- Lend asset (liquidity risk)
  Number ->        -- Position amount
  Number ->        -- Pool volatility
  Number ->        -- Pool utilization
  FeeComponents
calculateRiskBasedFee duration leverage lendAsset amount volatility utilization =
  let
    -- 1. Credit Risk Component (Time/Duration Risk)
    baseCreditRisk = case duration of
      Flash -> 0.0       -- No credit risk (same transaction)
      Monthly -> 0.002   -- 20 bps (capital locked 28 days)
      Spot -> 0.0005     -- 5 bps (perpetual but liquid, infinite duration)
    
    -- Utilization amplifies credit risk (harder to get capital back)
    creditRisk = baseCreditRisk * (1.0 + utilization)
    
    -- 2. Directional Risk Component (Leverage/Principal Volatility)
    baseDirectionalRisk = case leverage of
      Senior -> 0.0001   -- 1 bp (1x, minimal)
      Junior -> 0.001    -- 10 bps (3x leverage)
    
    -- Volatility amplifies directional risk
    directionalRisk = baseDirectionalRisk * (1.0 + volatility * 3.0)
    
    -- 3. Liquidity Risk Component (Asset Choice)
    baseLiquidityRisk = case lendAsset of
      FeelsSOL -> 0.0    -- No liquidity risk
      JitoSOL -> 0.0     -- JitoSOL is also liquid
      Token _ -> 0.001   -- 10 bps for illiquid tokens
    
    -- Volatility increases liquidity risk
    liquidityRisk = baseLiquidityRisk * (1.0 + volatility)
    
    -- 4. Total Risk and Protocol Margin
    totalRisk = creditRisk + directionalRisk + liquidityRisk
    protocolMargin = 0.0005  -- 5 bps base margin
    
    -- 5. Final fee calculation
    totalFeeRate = max 0.0002 (totalRisk + protocolMargin)  -- Min 2 bps
    totalFee = amount * totalFeeRate
    
    -- For compatibility with existing FeeComponents type
    baseFee = amount * (baseCreditRisk + baseDirectionalRisk + baseLiquidityRisk + protocolMargin)
    durationMultiplier = totalFee / baseFee  -- Implied multiplier
    
  in { baseFee, durationMultiplier, totalFee }
```

### Fee Components

| Active Parameters | Risk Types | Fee Calculation |
|------------------|------------|-----------------|
| Duration | Credit Risk | `baseCreditRisk × (1 + utilization)` |
| Leverage | Price Risk | `baseDirectionalRisk × (1 + volatility × 3.0)` |
| Lend Asset | Liquidity Risk | `baseLiquidityRisk × (1 + volatility)` |
| Protocol Margin | Protocol Risk | `0.0005` |

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
       100.0             -- amount
       1.0               -- price (at par)
       monthlyDuration   -- duration: Monthly
       Senior            -- leverage: Senior
       FeelsSOL          -- lend asset
       JitoSOL           -- collateral asset (simplified)
       false             -- rollover
       100.0             -- shares (simplified)
       protocolState.currentBlock
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
       50.0
       1.0
       spotDuration      -- duration: Spot
       Senior            -- leverage: Senior
       FeelsSOL
       JitoSOL
       false
       50.0
       protocolState.currentBlock
       protocolState

-- b) Leveraged position (Junior, Spot):
_ <- createPosition
       user
       52.0
       1.0
       spotDuration      -- duration: Spot
       Junior            -- leverage: Junior
       FeelsSOL
       JitoSOL
       false
       156.0             -- 52 * 3 (shares)
       protocolState.currentBlock
       protocolState

-- 3. Manage positions based on market movements (conceptual)
-- 4. Exit when profitable → receive jitoSOL
_ <- exitFeelsSOL user 100.0 protocolState -- Simplified
```

### Yield Farmer Maximalist
```purescript
-- Assuming 'user' and 'protocolState' are available in scope

-- 1. Deposit 100 jitoSOL → receive FeelsSOL
_ <- enterFeelsSOL user 100.0 protocolState

-- 2. Create a comprehensive position (Junior, Monthly):
_ <- createPosition
       user
       102.0
       1.0
       monthlyDuration   -- duration: Monthly
       Junior            -- leverage: Junior
       FeelsSOL
       JitoSOL
       false
       306.0             -- 102 * 3 (shares)
       protocolState.currentBlock
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
3. **AMM Pool**: Single market with position parameters (implemented in Protocol.Pool.purs)
4. **PositionManager**: Creates and manages positions (implemented in Protocol.Position.purs)
5. **FeeCalculator**: Dynamic fee computation based on active parameters (implemented in Protocol.Incentive.purs)
6. **ValidationSystem**: Input validation and formatting utilities (validation logic is distributed across relevant modules like Protocol.Token.purs and UI.Action.purs)

### Key Invariants
- Total FeelsSOL ≤ Total jitoSOL deposited × exchange rate
- All positions have non-negative value
- Fee revenue is strictly positive
- Leverage does not create systemic risk (no liquidations)

## Benefits of the Design

1. **Capital Efficiency**: One liquidity pool serves all functions (exchange, lending, exposure).
2. **Natural Pricing**: (Conceptual - detailed fee model is a planned feature) Each parameter is intended to contribute independently to fees.
3. **Composability**: The three parameters (`price`, `Duration`, `Leverage`) combine orthogonally.
4. **User Experience**: A single interface allows users to dial in exact exposure across all three dimensions.
5. **Risk Clarity**: Each parameter conceptually maps to a specific risk type.
6. **No Liquidations**: Exposure is bounded by collateral, positions cannot be forcibly closed.

## Mathematical Properties

The tick system exhibits the following properties:

1. **Parameter Independence**: Each parameter affects exactly one monetary function
2. **Risk Orthogonality**: `price` → liquidity risk, `Duration` → credit risk, `Leverage` → price risk
3. **Fee Additivity**: Total fee is a product of individual parameter contributions
4. **Collateral Scaling**: Leverage inversely affects effective collateral ratio
5. **Bounded Loss**: Maximum loss = initial deposit (no liquidations possible)

## Conclusion

The Feels Protocol MVP reduces all financial positions to a single type with three orthogonal parameters. This design maps the theoretical triangle model (exchange, lending, leverage) to practical implementation parameters (price, duration, leverage). The addition of the jitoSOL deposit layer provides baseline staking yields while the tick-based position system offers composability and capital efficiency. Users can access any combination of DeFi primitives through a single interface.
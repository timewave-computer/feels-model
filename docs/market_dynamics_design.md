# Unified Market Dynamics Design

## Overview

The MarketDynamics module unifies fees and returns into a single adaptive system that treats both as different perspectives on the same underlying market dynamics. This design replaces the separate Fees.purs and Returns.purs modules with a cohesive framework.

## Core Concept: The Spread Model

The fundamental insight is that fees and returns are two sides of the same equation:

```
Borrower Rate = Lender Rate + Spread
NFV Flow = Spread × NFV Allocation Rate
```

This creates a self-balancing system where:
- **Lenders** receive the base rate (their return)
- **Borrowers** pay the base rate plus spread (their fee)
- **NFV** captures the spread (protocol revenue)

## Key Design Principles

### 1. Unified Rate Calculation

Instead of calculating fees and returns separately, we calculate a single base rate that is then adjusted:

```purescript
type DynamicsResult =
  { baseRate :: Number         -- Core rate for the operation
  , effectiveSpread :: Spread  -- Spread after all adjustments
  , lenderRate :: Number       -- What lenders receive
  , borrowerRate :: Number     -- What borrowers pay
  , nfvFlow :: Number         -- What flows to NFV
  }
```

### 2. Operation-Specific Dynamics

Each operation type has its own base rate and spread:

| Operation | Base Rate | Spread | Characteristics |
|-----------|-----------|--------|-----------------|
| Swap | 2% | 0.5% | Low risk, high volume |
| Staking | 5% | 2% | Duration risk, stable returns |
| Leverage | 8% | 3% | High risk, volatile returns |
| Gateway | 3% | 1% | Cross-chain risk, moderate volume |

### 3. Adaptive Behavior

The system adapts to market conditions through multiple multipliers:

```purescript
type RateMultipliers =
  { termsMultiplier :: Number    -- Based on specific terms (duration, leverage)
  , marketMultiplier :: Number   -- Based on market conditions
  , riskMultiplier :: Number     -- Based on risk metrics
  , totalMultiplier :: Number    -- Combined effect
  }
```

### 4. Market Metrics Integration

The system responds to real-time market conditions:

```purescript
type MarketMetrics =
  { volatility :: Number        -- Increases rates during volatile periods
  , utilizationRate :: Number   -- Low utilization → lower rates
  , liquidityDepth :: Number    -- Low liquidity → higher rates
  , systemHealthScore :: Number -- Poor health → higher rates
  , crossRiskFactor :: Number   -- Risk amplification across types
  }
```

## Mathematical Relationships

### Base Rate Calculation

For each operation type, the effective base rate is:

```
Effective Base Rate = Base Rate × Terms Multiplier × Market Multiplier × Risk Multiplier
```

### Spread Calculation

The effective spread follows the same pattern:

```
Effective Spread = Base Spread × Terms Multiplier × Market Multiplier × Risk Multiplier
```

### Component Rates

From these, we derive:

```
Lender Rate = Effective Base Rate
Borrower Rate = Effective Base Rate + Effective Spread
NFV Flow Rate = Effective Spread × NFV Allocation Rate
```

## Examples

### Example 1: Simple Swap

```
Conditions:
- Operation: Swap
- Amount: 1000 SOL
- Market: Normal (all multipliers = 1.0)

Calculation:
- Base Rate: 2%
- Spread: 0.5%
- Lender receives: 2% annual (20 SOL/year)
- Borrower pays: 2.5% annual (25 SOL/year)
- NFV receives: 0.5% annual (5 SOL/year)
```

### Example 2: High-Risk Staking

```
Conditions:
- Operation: 90-day staking
- Market: High volatility (1.5x), low health (1.3x)
- Terms multiplier: 1.3x (90 days)

Calculation:
- Base Rate: 5% × 1.3 × 1.5 × 1.3 = 12.675%
- Spread: 2% × 1.3 × 1.5 × 1.3 = 5.07%
- Lender receives: 12.675% annual
- Borrower pays: 17.745% annual
- NFV receives: 5.07% annual
```

### Example 3: Leveraged Position

```
Conditions:
- Operation: 5x leverage
- Market: Moderate volatility, high cross-risk
- Terms multiplier: 2.24x (sqrt(5))

Calculation:
- Base Rate: 8% × 2.24 × risk factors = ~20%
- Spread: 3% × 2.24 × risk factors = ~7.5%
- Lender receives: 20% annual
- Borrower pays: 27.5% annual
- NFV receives: 7.5% annual
```

## Adaptive Mechanisms

### 1. Volatility Response

Higher volatility increases both base rates and spreads:

```purescript
volatilityFactor = 1.0 + (metrics.volatility * config.volatilityMultiplier)
```

### 2. Utilization-Based Pricing

The system incentivizes balanced utilization:

```purescript
utilizationFactor = 
  if utilization < 0.3 then 0.8      -- Discount to attract users
  else if utilization > 0.8 then 1.2  -- Premium for scarcity
  else 1.0                            -- Normal rate
```

### 3. Health-Based Adjustment

System health inversely affects rates:

```purescript
healthFactor = 2.0 - metrics.systemHealthScore
```

## NFV Flow Mechanics

The spread between borrower and lender rates flows to NFV:

1. **Collection**: Borrowers pay the full rate (base + spread)
2. **Distribution**: Lenders receive the base rate
3. **Accumulation**: NFV captures the spread
4. **Reinvestment**: NFV provides liquidity as lender of last resort

This creates a virtuous cycle where:
- Higher activity → More NFV growth
- More NFV → Better liquidity backstop
- Better backstop → Lower systemic risk
- Lower risk → More attractive rates

## Integration Points

### With LendingBook

```purescript
-- When creating a position
dynamics <- calculateDynamics marketDynamics lendingRecord
let borrowerCost = getBorrowerRate dynamics
    lenderReturn = getLenderRate dynamics
    nfvContribution = getNFVFlow dynamics
```

### With NFV Module

```purescript
-- When processing fees
let nfvAmount = principal * dynamics.nfvFlow * timeElapsed
contributeToNFV nfvState opType nfvAmount (Just positionId)
```

### With Risk Assessment

```purescript
-- Update market metrics based on risk
let newMetrics = calculateRiskMetrics systemState
updateMarketMetrics marketDynamics newMetrics
```

## Benefits of Unification

1. **Conceptual Clarity**: Fees and returns are clearly related through the spread
2. **Balanced Incentives**: Both sides of the market are considered together
3. **Sustainable Revenue**: NFV growth is directly tied to market activity
4. **Adaptive Pricing**: Single system responds coherently to market conditions
5. **Reduced Complexity**: One module instead of two, with clear relationships

## Migration Path

To migrate from separate Fees/Returns modules:

1. Replace fee calculations with `getBorrowerRate`
2. Replace return calculations with `getLenderRate`
3. Route spread amounts to NFV instead of separate fee handling
4. Update UI to show both sides of the market
5. Remove old modules once migration is complete

## Future Enhancements

1. **Cross-Operation Dynamics**: Rates in one operation type affect others
2. **Time-Based Adjustments**: Rates change based on time of day/week
3. **User-Specific Rates**: Loyalty or volume-based rate adjustments
4. **Predictive Adjustments**: Use ML to anticipate market conditions
5. **Multi-Asset Dynamics**: Different rates for different asset pairs
# Unified Incentives Design

## Overview

The Incentives module unifies fees and returns into a single adaptive system that treats both as different perspectives on the same underlying market dynamics. This design replaces the separate Fees.purs and Returns.purs modules with a cohesive framework.

## Core Concept: The Spread Model

The fundamental insight is that fees and returns are two sides of the same equation:

$$\text{Borrower Rate} = \text{Lender Rate} + \text{Spread}$$
$$\text{POL Flow} = \text{Spread} \times \text{POL Allocation Rate}$$

This creates a self-balancing system where:
- Lenders receive the base rate (their return)
- Borrowers pay the base rate plus spread (their fee)
- POL captures the spread (protocol revenue)

## Key Design Principles

### 1. Unified Rate Calculation

Instead of calculating fees and returns separately, we calculate a single base rate that is then adjusted:

```purescript
type DynamicsResult =
  { baseRate :: Number         -- Core rate for the operation
  , effectiveSpread :: Spread  -- Spread after all adjustments
  , lenderRate :: Number       -- What lenders receive
  , borrowerRate :: Number     -- What borrowers pay
  , polFlow :: Number         -- What flows to POL
  }
```

### 2. Operation-Specific Dynamics

Each operation type has its own base rate and spread:

| Operation | Base Rate | Spread | Characteristics |
|-----------|-----------|--------|-----------------|
| Swap | 2% | 1% | Low risk, high volume |
| Staking | 4% | 1-5% | Duration risk, stable returns |
| Leverage | 6% | 1-5% | High risk, volatile returns |

### 3. Adaptive Behavior

The system adapts to market conditions through multiple multipliers:

```purescript
type RateComponents =
  { baseRate :: BaseRate            -- Fundamental lending rate
  , riskPremium :: Number           -- Additional rate for risk
  , marketAdjustment :: Number      -- Dynamic market-based adjustment
  , protocolSpread :: Spread        -- Protocol revenue spread
  }
```

### 4. Market Metrics Integration

The system responds to real-time market conditions:

```purescript
type MarketMetrics =
  { volatility :: Number        -- Increases rates during volatile periods
  , utilizationRate :: Number   -- Low utilization → lower rates
  , liquidityDepth :: Number    -- Low liquidity → higher rates
  , polGrowthRate :: Number     -- POL growth affects system health
  , marketEfficiency :: Number  -- Market efficiency affects spreads
  }
```

## Mathematical Relationships

### Base Rate Calculation

For each operation type, the effective base rate is:

$$\text{Effective Base Rate} = \text{Base Rate} + \text{Risk Premium} + \text{Market Adjustment}$$

### Spread Calculation

The effective spread is dynamically calculated based on market conditions:

$$\text{Effective Spread} = \text{Target Spread} \times \text{Volatility Multiplier} \times (1 - \text{Efficiency Discount}) \times (1 + \text{Liquidity Adjustment})$$

### Component Rates

From these, we derive:

$$\text{Lender Rate} = \text{Effective Base Rate}$$
$$\text{Borrower Rate} = \text{Effective Base Rate} + \text{Effective Spread}$$
$$\text{POL Flow Rate} = \text{Effective Spread} \times \text{POL Allocation Rate}$$

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
- POL receives: 0.5% annual (5 SOL/year)
```

### Example 2: High-Risk Staking

```
Conditions:
- Operation: 90-day staking
- Market: High volatility, moderate utilization
- Base rate: 4% + duration adjustment

Calculation:
- Base Rate: 4% × 1.16 (90 days) = 4.64%
- Risk Premium: 0.49% (90/365 × 2%)
- Market Adjustment: 0.5% (volatility)
- Lender receives: 5.63% annual
- Spread: 1.5% (volatile market)
- Borrower pays: 7.13% annual
- POL receives: 0.375% (25% of spread)
```

### Example 3: Leveraged Position

```
Conditions:
- Operation: 3x leverage
- Market: High volatility
- Base rate: 6% + leverage adjustment

Calculation:
- Base Rate: 6% × 1.4 (3x leverage) = 8.4%
- Risk Premium: 4% (quadratic risk)
- Market Adjustment: 1.2% (volatility + cross-risk)
- Lender receives: 13.6% annual
- Borrower pays: 16.1% annual
- Spread: 2.5% (high risk)
- POL receives: 0.75% (30% of spread)
```

## Adaptive Mechanisms

### 1. Volatility Response

Higher volatility increases both base rates and spreads:

```purescript
volatilityFactor = 1.0 + (metrics.volatility * config.volatilityMultiplier)
```

$$\text{volatilityFactor} = 1.0 + (\text{metrics.volatility} \times \text{config.volatilityMultiplier})$$

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

$$\text{healthFactor} = 2.0 - \text{metrics.systemHealthScore}$$

## POL Flow Mechanics

The spread between borrower and lender rates flows to POL:

1. **Collection**: Borrowers pay the full rate (base + spread)
2. **Distribution**: Lenders receive the base rate
3. **Accumulation**: POL captures the spread
4. **Reinvestment**: POL provides liquidity as lender of last resort

This creates a virtuous cycle where:
- Higher activity → More POL growth
- More POL → Better liquidity backstop
- Better backstop → Lower systemic risk
- Lower risk → More attractive rates

## Integration Points

### With LendingBook

```purescript
-- When creating a position
dynamics <- calculateDynamics incentives lendingRecord
let borrowerCost = dynamics.borrowerRate
    lenderReturn = dynamics.lenderRate
    polContribution = dynamics.polFlow
```

### With POL Module

```purescript
-- When processing fees
let polAmount = principal * dynamics.polFlow * timeElapsed
contributeToPOL polState opType polAmount (Just positionId)
```

### With Risk Assessment

```purescript
-- Update market metrics based on risk
let newMetrics = calculateRiskMetrics systemState
-- Oracle handles market metrics updates
```

## Benefits of Unification

1. **Conceptual Clarity**: Fees and returns are clearly related through the spread
2. **Balanced Incentives**: Both sides of the market are considered together
3. **Sustainable Revenue**: POL growth is directly tied to market activity
4. **Adaptive Pricing**: Single system responds coherently to market conditions
5. **Reduced Complexity**: One module instead of two, with clear relationships

## Key Differences from Traditional Models

1. **Unified Calculation**: Single module calculates both lender and borrower rates
2. **Dynamic Spreads**: Spreads adjust based on market conditions (1-5%)
3. **Risk-Based Pricing**: Each operation type has distinct risk premiums
4. **POL Integration**: Direct flow of spreads to POL based on operation type
5. **Oracle-Driven**: Market metrics from Oracle drive rate adjustments

## Implementation Notes

1. **Rate Components**: Base rate + risk premium + market adjustment = lender rate
2. **Spread Bounds**: Minimum 0.1%, maximum 5%, target 1%
3. **POL Allocation**: Swap 15%, Staking 25%, Leverage 30%
4. **Adjustment Weights**: Volatility 30%, Utilization 40%, Health 20%, Cross-risk 10%
5. **Rate Bounds**: Minimum lender rate 0.1%, maximum borrower rate 50%
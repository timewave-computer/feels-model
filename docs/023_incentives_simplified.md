# Simplified Risk-Based Fee Model

## Overview

This document outlines the simplified fee model for the Feels Protocol, which prices fees based on the three core risks from the user perspective: credit risk, directional risk, and liquidity risk. Each risk dimension combines position parameters with real-time pool metrics to accurately price risk.

## Core Risk Dimensions

### 1. Credit Risk (Time/Duration)
The risk that capital won't be returned when expected.
- **Flash**: No credit risk (same transaction return)
- **Spot**: Minimal credit risk (perpetual but can exit anytime)
- **Monthly**: High credit risk (capital locked for 28 days)

### 2. Directional Risk (Leverage/Principal Volatility)
The risk of principal value changes due to market movements.
- **Senior (1x)**: Low directional risk (stable value)
- **Junior (3x)**: High directional risk (3x amplified movements)

### 3. Liquidity Risk (Asset Choice)
The risk of not being able to exit a position at fair value.
- **FeelsSOL**: No liquidity risk (protocol's base asset)
- **Token**: High liquidity risk (dependent on token market)

## Risk-Based Fee Model

The fee model maps position parameters to risk dimensions, then applies pool metrics as amplifiers:

```
Position Parameter → Base Risk × Pool Metric Amplifier = Risk Component

Credit Risk: Duration → Base Credit Risk × (1 + Utilization) 
Directional Risk: Leverage → Base Directional Risk × (1 + Volatility × 3)
Liquidity Risk: Asset → Base Liquidity Risk × (2 - Depth) × (1 + Volatility)

Total Fee = Credit Risk + Directional Risk + Liquidity Risk + Protocol Margin
```

### Component Calculations

#### Credit Risk Fee
```purescript
creditRisk = baseCreditRisk * (1 + utilization)

Where:
- Flash: 0 bps base
- Spot: 5 bps base  
- Monthly: 20 bps base
- Utilization amplifies credit risk (harder to get capital back when pool is full)
```

#### Directional Risk Fee
```purescript
directionalRisk = baseDirectionalRisk * (1 + volatility * 3)

Where:
- Senior: 1 bp base
- Junior: 10 bps base
- Volatility amplifies directional risk (higher volatility = more leverage risk)
```

#### Liquidity Risk Fee
```purescript
liquidityRisk = baseLiquidityRisk * (2 - depth) * (1 + volatility)

Where:
- FeelsSOL: 0 bps base
- Token: 10 bps base
- Depth score (0-1): Well-distributed liquidity reduces risk
  - depth = 1.0 → 1x multiplier (no amplification)
  - depth = 0.5 → 1.5x multiplier
  - depth = 0.0 → 2x multiplier (maximum amplification)
- Volatility further amplifies liquidity risk
```

#### Protocol Margin
Fixed 5 bps to cover operational costs and provide protocol revenue.

## Pool Metrics and Risk Amplification

### Utilization (Credit Risk Amplifier)
Pool utilization directly affects credit risk:
- Low utilization (< 50%): Normal credit risk
- High utilization (> 80%): 1.8x credit risk multiplier
- This incentivizes liquidity provision when most needed

### Volatility (Directional & Liquidity Risk Amplifier)
Pool volatility affects both directional and liquidity risks:
- Low volatility (< 20%): Minimal risk amplification
- High volatility (> 50%): Significant risk amplification
- This compensates liquidity providers for increased market risk

### Liquidity Depth (Liquidity Risk Amplifier)
Pool liquidity depth inversely affects liquidity risk:
- Well-distributed (depth > 0.8): Minimal amplification
- Concentrated (depth < 0.2): 1.8x risk multiplier
- This incentivizes liquidity provision across price ranges

## Example Fee Calculations

### Volatility Harvester Position
- **Profile**: Senior, Monthly, FeelsSOL
- **Market**: 30% volatility, 50% utilization, 0.7 depth
```
Credit Risk: 20bp * 1.5 = 30bp
Directional Risk: 1bp * 1.9 = 1.9bp
Liquidity Risk: 0bp (FeelsSOL has no base liquidity risk)
Total: 31.9bp + 5bp = 36.9bp (0.369%)
```

### Momentum Trader Position
- **Profile**: Junior, Flash, FeelsSOL
- **Market**: 30% volatility, 50% utilization, 0.3 depth (concentrated)
```
Credit Risk: 0bp
Directional Risk: 10bp * 1.9 = 19bp
Liquidity Risk: 0bp (FeelsSOL has no base liquidity risk)
Total: 19bp + 5bp = 24bp (0.24%)
```

### Token Issuer Position
- **Profile**: Senior, Monthly, Token
- **Market**: 30% volatility, 50% utilization, 0.5 depth
```
Credit Risk: 20bp * 1.5 = 30bp
Directional Risk: 1bp * 1.9 = 1.9bp
Liquidity Risk: 10bp * 1.5 * 1.3 = 19.5bp
Total: 51.4bp + 5bp = 56.4bp (0.564%)
```

## Implementation Notes

1. **Minimum Fee**: 2 bps to prevent spam and ensure protocol sustainability
2. **Maximum Fee**: No hard cap, but market forces naturally limit fees
3. **Update Frequency**: Fees recalculated on each swap (when volatility updates)
4. **Granularity**: All calculations use basis points (1 bp = 0.01%)

## Behavioral Incentives

This fee model encourages:
- **Liquidity provision** during high utilization (higher fees attract LPs)
- **Risk-appropriate positioning** (high-risk strategies pay proportional fees)
- **Market stability** (volatile conditions increase costs for destabilizing actions)
- **Efficient capital allocation** (fees reflect actual protocol costs)

## Complete Risk Model Summary

The Feels Protocol fee model creates a comprehensive risk framework:

```
1. Credit Risk → Duration + Utilization
   - Position commitment (Flash/Monthly/Spot) determines base risk
   - Pool utilization amplifies risk when liquidity is scarce

2. Directional Risk → Leverage + Volatility  
   - Leverage tier (Senior/Junior) determines exposure
   - Pool volatility amplifies risk during market turbulence

3. Liquidity Risk → Asset Choice + Depth
   - Asset type (FeelsSOL/Token) determines base risk
   - Pool depth measures distribution quality
   - Volatility further amplifies exit difficulty
```

This creates a self-balancing system where:
- **High-risk conditions** → Higher fees → Attracts liquidity providers
- **Low-risk conditions** → Lower fees → Attracts traders and borrowers
- **Concentrated liquidity** → Higher fees → Incentivizes broader distribution

## Future Considerations

As the protocol matures, we may add:
- Volume-based discounts for frequent traders
- Time-weighted fee reductions for long-term positions
- Dynamic protocol margin based on treasury needs
- Cross-position netting for portfolio risk management

But the core three-risk model remains the foundation for fair and transparent fee pricing.
# Three-Tier Risk Model for Feels Protocol

## Overview

This document presents a simplified three-tier risk framework that captures risks at the system, pool, and position levels. Each tier addresses all three sides of the risk triangle: Liquidity, Credit/Lending, and Price/Leverage.

## Tier 1: System-Level Risks (Synthetic SOL & Protocol)

### Liquidity Risk
- **Risk**: Synthetic SOL liquidity crisis across all pools
- **Metric**: Total system liquidity depth relative to daily volume
- **Mitigation**: System-wide NFV growth from all pool fees

### Credit Risk  
- **Risk**: Synthetic SOL backing insufficiency (jitoSOL collateral)
- **Metric**: Collateralization ratio (jitoSOL reserves / synthetic SOL supply)
- **Mitigation**: Maintain overcollateralization buffer, halt minting if ratio drops

### Price Risk
- **Risk**: Synthetic SOL depeg from SOL value
- **Metric**: TWAP deviation from target peg
- **Mitigation**: Rebase mechanism, arbitrage incentives, emergency backstop

## Tier 2: Pool-Level Risks (Individual Trading Pairs)

### Liquidity Risk
- **Risk**: Insufficient liquidity for normal trading
- **Metric**: Pool depth at ±2% from current price
- **Mitigation**: Pool-specific NFV, transitive liquidity borrowing, LP incentives

### Credit Risk
- **Risk**: Bad debt from lending positions in pool
- **Metric**: Pool utilization rate and health factors
- **Mitigation**: Interest rate curve, borrowing limits, NFV as insurance

### Price Risk
- **Risk**: Extreme price movements causing cascading effects
- **Metric**: Realized volatility and price impact of typical trades
- **Mitigation**: Fee tiers based on volatility, position limits

## Tier 3: Position-Level Risks (Individual Users)

### Liquidity Risk
- **Risk**: Loss versus rebalancing (LVR) for LPs
- **Metric**: IL relative to hodling, fee income vs LVR
- **Mitigation**: Concentrated liquidity, dynamic fees, fee compensation

### Credit Risk
- **Risk**: Bad debt from leveraged/lending positions
- **Metric**: Position health factor, time to liquidation
- **Mitigation**: Liquidation buffers, progressive fees near liquidation

### Price Risk
- **Risk**: Nominal losses from leverage or IL
- **Metric**: Position PnL, leverage ratio
- **Mitigation**: No-liquidation design for base positions, leverage limits

## Risk Scoring Formula

### System Score
```
SystemRisk = 0.4 × LiquidityRisk + 0.4 × CreditRisk + 0.2 × PriceRisk
```

### Pool Score  
```
PoolRisk = 0.3 × LiquidityRisk + 0.3 × CreditRisk + 0.4 × PriceRisk
```

### Position Score
```
PositionRisk = 0.3 × LiquidityRisk + 0.2 × CreditRisk + 0.5 × PriceRisk
```

## Fee Distribution Based on Risk

Fees are allocated to address the highest risk at each tier:

1. **High System Risk**: More fees directed to system-wide reserves
2. **High Pool Risk**: More fees directed to pool NFV
3. **High Position Risk**: More fees compensate risk-bearing positions

### Dynamic Fee Allocation
```
If SystemRisk > 0.7: 50% of fees to system reserves
Else if PoolRisk > 0.7: 40% of fees to pool NFV  
Else: Standard distribution (20% system, 30% NFV, 50% LPs)
```

## Risk Thresholds and Actions

### Green (0-0.3): Normal Operations
- Standard fee distribution
- All features enabled
- No restrictions

### Yellow (0.3-0.7): Elevated Caution
- Increased portion to NFV/reserves
- Higher fees for risky actions
- Monitoring alerts

### Red (0.7-1.0): Risk Mitigation Mode
- Maximum fees to risk mitigation
- Restrictions on new risky positions
- Potential circuit breakers

## Implementation Priorities

1. **Start Simple**: Basic risk metrics for each component
2. **Add Gradually**: Introduce advanced metrics based on data
3. **Stay Transparent**: Users can see all risk scores
4. **Automate Response**: Preset actions at each threshold

## Key Design Principles

1. **Clarity**: Each risk type is clearly defined and measurable
2. **Balance**: All three risk types addressed at each tier
3. **Actionability**: Clear responses to different risk levels
4. **Simplicity**: Avoid complex interactions between metrics

This three-tier model provides comprehensive risk coverage while remaining simple enough to understand and implement effectively.
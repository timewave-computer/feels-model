# Unified Risk & Fee Model: Triangular Dual Risk Mitigation

## Overview

This model implements a comprehensive dual risk mitigation framework across all three sides of the risk triangle. Each side addresses both system risks and user risks through dynamic fee compensation mechanisms.

### Core Design Principles

1. **Manipulation Resistance**: All metrics are based on:
   - System-wide health indicators (cannot be gamed individually)
   - Real risk exposure (requires actual capital at risk)
   - Opportunity costs (cannot fake without missing other opportunities)

2. **Dual Risk Mitigation**: Each side addresses:
   - **System Risk**: Threats to protocol stability
   - **User Risk**: Costs borne by individual participants

3. **Self-Balancing**: Dynamic adjustments ensure:
   - Resources flow to stressed areas automatically
   - Feedback loops prevent cascading failures
   - Equilibrium between user profitability and system health

## 1. Liquidity Risk (LP Positions)

### Risk Profile

- **System Risk**: **Liquidity Crisis** - System may face liquidity shortage
- **User Risk**: **Loss vs Rebalancing (LVR)** - LPs lose to arbitrageurs

### Core Metrics

- $\text{VWBD}_i$: Volume-Weighted Book Depth contribution of LP $i$
- $\text{LVR}_i$: Cumulative Loss vs Rebalancing for LP $i$
- $\text{LiquidityStress}(S) \in [0,1]$: System liquidity stress indicator

### LP Fee Compensation Formula

$$\text{Fee}_i^{LP} = \lambda_{LP}(S) \cdot \frac{\frac{\text{VWBD}_i}{\text{LVR}_i + \epsilon}}{\sum_j \frac{\text{VWBD}_j}{\text{LVR}_j + \epsilon}}$$

Where:
$$\lambda_{LP}(S) = \lambda_{base} \cdot \left(1 + \alpha \cdot \text{LiquidityStress}(S)\right)$$

### Liquidity Stress Calculation

$$\text{LiquidityStress}(S) = \max\left(\frac{\text{TargetDepth} - \text{CurrentDepth}}{\text{TargetDepth}}, 0\right) \cdot \beta + \text{VolatilityMultiplier} \cdot (1-\beta)$$

Where:
- $\text{TargetDepth}$: Desired liquidity depth for healthy markets
- $\text{CurrentDepth}$: Actual available liquidity
- $\text{VolatilityMultiplier}$: Scales with market volatility
- $\beta \in [0,1]$: Weight between depth and volatility factors

### Why This Works

- **LPs with high VWBD** provide deep, useful liquidity → get rewarded
- **LPs with high LVR** suffer more from adverse selection → get compensated
- **Dynamic scaling** increases total rewards when liquidity is scarce

## 2. Credit Risk (Lending Positions)

### Risk Profile

- **System Risk**: **Insolvency Risk** - Protocol may become insolvent from bad debt
- **User Risk**: **Bad Debt** - Lenders may be exposed to borrower defaults

### Core Metrics

- $\text{TVL}_i$: Time-Value Locked (Amount × Duration) for lender $i$
- $\text{RAD}_i$: Risk-Adjusted Duration for lender $i$
- $\text{LPS}_i$: Liquidity Provision Score for lender $i$
- $\text{InsolvencyStress}(S) \in [0,1]$: System solvency stress indicator

### Lending Fee Compensation Formula

$$\text{Fee}_i^{Lend} = \lambda_{Lend}(S) \cdot \frac{\text{TVL}_i \cdot \text{RiskMultiplier}_i}{\sum_j \text{TVL}_j \cdot \text{RiskMultiplier}_j}$$

Where:
$$\text{RiskMultiplier}_i = (1 + \text{RAD}_i) \cdot (1 + \text{LPS}_i)$$

$$\lambda_{Lend}(S) = \lambda_{base} \cdot (1 + \beta \cdot \text{InsolvencyStress}(S))$$

### Metric Calculations

#### Risk-Adjusted Duration
$$\text{RAD}_i = \text{Duration}_i \cdot (1 - \text{AvgHealthFactor}_{system})$$

- When system health is low, duration is valued higher
- Rewards lending during risky periods

#### Liquidity Provision Score
$$\text{LPS}_i = \frac{1}{\text{Duration}_i} \int_0^T \text{Available}_i(t) \cdot \text{UtilizationStress}(t) \, dt$$

- Integrates availability weighted by system stress
- Higher score for being available when needed

#### Insolvency Stress
$$\text{InsolvencyStress}(S) = \max\left(0, \frac{\text{TargetSolvency} - \text{CurrentSolvency}}{\text{TargetSolvency}}\right)$$

### Dynamic Interest Rate Model

$$\text{InterestRate} = \text{BaseRate} \cdot (1 + \text{UtilizationMultiplier}) \cdot (1 + \text{SolvencyMultiplier})$$

## 3. Price Risk (Leverage Positions)

### Risk Profile

- **System Risk**: **Debasement Risk** - System token may lose value from overleveraging
- **User Risk**: **Nominal Loss** - Leveraged users face amplified losses

### Core Metrics

- $\text{Exposure}_i$: Position size × Leverage ratio for position $i$
- $\text{HealthFactor}_i$: Collateral value / Borrowed value for position $i$
- $\sigma_{realized}$: Realized market volatility
- $\text{DebasementStress}(S) \in [0,1]$: System debasement stress indicator

### Leverage Fee Compensation Formula

$$\text{Fee}_i^{Lev} = \lambda_{Lev}(S) \cdot \frac{\text{RiskAbsorption}_i}{\sum_j \text{RiskAbsorption}_j}$$

Where:
$$\text{RiskAbsorption}_i = \text{Exposure}_i \cdot \sigma_{realized} \cdot \frac{1}{\text{HealthFactor}_i}$$

$$\lambda_{Lev}(S) = \lambda_{base} \cdot (1 + \gamma \cdot \text{DebasementStress}(S))$$

### Debasement Stress Calculation

$$\text{DebasementStress}(S) = \text{TotalLeverageRatio} \cdot \text{VolatilityIndex} \cdot \text{VelocityMultiplier}$$

Where:
- $\text{TotalLeverageRatio} = \frac{\text{Total Leveraged Exposure}}{\text{Total Collateral}}$
- $\text{VolatilityIndex}$: Market-wide volatility measure
- $\text{VelocityMultiplier}$: Token velocity indicator

### Liquidation Protection Fee

$$\text{ProtectionFee} = \text{BaseFee} \cdot \exp(-k \cdot \text{LiquidationDistance})$$

- Increases exponentially as positions approach liquidation
- Creates buffer against liquidation cascades

## 4. Edge Positions (Combined Risk Types)

### Overview

Edge positions combine two of the three fundamental risk types, creating hybrid positions with unique risk/reward profiles. These positions require specialized fee models that account for the interaction between risk types.

### 4.1 Staked Lending (Credit + Liquidity Risk)

**Risk Profile**:
- **System Risks**: Insolvency Risk + Liquidity Crisis
- **User Risks**: Bad Debt + Loss vs Rebalancing

**Position Characteristics**:
- Time-locked liquidity provision
- Combines lending duration with LP exposure
- Higher capital efficiency through dual utility

**Fee Compensation Formula**:

$$\text{Fee}_i^{SL} = \lambda_{SL}(S) \cdot \frac{\text{TVL}_i \cdot \text{VWBD}_i \cdot \text{DualRiskMultiplier}_i}{\sum_j \text{TVL}_j \cdot \text{VWBD}_j \cdot \text{DualRiskMultiplier}_j}$$

Where:
$$\text{DualRiskMultiplier}_i = \sqrt{\frac{1}{\text{LVR}_i + \epsilon} \cdot (1 + \text{RAD}_i)}$$

$$\lambda_{SL}(S) = \lambda_{base} \cdot \left(1 + \frac{\alpha \cdot \text{LiquidityStress}(S) + \beta \cdot \text{InsolvencyStress}(S)}{2}\right)$$

**Why This Works**:
- Rewards providing both time commitment and liquidity depth
- Square root prevents either risk from dominating
- Dynamic lambda responds to both types of system stress

### 4.2 Leveraged Lending (Credit + Price Risk)

**Risk Profile**:
- **System Risks**: Insolvency Risk + Debasement Risk
- **User Risks**: Bad Debt + Nominal Loss

**Fee Compensation Formula**:

$$\text{Fee}_i^{LL} = \lambda_{LL}(S) \cdot \frac{\text{TVL}_i \cdot \text{RiskAbsorption}_i}{\sum_j \text{TVL}_j \cdot \text{RiskAbsorption}_j}$$

### 4.3 Leveraged Liquidity (Liquidity + Price Risk)

**Risk Profile**:
- **System Risks**: Liquidity Crisis + Debasement Risk
- **User Risks**: Loss vs Rebalancing + Nominal Loss

**Fee Compensation Formula**:

$$\text{Fee}_i^{LLP} = \lambda_{LLP}(S) \cdot \frac{\text{VWBD}_i \cdot \text{Exposure}_i}{\sum_j \text{VWBD}_j \cdot \text{Exposure}_j}$$

## 5. Unified System Management

### Cross-Risk Interactions

The three risks interact and amplify each other:

1. **Liquidity → Credit**: Low liquidity makes liquidations harder → increases bad debt
2. **Credit → Price**: Bad debt forces token minting → causes debasement
3. **Price → Liquidity**: Token debasement reduces LP incentives → lowers liquidity

### Systemic Risk Score

$$\text{SystemicRisk} = \alpha_1 \cdot \text{LiquidityStress} + \alpha_2 \cdot \text{InsolvencyStress} + \alpha_3 \cdot \text{DebasementStress} + \alpha_4 \cdot \text{CrossRisk}$$

Where:
$$\text{CrossRisk} = \text{LiquidityStress} \cdot \text{InsolvencyStress} + \text{InsolvencyStress} \cdot \text{DebasementStress} + \text{DebasementStress} \cdot \text{LiquidityStress}$$

### Global Fee Distribution

Total protocol fees are distributed based on quadratic stress levels:

$$\text{LiquidityShare} = \frac{\text{LiquidityStress}^2}{\text{LiquidityStress}^2 + \text{InsolvencyStress}^2 + \text{DebasementStress}^2}$$

$$\text{LendingShare} = \frac{\text{InsolvencyStress}^2}{\text{LiquidityStress}^2 + \text{InsolvencyStress}^2 + \text{DebasementStress}^2}$$

$$\text{LeverageShare} = \frac{\text{DebasementStress}^2}{\text{LiquidityStress}^2 + \text{InsolvencyStress}^2 + \text{DebasementStress}^2}$$

## 6. Implementation Parameters

### Time Windows

- $T$: Normalization period for all metrics (e.g., 24 hours)
- Integration periods for time-weighted calculations
- Decay factors for recent vs. historical data

### System Parameters

| Parameter | Symbol | Typical Range | Description |
|-----------|--------|---------------|-------------|
| Base fee budget | $\lambda_{base}$ | 0.1-1% of TVL | Daily fee distribution |
| Liquidity sensitivity | $\alpha$ | 0.5-2.0 | Response to liquidity stress |
| Solvency sensitivity | $\beta$ | 0.5-2.0 | Response to insolvency stress |
| Debasement sensitivity | $\gamma$ | 0.5-2.0 | Response to debasement stress |
| Small constant | $\epsilon$ | 0.0001 | Prevents division by zero |
| Liquidation constant | $k$ | 2-5 | Liquidation fee curve steepness |

### Adaptive Bounds

- **Minimum thresholds**: Ensure meaningful participation levels
- **Maximum multipliers**: Cap extreme rewards for sustainability
- **Dynamic adjustments**: Bounds respond to system state

## 7. System Benefits

### Self-Balancing Properties

1. **Liquidity Crisis Prevention**
   - Stress → Higher $\lambda_{LP}(S)$ → More LP rewards → Increased liquidity

2. **Solvency Maintenance**
   - Low health → Higher $\lambda_{Lend}(S)$ → More lending rewards → Better solvency

3. **Debasement Control**
   - High leverage → Higher $\lambda_{Lev}(S)$ → Leverage costs increase → Reduced overleveraging

### Manipulation Resistance

All metrics require:
- **Real capital deployment**: Cannot fake participation
- **Opportunity costs**: Missing other yields while participating
- **System-wide dependencies**: Individual actions cannot game metrics

### User Alignment

- **Fair compensation**: Rewards proportional to risk absorbed
- **Predictable dynamics**: Clear relationship between contribution and reward
- **No perverse incentives**: Cannot profit from harming the system

## 8. Conclusion

This unified model creates a comprehensive risk management framework where:

1. **Each risk type** has dedicated compensation mechanisms
2. **System and user risks** are both addressed
3. **Cross-risk amplification** is explicitly managed
4. **Dynamic responses** maintain equilibrium

The result is a self-stabilizing ecosystem that:
- Compensates participants fairly for risks absorbed
- Prevents systemic crises through early intervention
- Aligns individual incentives with protocol health
- Resists manipulation while rewarding genuine contribution
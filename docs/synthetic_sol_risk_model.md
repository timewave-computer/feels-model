# Synthetic SOL Risk Model: Per-Pool vs Global Scoring

## The Central Role of Synthetic SOL

### System Architecture
- Every pool: `syntheticSOL ↔ TokenX`
- Synthetic SOL acts as the **unit of account** and **routing medium**
- All value flows through synthetic SOL, creating a hub-and-spoke topology

### Risk Transmission Mechanism
Synthetic SOL creates **risk contagion pathways**:
```
TokenA crisis → syntheticSOL/TokenA pool → syntheticSOL systemwide → all other pools
```

## Risk Scoring Approaches

### 1. Pure Per-Pool Scoring

**Model**: Each pool independently scored based on its specific token risk

**Advantages**:
- Accurate local risk pricing
- Tokens with different risk profiles pay appropriate fees
- Market-based risk discovery

**Critical Flaw**: 
- **Ignores systemic risk transmission through syntheticSOL**
- A crisis in one large pool affects syntheticSOL value globally
- Could create false sense of security in "safe" token pools

### 2. Pure Global Scoring

**Model**: All pools share the same risk score based on system-wide metrics

**Advantages**:
- Captures systemic risk
- Simple to implement
- No gaming between pools

**Critical Flaw**:
- **No differentiation between token risks**
- Safe tokens subsidize risky ones
- No incentive for quality token selection

### 3. Hybrid Model: Local + Systemic Risk

**This is the recommended approach**

## Recommended: Dual-Layer Risk Model

### Layer 1: Pool-Specific Risk
Each pool has local risk factors based on its specific token:

```
PoolRisk_i = f(
    TokenVolatility_i,
    PoolDepth_i,
    TokenMarketCap_i,
    PoolUtilization_i
)
```

### Layer 2: Systemic Risk (Synthetic SOL Health)
Global risk factor affecting all pools:

```
SystemicRisk = f(
    syntheticSOL_TotalSupply,
    syntheticSOL_CollateralRatio,
    syntheticSOL_Velocity,
    syntheticSOL_PegDeviation
)
```

### Combined Risk Score
```
TotalRisk_i = PoolRisk_i × (1 + SystemicRisk) + CrossPoolContagion_i
```

Where:
```
CrossPoolContagion_i = Σ_j (PoolRisk_j × LiquidityShare_j × RoutingDistance_ij)
```

## Fee Structure Under Dual-Layer Model

### Base Fee Component
```
BaseFee_i = λ_base × PoolRisk_i
```
- Varies by pool based on token-specific risks
- Incentivizes liquidity in safer tokens

### Systemic Insurance Component
```
InsuranceFee = λ_insurance × SystemicRisk × PoolTVL_i / TotalTVL
```
- Same percentage for all pools
- Builds reserves for potential syntheticSOL devaluation
- Proportional to pool's systemic importance

### Contagion Premium
```
ContagionFee_i = λ_contagion × CrossPoolContagion_i
```
- Higher for pools with risky neighbors
- Accounts for routing exposure

### Total Fee
```
TotalFee_i = BaseFee_i + InsuranceFee + ContagionFee_i
```

## Synthetic SOL Stability Mechanisms

### 1. Early Warning System
Monitor synthetic SOL health metrics:
- **Peg Deviation**: |syntheticSOL/SOL - 1|
- **Backing Ratio**: Total collateral / syntheticSOL supply
- **Velocity**: Trading volume / supply
- **Concentration**: Largest pool TVL / Total TVL

### 2. Progressive Interventions
As SystemicRisk increases:

**Level 1 (SystemicRisk < 0.2)**:
- Normal operations
- Standard fee distribution

**Level 2 (0.2 < SystemicRisk < 0.5)**:
- Increase insurance fees
- Restrict new risky token listings
- Increase collateral requirements

**Level 3 (0.5 < SystemicRisk < 0.8)**:
- Pause leveraged positions
- Increase liquidation buffers
- Direct fees to stability fund

**Level 4 (SystemicRisk > 0.8)**:
- Consider controlled devaluation
- Implement withdrawal limits
- Emergency liquidity provisions

### 3. Devaluation as Last Resort
If synthetic SOL must be devalued:
```
NewRate = CurrentRate × (1 - DevaluationFactor)
DevaluationFactor = min(RequiredAdjustment, MaxDevaluation)
```

- Apply equally across all pools
- Use insurance fund to cushion impact
- Gradual implementation to prevent bank run

## Implementation Considerations

### 1. Oracle Requirements
- Need reliable price feeds for all tokens
- Synthetic SOL price monitoring vs SOL
- Cross-pool correlation tracking

### 2. Computational Efficiency
- Cache systemic risk calculations (update every block)
- Efficient routing distance calculations
- Gas-optimized fee updates

### 3. Governance Parameters
- Risk weight calibration
- Insurance fund target size
- Devaluation triggers and limits

## Example Scenarios

### Scenario 1: Single Token Crash
- TokenA crashes 50%
- PoolRisk_A spikes
- Small increase in SystemicRisk
- TokenA LPs pay higher fees
- Other pools see minor fee increases

### Scenario 2: Systemic Crisis
- Multiple tokens crash
- SystemicRisk spikes
- All pools see significant fee increases
- Insurance fund activated
- Possible intervention triggers

### Scenario 3: Synthetic SOL Depeg
- Synthetic SOL trades below SOL
- Maximum SystemicRisk
- Emergency measures activated
- Controlled devaluation if necessary

## Conclusion

The dual-layer model balances:
1. **Local accuracy**: Pools pay for their specific risks
2. **Systemic protection**: All pools contribute to system stability
3. **Contagion awareness**: Routing risks are priced in
4. **Backstop mechanism**: Devaluation available but discouraged

This creates proper incentives:
- LPs prefer safer tokens (lower fees)
- System builds reserves during good times
- Early intervention prevents crises
- Fair distribution of systemic costs
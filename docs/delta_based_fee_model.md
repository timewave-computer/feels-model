# Delta-Based Dynamic Fee Model

## Core Formula

```
Fee = BaseFee × (1 + ΔSystemRiskCapacity + ΔPoolRiskCapacity + ΔSystemUtility + ΔPoolUtility)
```

Where each delta is calculated as:
```
Δ = (ValueAfter - ValueBefore) / ValueBefore
```

## Components

### 1. System Risk Capacity Delta (ΔSystemRiskCapacity)

**Definition**: How the action changes the system's ability to absorb future risks

**Calculation**:
```
SystemRiskCapacity = (Total_NFV + JitoSOL_Reserves) / (Total_Exposure × System_Volatility)

ΔSystemRiskCapacity = (Capacity_After - Capacity_Before) / Capacity_Before
```

**Examples**:
- Adding liquidity to thin market: Positive (increases capacity)
- Taking large leveraged position: Negative (decreases capacity)
- Contributing to NFV: Positive (increases reserves)

### 2. Pool Risk Capacity Delta (ΔPoolRiskCapacity)

**Definition**: How the action changes the specific pool's risk absorption ability

**Calculation**:
```
PoolRiskCapacity = (Pool_NFV + Available_Liquidity) / (Pool_Volume × Pool_Volatility)

ΔPoolRiskCapacity = (Capacity_After - Capacity_Before) / Capacity_Before
```

**Examples**:
- Providing concentrated liquidity near price: Positive
- Removing liquidity during volatility: Negative
- Adding to pool's NFV: Positive

### 3. System Utility Delta (ΔSystemUtility)

**Definition**: How the action improves overall system usefulness

**Calculation**:
```
SystemUtility = (Active_Pools × Average_Depth × Transitive_Connections) / Total_TVL

ΔSystemUtility = (Utility_After - Utility_Before) / Utility_Before
```

**Examples**:
- Creating new pool connection: Positive (more routing options)
- Enabling transitive lending: Positive (better capital efficiency)
- Fragmenting liquidity: Negative (less useful)

### 4. Pool Utility Delta (ΔPoolUtility)

**Definition**: How the action improves the specific pool's usefulness

**Calculation** (based on Risk.purs unified model):
```
PoolUtility = TickEfficiency × UtilizationEfficiency × DurationValue × LeverageActivity

Where:
- TickEfficiency = (1 - Concentration) × TickDepth
- UtilizationEfficiency = 1 / (1 + Utilization)
- DurationValue = AverageDuration / MaxDuration
- LeverageActivity = TotalLeveragedVolume / PoolVolume

ΔPoolUtility = (Utility_After - Utility_Before) / Utility_Before
```

**Detailed Components**:

1. **Tick Efficiency**: 
   - Concentration = 1 - (RangeWidth / MaxRange)
   - Higher concentration at key prices = better efficiency
   - Balanced by tick depth to prevent thin orderbooks

2. **Utilization Efficiency**:
   - Lower utilization = more lending capacity available
   - Higher efficiency when pool has spare capacity

3. **Duration Value**:
   - Longer average lock duration = more stable liquidity
   - Normalized by maximum duration (e.g., 180 days)

4. **Leverage Activity**:
   - Active leverage usage indicates useful pool
   - But too much leverage reduces utility (risk)

**Examples**:
- Adding concentrated liquidity near current price: Positive (improves tick efficiency)
- Locking liquidity for longer duration: Positive (stability)
- Over-utilizing lending capacity: Negative (reduces efficiency)
- Balanced leverage usage: Positive (healthy activity)

## Fee Calculation Examples

### Example 1: Adding Liquidity to New Pool
```
ΔSystemRiskCapacity = +0.05 (improves system resilience)
ΔPoolRiskCapacity = +0.20 (significantly improves thin pool)
ΔSystemUtility = +0.10 (new routing path)
ΔPoolUtility = +0.30 (makes pool usable)

Fee Multiplier = 1 + 0.05 + 0.20 + 0.10 + 0.30 = 1.65
Final Fee = BaseFee × 1.65
```
Result: Higher fee but user is compensated through LP rewards

### Example 2: Taking Leveraged Position
```
ΔSystemRiskCapacity = -0.10 (increases system risk)
ΔPoolRiskCapacity = -0.05 (uses lending capacity)
ΔSystemUtility = 0 (neutral)
ΔPoolUtility = +0.02 (fee generation)

Fee Multiplier = 1 - 0.10 - 0.05 + 0 + 0.02 = 0.87
Final Fee = BaseFee × 0.87
```
Result: Slightly lower fee (user is taking risk)

### Example 3: Removing Liquidity During Crisis
```
ΔSystemRiskCapacity = -0.15 (reduces resilience when needed)
ΔPoolRiskCapacity = -0.25 (abandoning stressed pool)
ΔSystemUtility = -0.05 (reduces routing)
ΔPoolUtility = -0.20 (less useful pool)

Fee Multiplier = 1 - 0.15 - 0.25 - 0.05 - 0.20 = 0.35
Final Fee = BaseFee × 0.35
```
Result: Much lower fee (but this is an exit fee, so user still pays)

## Integration with Risk.purs Fee Model

The delta-based fee model extends the existing Risk.purs unified fee calculation:

### Base Fee Calculation (from Risk.purs)
```
UnifiedFeeMult = BaseFee × TickComponent × DurationComponent × LeverageComponent

Where:
- TickComponent = (1 + Concentration/TickDepth) × (1 + Utilization)
- DurationComponent = 1 + Duration/180
- LeverageComponent = (1 + Volatility) × LeverageRatio
```

### Delta Enhancement
```
FinalFee = UnifiedFeeMult × DeltaMultiplier

Where:
DeltaMultiplier = (1 + ΔSystemRiskCapacity + ΔPoolRiskCapacity + ΔSystemUtility + ΔPoolUtility)
```

This creates a two-layer fee model:
1. **Base Layer**: Risk-based pricing from position parameters
2. **Delta Layer**: Dynamic adjustment based on system impact

## Implementation Details

### 1. Pre-calculation
Before user confirms action, calculate all deltas and show:
- Current state metrics
- Projected state after action
- Each delta component
- Base fee from Risk.purs model
- Delta multiplier
- Final combined fee

### 2. Bounds and Limits
```
MinFeeMultiplier = 0.1  (90% discount maximum)
MaxFeeMultiplier = 3.0  (3x maximum charge)
```

### 3. Smoothing Function
For extreme deltas, apply smoothing:
```
If |Δ| > 0.5:
    Δ_smoothed = 0.5 × sign(Δ) + 0.5 × tanh(Δ)
```

### 4. Time Weighting
Recent actions have more impact:
```
Weight = exp(-time_since_action / decay_constant)
```

### 5. Complexity Premium (from Risk.purs)
Positions with multiple risk types have additional multiplier:
- Single risk type: 1.0x
- Two risk types: 1.1x  
- Three risk types: 1.2x

## Special Cases

### 1. Initial Pool Creation
- Use system averages for "before" state
- Bonus multiplier for creating useful pools

### 2. NFV Contributions
- Always positive deltas
- Additional rewards beyond fee reduction

## Benefits

1. **Aligned Incentives**: Users pay less when helping the system
2. **Dynamic Pricing**: Fees reflect real-time system needs
3. **Predictable**: Users can calculate fees before acting
4. **Self-Balancing**: System naturally attracts beneficial actions

## Monitoring Metrics

Track these to tune the model:
- Average fee multiplier by action type
- Correlation between deltas and system health
- User behavior changes based on fees
- Revenue impact vs. system improvement

This delta-based model creates a responsive fee system that automatically incentivizes actions that improve the protocol while charging more for actions that stress it.
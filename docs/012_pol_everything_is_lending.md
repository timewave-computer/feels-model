# Everything is Lending -> POL

## Summary

The "Everything is Lending" conceptual framework can be adapted to the Protocol-Owned Liquidity (POL) model, where the POL becomes protocol-owned lending liquidity that grows from all position types (swaps, staking, leverage) and provides an unruggable lending floor for the entire system.

## Core Adaptation: POL as Protocol-Owned Lending Capacity

### Definition in Everything is Lending
Protocol-Owned Liquidity (POL) in our model is permanently locked protocol-owned FeelsSOL that:
- Provides liquidity according to a distribution curve across the tick parameter space
- Cannot be withdrawn (unruggable guarantee)
- Grows from fees across all position types
- Concentrates liquidity below spot price as "floor" liquidity
- Maintains presence near spot and extends discovery range above for price discovery

### Revenue Streams
Each position type contributes to POL growth:

1. **Swap Fees** (Perpetual Loans): Portion of collateral exchange fees (α)
2. **Staking Fees** (Fixed-Term Loans): Portion of interest rate spreads (β)  
3. **Leverage Fees** (Synthetic Positions): Portion of leverage position fees (γ)
4. **Gateway Fees**: Portion of JitoSOL ↔ FeelsSOL conversion fees (δ)

### Growth Formula
$$\text{POL}_{\text{growth\_rate}} = \alpha \times \text{SwapVolume} \times \text{SwapFee} + \beta \times \text{StakingVolume} \times \text{InterestSpread} + \gamma \times \text{LeverageVolume} \times \text{LeverageFee} + \delta \times \text{GatewayVolume} \times \text{GatewayFee}$$

## Tick-Based POL

- POL distributed according to a curve across tick space
- Concentrated below spot (floor liquidity)
- Strategic placement near spot and discovery range above
- Extends into lending and leverage parameter spaces

## Implementation in Our Architecture

### 1. POL Distribution Curve
The protocol distributes POL liquidity according to a curve:
```purescript
-- Distribution curve parameters
type POLDistribution =
  { floorWeight :: Number     -- 60% below spot (e.g., 0.6)
  , spotWeight :: Number      -- 30% near spot (e.g., 0.3)
  , discoveryWeight :: Number -- 10% above spot (e.g., 0.1)
  , floorRange :: Range       -- 0.5x - 0.9x spot
  , spotRange :: Range        -- 0.9x - 1.1x spot
  , discoveryRange :: Range   -- 1.1x - 2.0x spot
  }
```

### 2. Fee Accumulation
Modify our fee structure to direct portions to POL:
```purescript
-- In Fees.purs
calculatePOLContribution :: FeeComponents -> Number
calculatePOLContribution fees =
  let swapContribution = fees.baseFee * 0.15  -- 15% of swap fees
      stakingContribution = fees.totalFee * 0.25  -- 25% of staking fees
      leverageContribution = fees.totalFee * 0.30  -- 30% of leverage fees
      gatewayContribution = fees.baseFee * 0.20  -- 20% of gateway fees
  in swapContribution + stakingContribution + leverageContribution + gatewayContribution
```

### 3. POL Tick Generation
POL creates ticks across the distribution curve:
```purescript
-- Generate POL ticks according to distribution
generatePOLTicks :: POLState -> SpotPrice -> Array Tick
generatePOLTicks polState spot =
  let balance = getPOLBalance polState
      dist = getPOLDistribution()
      
      -- Floor liquidity (60% of POL)
      floorTicks = generateRangeTicks 
        (balance * dist.floorWeight)
        dist.floorRange
        spot
        
      -- Near spot liquidity (30% of POL)
      spotTicks = generateRangeTicks
        (balance * dist.spotWeight)
        dist.spotRange
        spot
        
      -- Discovery liquidity (10% of POL)
      discoveryTicks = generateRangeTicks
        (balance * dist.discoveryWeight)
        dist.discoveryRange
        spot
        
  in floorTicks <> spotTicks <> discoveryTicks
```

## Antifragile Properties

### 1. Position Type Diversification
```
High swap activity → POL grows → More lending for staking positions
High staking demand → POL grows → More lending for leverage positions
High leverage usage → POL grows → More lending for all positions
```

### 2. Gateway Integration
```
JitoSOL inflows → Gateway fees → POL growth → Deeper FeelsSOL liquidity
Market volatility → More gateway usage → Faster POL accumulation
```

### 3. Dynamic Liquidity Support
```
Market stress → User ticks withdrawn → POL floor liquidity absorbs pressure
Price discovery → POL discovery liquidity ensures continuous markets
Volatility → POL spot liquidity dampens extreme moves
```

## Risk Scoring Adaptations

### Position-Level Metrics

#### 1. POL Support Score
$$\text{POLSupport} = \frac{\text{POL}_{\text{available}}}{\text{TotalLendingDemand} \times \text{getPOLCollateralRatio}}$$
Measures how much POL backing exists for current positions.

#### 2. Fee Contribution Score
$$\text{FeeContribution} = \frac{\text{PositionFees} \times \text{POLAllocationRate}}{\text{PositionSize}}$$
Measures how much each position contributes to POL growth.

### System-Level Metrics

#### 1. Lending Resilience
$$\text{LendingResilience} = \frac{\text{POL}_{\text{total}}}{\text{ActivePositions} \times \text{AveragePositionSize}}$$
Higher = system can support more positions even if all ticks withdraw.

#### 2. Growth Sustainability
$$\text{GrowthSustainability} = \frac{\text{POL}_{\text{growth\_rate}}}{\text{NewPositionRate} \times \text{AveragePOLContribution}}$$
Measures if POL grows faster than lending demand.

## Benefits of POL in Everything is Lending

1. **Universal Liquidity**: POL can support any asset pair through FeelsSOL
2. **Compound Growth**: All position types contribute to the same POL pool
3. **Risk Distribution**: POL absorbs risk across all position types
4. **Gateway Synergy**: JitoSOL backing + POL creates double protection
5. **Perpetual Growth**: Even "completed" positions leave permanent POL contributions

## Monitoring and Governance

### Key Metrics to Track
- POL growth rate by position type
- POL utilization during stress periods
- Fee parameter efficiency
- Gateway volume correlation with POL growth

## Conclusion

The POL model integrates well with the "Everything is Lending" conceptual framework by perpetually expanding protocol-owned lending capacity. This transforms the system from defending against risks to actively converting all activity into permanent lending liquidity.
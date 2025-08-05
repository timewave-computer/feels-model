# NFV Risk Model

## Executive Summary

The Network Floor Value (NFV) model can be powerfully adapted to our "Everything is Lending" architecture, where the NFV becomes protocol-owned lending liquidity that grows from all position types (swaps, staking, leverage) and provides an unruggable lending floor for the entire system.

## Core Adaptation: NFV as Protocol-Owned Lending Capacity

### Definition in Everything is Lending
Network Floor Value (NFV) in our model is permanently locked protocol-owned FeelsSOL that:
- Provides liquidity according to a distribution curve across the tick parameter space
- Cannot be withdrawn (unruggable guarantee)
- Grows from fees across all position types
- Concentrates liquidity below spot price as "floor" liquidity
- Maintains presence near spot and extends tail above for price discovery

### Revenue Streams
Each position type contributes to NFV growth:

1. **Swap Fees** (Perpetual Loans): Portion of collateral exchange fees (α)
2. **Staking Fees** (Fixed-Term Loans): Portion of interest rate spreads (β)  
3. **Leverage Fees** (Synthetic Positions): Portion of leverage position fees (γ)
4. **Gateway Fees**: Portion of JitoSOL ↔ FeelsSOL conversion fees (δ)

### Growth Formula
```
NFV_growth_rate = 
    α × SwapVolume × SwapFee +
    β × StakingVolume × InterestSpread +
    γ × LeverageVolume × LeverageFee +
    δ × GatewayVolume × GatewayFee
```

## Tick-Based NFV

- NFV distributed according to a curve across tick space
- Concentrated below spot (floor liquidity)
- Strategic placement near spot and tail above
- Extends into lending and leverage parameter spaces

## Implementation in Our Architecture

### 1. NFV Distribution Curve
The protocol distributes NFV liquidity according to a curve:
```purescript
-- Distribution curve parameters
type NFVDistribution =
  { floorWeight :: Number     -- 60% below spot (e.g., 0.6)
  , nearSpotWeight :: Number  -- 30% near spot (e.g., 0.3)
  , tailWeight :: Number      -- 10% above spot (e.g., 0.1)
  , floorRange :: Range       -- 0.5x - 0.9x spot
  , nearSpotRange :: Range    -- 0.9x - 1.1x spot
  , tailRange :: Range        -- 1.1x - 2.0x spot
  }
```

### 2. Fee Accumulation
Modify our fee structure to direct portions to NFV:
```purescript
-- In Fees.purs
calculateNFVContribution :: FeeComponents -> Number
calculateNFVContribution fees =
  let swapContribution = fees.baseFee * 0.15  -- 15% of swap fees
      stakingContribution = fees.totalFee * 0.25  -- 25% of staking fees
      leverageContribution = fees.totalFee * 0.30  -- 30% of leverage fees
      gatewayContribution = fees.baseFee * 0.20  -- 20% of gateway fees
  in swapContribution + stakingContribution + leverageContribution + gatewayContribution
```

### 3. NFV Tick Generation
NFV creates ticks across the distribution curve:
```purescript
-- Generate NFV ticks according to distribution
generateNFVTicks :: NFVState -> SpotPrice -> Array Tick
generateNFVTicks nfvState spot =
  let balance = getNFVBalance nfvState
      dist = getNFVDistribution()
      
      -- Floor liquidity (60% of NFV)
      floorTicks = generateRangeTicks 
        (balance * dist.floorWeight)
        dist.floorRange
        spot
        
      -- Near spot liquidity (30% of NFV)
      nearSpotTicks = generateRangeTicks
        (balance * dist.nearSpotWeight)
        dist.nearSpotRange
        spot
        
      -- Tail liquidity (10% of NFV)
      tailTicks = generateRangeTicks
        (balance * dist.tailWeight)
        dist.tailRange
        spot
        
  in floorTicks <> nearSpotTicks <> tailTicks
```

## Antifragile Properties

### 1. Position Type Diversification
```
High swap activity → NFV grows → More lending for staking positions
High staking demand → NFV grows → More lending for leverage positions
High leverage usage → NFV grows → More lending for all positions
```

### 2. Gateway Integration
```
JitoSOL inflows → Gateway fees → NFV growth → Deeper FeelsSOL liquidity
Market volatility → More gateway usage → Faster NFV accumulation
```

### 3. Dynamic Liquidity Support
```
Market stress → User ticks withdrawn → NFV floor liquidity absorbs pressure
Price discovery → NFV tail liquidity ensures continuous markets
Volatility → NFV near-spot liquidity dampens extreme moves
```

## Risk Scoring Adaptations

### Position-Level Metrics

#### 1. NFV Support Score
```
NFVSupport = NFV_available / (TotalLendingDemand × getNFVCollateralRatio)
```
Measures how much NFV backing exists for current positions.

#### 2. Fee Contribution Score
```
FeeContribution = PositionFees × NFVAllocationRate / PositionSize
```
Measures how much each position contributes to NFV growth.

### System-Level Metrics

#### 1. Lending Resilience
```
LendingResilience = NFV_total / (ActivePositions × AveragePositionSize)
```
Higher = system can support more positions even if all ticks withdraw.

#### 2. Growth Sustainability
```
GrowthSustainability = NFV_growth_rate / (NewPositionRate × AverageNFVContribution)
```
Measures if NFV grows faster than lending demand.

## Strategic Implementation

### 1. NFV Distribution Strategy
Deploy NFV liquidity according to the curve:
- 60% below spot price (0.5x-0.9x) as floor liquidity
- 30% near spot price (0.9x-1.1x) for active markets
- 10% above spot price (1.1x-2.0x) for price discovery
- Extends into staking (different durations) and leverage (different multiples) dimensions
- Future: Adaptive curve based on system feedback loops

### 2. Fee Parameter Optimization
Start with conservative parameters:
- α (swap fees to NFV): 15%
- β (staking spread to NFV): 25%
- γ (leverage fees to NFV): 30%
- δ (gateway fees to NFV): 20%

### 3. Unbonding Period Benefits
Staking positions with unbonding periods provide predictable NFV growth:
- Locked liquidity generates continuous fees
- Unbonding delays prevent sudden withdrawals
- Longer periods = higher NFV contribution rates

## Code Integration Points

### 1. Gateway Module
```purescript
-- In Gateway.purs
processGatewayFee :: Number -> Effect Unit
processGatewayFee feeAmount = do
  let nfvPortion = feeAmount * 0.20
  -- Add to protocol NFV balance
  updateNFVBalance nfvPortion
```

### 2. Execution Engine
```purescript
-- In Execution.purs
executePosition = do
  -- ... existing execution logic ...
  -- Calculate and allocate NFV contribution
  let nfvContribution = calculateNFVContribution positionFees
  allocateToNFV nfvContribution
```

### 3. Returns Module
```purescript
-- In Returns.purs
processReturns = do
  -- ... existing return logic ...
  -- NFV positions never mature/withdraw
  positions <- filterOut isNFVPosition positions
```

## Benefits of NFV in Everything is Lending

1. **Universal Liquidity**: NFV can support any asset pair through FeelsSOL
2. **Compound Growth**: All position types contribute to the same NFV pool
3. **Risk Distribution**: NFV absorbs risk across all position types
4. **Gateway Synergy**: JitoSOL backing + NFV creates double protection
5. **Perpetual Growth**: Even "completed" positions leave permanent NFV contributions

## Monitoring and Governance

### Key Metrics to Track
- NFV growth rate by position type
- NFV utilization during stress periods
- Fee parameter efficiency
- Gateway volume correlation with NFV growth

### Governance Considerations
- Adjust fee allocation percentages based on data
- Set NFV lending parameters (collateral ratios)
- Decide on NFV tick placement strategies
- Consider NFV-backed products (using NFV as collateral)

## Conclusion

The NFV model integrates perfectly with "Everything is Lending" by creating a perpetually growing protocol-owned lending capacity. This transforms our system from defending against risks to actively converting all activity into permanent lending liquidity.
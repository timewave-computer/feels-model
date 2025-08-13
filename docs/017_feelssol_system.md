# FeelsSOL System - Synthetic SOL Implementation

## Overview

The FeelsSOL system provides a synthetic SOL token backed by jitoSOL, enabling users to maintain exposure to Solana staking rewards while participating in the Feels Protocol's unified tick-based market. This implementation serves as the fundamental deposit layer that bridges liquid staking tokens with the protocol's DeFi operations.

## Architecture

### Core Components

1. **FeelsSOL Token**: Synthetic SOL token with 1:1 economic equivalence
2. **JitoSOL Backing**: Liquid staking token providing yield-bearing collateral
3. **Oracle System**: Price discovery and rate tracking with buffer management
4. **Withdrawal Buffer**: JitoSOL reserves for immediate liquidity

### System State

```purescript
type FeelsSOLState =
  { totalFeelsSOLSupply :: Ref Number      -- Total FeelsSOL in circulation
  , totalJitoSOLBacking :: Ref Number      -- Total JitoSOL backing the synthetics  
  , jitoSOLBuffer :: Ref Number            -- JitoSOL buffer for withdrawals
  , priceOracle :: Effect Number           -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Ref Number         -- Last oracle update timestamp
  , cachedPrice :: Ref (Maybe OraclePrice) -- Cached oracle price
  , entryFee :: Number                     -- Fee for entering (e.g., 0.001 = 0.1%)
  , exitFee :: Number                      -- Fee for exiting (e.g., 0.002 = 0.2%)
  , polAllocationRate :: Number            -- Portion of fees going to POL
  , bufferTargetRatio :: Number            -- Target buffer as % of backing (1%)
  }
```

## Core Operations

### System Entry (JitoSOL → FeelsSOL)

**Function**: `enterSystem :: FeelsSOLState -> String -> Number -> Effect (Either ProtocolError MintResult)`

**Process**:
1. **Validation**: Ensure positive jitoSOL amount
2. **Fee Calculation**: Apply entry fee (configured rate, not always 0%)
3. **Oracle Consultation**: Get current jitoSOL/SOL exchange rate
4. **FeelsSOL Minting**: Calculate FeelsSOL amount based on exchange rate
5. **Buffer Management**: Allocate portion to withdrawal buffer (1% target)
6. **State Update**: Update supply, backing, and buffer amounts

**Example**:
```
User deposits 100 jitoSOL (rate: 1.05 jitoSOL/SOL)
→ Mints 105 FeelsSOL 
→ Allocates 1 jitoSOL to buffer, 99 jitoSOL to backing
→ Total: 105 FeelsSOL supply, 99 jitoSOL backing, 1 jitoSOL buffer
```

### System Exit (FeelsSOL → JitoSOL)

**Function**: `exitSystem :: FeelsSOLState -> String -> Number -> Effect (Either ProtocolError BurnResult)`

**Process**:
1. **Validation**: Ensure sufficient FeelsSOL supply exists
2. **Oracle Consultation**: Get current exchange rate
3. **JitoSOL Calculation**: Determine jitoSOL amount to release
4. **Buffer-First Withdrawal**: Use buffer reserves first, then backing
5. **Fee Application**: Apply exit fee (configured rate, not always 0%)
6. **State Update**: Update supply, backing, and buffer amounts

**Example**:
```
User burns 52.5 FeelsSOL (rate: 1.05 jitoSOL/SOL)
→ Requires 50 jitoSOL release
→ Uses 1 jitoSOL from buffer, 49 jitoSOL from backing
→ User receives 50 jitoSOL
```

## Buffer Management System

### Purpose
The withdrawal buffer maintains a small reserve of jitoSOL (1% of total backing) to ensure immediate liquidity for withdrawals without needing to unstake from backing positions.

### Configuration
- **Target Ratio**: 1% of total backing
- **Minimum Ratio**: 0.5% (health warning threshold)
- **Rebalancing Threshold**: 0.5% deviation from target

### Buffer Operations

#### Automatic Buffer Allocation
During minting, the system automatically maintains buffer levels:
```purescript
let bufferTarget = (currentBacking + jitoSOLAmount) * state.bufferTargetRatio
    bufferAddition = if bufferTarget > currentBuffer 
                    then min jitoSOLAmount (bufferTarget - currentBuffer)
                    else 0.0
```

#### Buffer-First Withdrawals
During burning, buffer is used before backing:
```purescript
let fromBuffer = min jitoSOLToRelease currentBuffer
    fromBacking = jitoSOLToRelease - fromBuffer
```

#### Health Monitoring
```purescript
getBufferStatus :: FeelsSOLState -> Effect BufferStatusResult
```

Returns:
- Current buffer amount and target
- Buffer ratio relative to backing
- Health status (above 2% minimum)
- Rebalancing recommendations

## Oracle Integration

### Price Caching
- **Cache Duration**: 1 minute for performance
- **Confidence Level**: 99% for jitoSOL (high liquidity)
- **Fallback**: System maintains last known good price

### Exchange Rate Calculation
```purescript
getExchangeRate :: FeelsSOLState -> Effect Number
```

Provides the current jitoSOL/SOL rate used for minting and burning calculations.

## System Health Metrics

```purescript
getSystemHealth :: FeelsSOLState -> Effect SystemHealthResult
```

**Returns**:
- **Collateral Ratio**: Total backing value / FeelsSOL supply
- **Total Locked**: Amount of jitoSOL in the system
- **Total Minted**: Amount of FeelsSOL in circulation
- **Buffer Ratio**: Buffer amount / total backing
- **Health Status**: Overall system health boolean

**Health Criteria**:
- Collateral ratio ≥ 100% (fully backed)
- Buffer ratio ≥ 0.5% (adequate liquidity)

## Integration with Feels Protocol

### Position Creation
FeelsSOL serves as the base currency for all position operations:
- Users enter with jitoSOL → receive FeelsSOL
- FeelsSOL used to create tick-based positions
- Position yields and fees paid in FeelsSOL

### POL Contribution
Fee allocation to Protocol-Owned Liquidity:
- **Default Rate**: 25% of fees go to POL
- **Use Cases**: Market making, stability fund, protocol development

### Cross-Module Design
- **Oracle Sharing**: Price data shared with position pricing
- **Buffer Coordination**: Buffer health affects position limits
- **Fee Integration**: Fees flow into protocol revenue streams

## Future Enhancements

### DAO Governance Integration
- **Buffer Management**: DAO control of buffer ratios and rebalancing
- **Fee Structure**: Governance-adjustable entry/exit fees
- **Emergency Controls**: Circuit breakers and emergency stops

### Advanced Features
- **Dynamic Buffering**: Market-responsive buffer sizing
- **Yield Distribution**: Staking rewards passing through to users
- **Cross-Asset Support**: Additional liquid staking tokens

## Technical Considerations

### Performance
- **State Efficiency**: Ref-based mutable state for frequent updates
- **Oracle Caching**: Reduced external calls with intelligent cache invalidation
- **Buffer Automation**: Minimal computational overhead for buffer management

### Security
- **Collateral Safety**: Full backing requirement prevents undercollateralization
- **Buffer Reserves**: Emergency liquidity prevents system lock-up
- **Oracle Reliability**: High confidence thresholds and fallback mechanisms

This implementation provides a robust foundation for synthetic asset creation while maintaining the safety, efficiency, and user experience required for the broader Feels Protocol ecosystem.
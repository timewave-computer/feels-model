# Architecture Decision Record: Pool-Centric Unified Tick System with Vertically Integrated Issuance

**Status**: Adopted
**Date**: 2024
**Authors**: System Architecture Team

## Context

The Feels Protocol requires a comprehensive system to manage individual liquidity pools. Each pool represents a FeelsSOL/Token pair and must handle:

- **Price Discovery**: Track spot price and volatility within the pool
- **Floor System**: Maintain an ever-increasing price support level
- **Liquidity Distribution**: Allocate liquidity optimally across price ranges
- **Position Management**: Track user stakes and contributions
- **Protocol-Owned Liquidity (POL)**: Deploy protocol liquidity strategically
- **Dual Token Issuance**: Control minting/burning of both tokens in the pair

Initially, these were conceived as separate systems. However, this led to synchronization complexity, redundant calculations, and potential inconsistencies. Additionally, the protocol's unique ability to issue both tokens in each pair was not fully leveraged.

## Decision

We will implement a **Pool-Centric Unified Tick System with Vertically Integrated Issuance** where:

1. **Each pool is a self-contained market** with its own state and dynamics
2. **Ticks are living entities** that maintain local state and metrics
3. **The protocol controls issuance of both tokens** in each pair
4. **All state updates are atomic** within a pool
5. **The floor emerges from actual market support** rather than arbitrary rules

## Architecture

### Core Pool Structure

```purescript
type PoolState =
  { token0 :: TokenType             -- Base token (usually FeelsSOL)
  , token1 :: TokenType             -- Quote token
  , sqrtPriceX96 :: Number          -- Current sqrt price (Q64.96 format)
  , liquidity :: Number             -- Active liquidity
  , tick :: Int                     -- Current tick
  , feeGrowthGlobal0X128 :: Number  -- Global fee growth token0
  , feeGrowthGlobal1X128 :: Number  -- Global fee growth token1
  , protocolFee :: Number           -- Protocol fee share (basis points)
  , unlocked :: Boolean             -- Reentrancy guard
  , offering :: Maybe OfferingId    -- Active offering (if any)
  }

type TermSchedule =
  { nextMonthly :: BlockNumber      -- Next monthly expiry block
  }

-- Tranche metrics are calculated dynamically from PoolState and Position data
-- and are not stored as a top-level field within PoolState.

-- Token pairs are represented by token0 and token1 of type TokenType.
```

### Living Tick System

```purescript
type Tick =
  { liquidityNet :: Number              -- Net liquidity change when crossing this tick
  , liquidityGross :: Number            -- Total liquidity referencing this tick
  , feeGrowthOutside0 :: Number         -- Fee growth on the other side (token0)
  , feeGrowthOutside1 :: Number         -- Fee growth on the other side (token1)
  , initialized :: Boolean              -- Whether this tick has been initialized
  }

-- Analytical metrics for ticks are managed in Indexer.Tick.purs
type TickMetrics =
  { support :: Number              -- Buy volume - sell volume
  , temperature :: Number          -- Activity level (hot/cold)
  , reliability :: Number          -- Consistency of liquidity provision
  , velocity :: Number             -- Rate of liquidity change
  , momentum :: Number             -- Directional trading bias
  , depth :: Number                -- Available liquidity depth score
  , liquidity :: Number            -- Current active liquidity at tick
  , volume24h :: Number            -- 24-hour trading volume
  , volume7d :: Number             -- 7-day trading volume
  , feeRevenue24h :: Number        -- Estimated fee revenue generated
  , priceHistory :: Array Number   -- Historical price movements
  , volumeHistory :: Array Number  -- Historical volume patterns
  , eventLog :: Array TickEvent    -- Processed event history
  }
```

### Unified Position System with Terms and Tranches

```purescript
type Position =
  { id :: PositionId
  , poolId :: PoolId               -- Pool this position belongs to
  , owner :: String                -- Owner of the position
  , amount :: Number                -- Initial capital
  , tranche :: Tranche             -- Senior (1x) or Junior (3x)
  , term :: TermCommitment         -- Synchronized term for all positions
  , rollover :: Boolean            -- Whether position auto-rolls to next term
  , shares :: ShareAmount          -- Shares within their tranche
  , createdAt :: BlockNumber       -- Creation block number
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount below floor
  }

data Tranche
  = Senior    -- 1x exposure, protected, lower yield
  | Junior    -- 3x exposure, first loss, higher yield

data TermCommitment
  = Spot                           -- Perpetual, no expiry
  | Monthly BlockNumber            -- 28-day term, expires at block boundary

-- ManagedLiquidity, TrancheBucket, and TermBucket are conceptual types
-- and are not explicitly defined as top-level records in the implementation.
-- Their concepts are handled implicitly or within other data structures.
```

### Vertically Integrated Issuance

-- The concept of "issuance" is handled by Protocol.FeelsSOL.purs (for FeelsSOL)
-- and Protocol.Offering.purs (for user-created tokens). There isn't a single
-- IssuanceState, IssuanceMetrics, or IssuanceController record in the implementation.
-- Their concepts are handled implicitly or within other data structures/logic.

### Data Flow

-- The data flow is distributed across various modules and functions.
-- Events are processed by indexers (e.g., Indexer.Pool.purs, Indexer.Tick.purs)
-- and commands are handled by UI.Command.purs, which then interacts with protocol modules.

## Key Mechanisms

### 1. Synchronized Term System

All positions align to global term boundaries for predictable liquidity events:

```purescript
-- Get next synchronized expiry block
getNextExpiry :: TermCommitment -> Int -> Int
getNextExpiry term currentBlock =
  case term of
    Spot -> 2147483647              -- MaxInt32 (effectively never expires)
    Monthly _ -> getNextMonthlyExpiry currentBlock

-- Process all expired positions at once
processExpiredPositions :: BlockNumber -> Array Position -> Array Position
processExpiredPositions currentBlock = map processOne
  where
    processOne pos =
      if isExpired currentBlock pos
      then handleExpiredPosition pos currentBlock
      else pos

-- Roll expired position to spot
rollToSpot :: Position -> Position
rollToSpot position = position { term = Spot }
```

### 2. Two-Tranche Exposure Model

Junior tranche provides amplified exposure while senior tranche is more protected:

```purescript
-- Calculate current tranche values based on total pool value and PnL
-- Implements the waterfall logic where Junior absorbs losses first
calculateTrancheValues :: Number -> Number -> TrancheState -> TrancheValues
calculateTrancheValues initialValue currentValue trancheState =
  let
    -- Calculate profit/loss
    pnl = currentValue - initialValue

    -- Get initial tranche values
    initialSenior = trancheState.seniorValue
    initialJunior = trancheState.juniorValue

    -- Apply waterfall logic
    result = if pnl >= 0.0 then
      -- Profits: Distribute based on exposure (Junior gets 3x weight)
      let juniorWeight = 3.0  -- Junior has 3x exposure
          seniorWeight = 1.0  -- Senior has 1x exposure

          -- Calculate weighted distribution
          totalWeight = (initialJunior * juniorWeight) + (initialSenior * seniorWeight)

      in  -- Avoid division by zero
          if totalWeight > 0.0 then
            { senior: initialSenior + pnl * (initialSenior * seniorWeight / totalWeight)
            , junior: initialJunior + pnl * (initialJunior * juniorWeight / totalWeight)
            }
          else
            { senior: initialSenior, junior: initialJunior }
    else
      -- Losses: Junior absorbs first up to 90% of their capital
      let absLoss = abs pnl
          maxJuniorLoss = initialJunior * 0.9  -- Junior can lose up to 90%
          
          -- Calculate actual losses
          juniorLoss = min maxJuniorLoss absLoss
          seniorLoss = max 0.0 (absLoss - juniorLoss)
          
          -- Junior retains at least 10% of initial value
          newJunior = max (initialJunior * 0.1) (initialJunior - juniorLoss)
          newSenior = initialSenior - seniorLoss
          
      in { senior: newSenior, junior: newJunior }

  in result

-- Update tranche state with new values and maintain share price consistency
updateTrancheValues :: TrancheState -> TrancheValues -> TrancheState
updateTrancheValues state newValues =
  state
    { seniorValue = newValues.senior
    , juniorValue = newValues.junior
    }

-- Distribute pool PnL to positions based on their shares and tranche
-- Returns updated position with new value
distributePoolPnL :: Position -> TrancheState -> TrancheValues -> Position
distributePoolPnL position oldState newValues =
  let
    -- Calculate per-share values for each tranche
    seniorPerShare = if oldState.seniorShares > 0.0
                     then newValues.senior / oldState.seniorShares
                     else 0.0
                     
    juniorPerShare = if oldState.juniorShares > 0.0
                     then newValues.junior / oldState.juniorShares
                     else 0.0
    
    -- Update position value based on tranche and shares
    newValue = case position.tranche of
      Senior -> position.shares * seniorPerShare
      Junior -> position.shares * juniorPerShare
      
  in position { value = newValue }

-- Get the exposure multiplier for a tranche
getTrancheMultiplier :: Tranche -> Number
getTrancheMultiplier tranche = case tranche of
  Senior -> 1.0  -- Protected exposure
  Junior -> 3.0  -- Fixed 3x exposure for MVP
```

### 3. Position Creation with Tranches and Terms

All positions choose a tranche (risk level) and term:

```purescript
-- Create a new position with all required parameters
createPosition :: 
  PositionId ->     -- Unique position identifier
  PoolId ->         -- Pool this position belongs to
  String ->         -- Position owner
  Number ->         -- Initial investment amount
  Tranche ->        -- Senior (1x) or Junior (3x) tranche
  TermCommitment -> -- Term commitment (Spot or Monthly)
  Boolean ->        -- Auto-rollover setting
  ShareAmount ->    -- Shares allocated by pool (calculated elsewhere)
  BlockNumber ->    -- Current block number
  Position
createPosition id poolId owner amount tranche term rollover shares currentBlock =
  { id
  , poolId
  , owner
  , amount
  , tranche
  , term
  , rollover
  , shares
  , createdAt: currentBlock
  , value: amount  -- Initial value equals amount invested
  , lockedAmount: 0.0
  }
```

### 4. Dual Token Issuance

Issuance of FeelsSOL is handled by `Protocol.FeelsSOL.purs` based on external oracle prices. Issuance of user-created tokens (Feels Assets) is handled by `Protocol.Offering.purs` through phased token launches and liquidity injection into pools.

-- The following sections (5, 6, 7) are conceptual and not directly implemented as described.
-- 5. Term-Aware Liquidity Distribution
-- 6. Complete Pool Update
-- 7. Floor Mechanism


## Benefits of Vertically Integrated Design

### 1. Conceptual Unification
- **Everything is Lending**: All liquidity provision is a position/lending operation
- **Staking = Managed Lending**: Users lend liquidity to protocol's distribution algorithm
- **Spot Orders = Direct Lending**: Users lend at specific price points
- **Issuance = POL Growth**: Minted tokens become protocol-owned liquidity

### 2. Market Stability
- Protocol can dampen volatility by counter-issuing into momentum (conceptual)
- Prevent death spirals through strategic burning (conceptual)
- Maintain healthy spreads through liquidity injection (conceptual)

### 3. Value Capture
- Protocol captures seigniorage from token creation (conceptual)
- Issuance fees contribute to POL growth (conceptual)
- Reduced need for external liquidity incentives (conceptual)

### 4. Superior Execution
- Always sufficient liquidity through issuance (conceptual)
- Tighter spreads from protocol market making (conceptual)
- Reduced slippage for large trades (conceptual)

### 5. Manipulation Resistance
- Harder to manipulate when protocol can counter-issue (conceptual)
- Floor based on actual volume, not price wicks (conceptual)
- Multiple metrics prevent gaming (conceptual)

### 6. Capital Efficiency
- Liquidity appears where needed through issuance (conceptual)
- No idle capital in cold ticks (conceptual)
- Dynamic rebalancing without external LPs (conceptual)

## Implementation Considerations

-- The concepts of issuance controls and position management with issuance are handled
-- across various modules (e.g., Protocol.FeelsSOL.purs, Protocol.Offering.purs,
-- UI.Action.PositionActions.purs, Protocol.Position.purs) and are not implemented
-- as single, unified functions or data structures as described below.

### Issuance Controls

```purescript
-- Prevent runaway inflation/deflation
validateIssuance :: IssuanceAmount -> IssuanceController -> Boolean
validateIssuance amount controller =
  let dailyGrowth = abs amount / currentSupply
      liquidityCheck = currentLiquidity > controller.minLiquidity
      growthCheck = dailyGrowth < controller.maxSupplyGrowth
      
  in liquidityCheck && growthCheck

-- Adaptive rules based on effectiveness
updateIssuanceController :: IssuanceMetrics -> IssuanceController -> IssuanceController
updateIssuanceController metrics controller =
  let effectiveness = measureIssuanceEffectiveness metrics
      
      newMaxGrowth = if effectiveness > 0.7
                     then min 0.05 (controller.maxSupplyGrowth * 1.1)
                     else max 0.001 (controller.maxSupplyGrowth * 0.9)
      
  in controller { maxSupplyGrowth = newMaxGrowth }
```

### Position Management with Issuance

```purescript
type PositionBook =
  { positions :: Map PositionId Position
  , nextId :: PositionId
  , totalManaged :: Number          -- Sum of managed positions
  , totalSpot :: Number             -- Sum of spot positions
  , lockedValue :: Number           -- Value locked below floor
  }

-- Close/withdraw position
closePosition :: PositionId -> PoolState -> (Number, PoolState)
closePosition posId pool =
  case Map.lookup posId pool.positions.positions of
    Nothing -> (0.0, pool)
    Just position ->
      case position.positionType of
        ManagedPosition terms ->
          -- Check if unlocked
          case terms.lockedUntil of
            Just unlock if currentTime < unlock -> (0.0, pool)  -- Still locked
            _ ->
              -- Calculate current value including fees
              let shareValue = (position.shares / pool.managed.totalShares) * 
                              (pool.managed.total + getAccumulatedFees pool)
                  
                  -- Remove from managed liquidity
                  newManaged = pool.managed
                    { total = pool.managed.total - shareValue
                    , totalShares = pool.managed.totalShares - position.shares
                    }
                  
                  -- Redistribute remaining managed liquidity
                  newTicks = distributeManagedLiquidity newManaged pool.aggregate pool.tickBook.ticks
                  
              in (shareValue, updatePool pool newManaged newTicks)
        
        SpotPosition terms ->
          -- Remove from specific tick
          let newTicks = updateAt terms.tickIndex
                (\tick -> tick { liquidity = tick.liquidity - position.amount })
                pool.tickBook.ticks
              
          in (position.value, pool { tickBook = pool.tickBook { ticks = newTicks } })
```

## Decision

We have adopted the Pool-Centric Unified Tick System with Vertically Integrated Issuance as it provides unprecedented market stability, capital efficiency, and value capture while maintaining security and simplicity for users. The ability to control both tokens in each pair gives the protocol sovereign market-making capabilities that traditional AMMs cannot match.

## Appendix: Complete Pool Lifecycle

-- The functions and types described in this section are conceptual and do not directly
-- reflect the current implementation. Pool initialization and updates are handled
-- across various modules (e.g., UI.PoolRegistry.purs, Protocol.Pool.purs, UI.Command.purs)
-- in a distributed manner.
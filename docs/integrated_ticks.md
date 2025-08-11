# Architecture Decision Record: Pool-Centric Unified Tick System with Vertically Integrated Issuance

**Status**: Proposed  
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
  { pair :: TokenPair                -- (FeelsSOL, Token)
  , tickBook :: TickBook            -- Living ticks with metrics
  , aggregate :: AggregateMetrics   -- Pool-wide metrics
  , positions :: PositionBook       -- ALL positions (unified)
  , managed :: ManagedLiquidity     -- Protocol-managed liquidity with tranches
  , issuance :: IssuanceState       -- Dual token minting/burning
  , termSchedule :: TermSchedule    -- Term expiry tracking
  , trancheMetrics :: TrancheMetrics -- Pool-wide tranche health
  }

type TermSchedule =
  { nextHourly :: Timestamp
  , nextDaily :: Timestamp
  , nextWeekly :: Timestamp
  , pendingExpiries :: Array (Timestamp, Array PositionId)
  }

type TrancheMetrics =
  { seniorTotal :: Number           -- Total senior capital
  , juniorTotal :: Number           -- Total junior capital
  , juniorMultiplier :: Number      -- Current junior multiplier (1.5x to 3x)
  , totalExposure :: Number         -- Total market exposure
  , juniorHealth :: Number          -- Junior tranche health (0 to 1)
  }

type TokenPair =
  { base :: FeelsSOL               -- Always FeelsSOL
  , quote :: Token                 -- The paired token
  }
```

### Living Tick System (Simplified)

```purescript
type Tick =
  { price :: Number
  , liquidity :: Number            -- Total liquidity at this price
  , volume24h :: Number            -- Rolling 24h volume
  , events :: RingBuffer Event    -- Recent activity
  , metrics :: TickMetrics         -- Derived local metrics
  , state :: TickState            -- Active/FloorLocked/FloorBoundary
  }

type TickMetrics =
  { support :: Number              -- Buy volume - sell volume
  , temperature :: Number          -- Activity level (hot/cold)
  , reliability :: Number          -- Consistency of liquidity provision
  , velocity :: Number             -- Rate of liquidity change
  }
```

### Unified Position System with Terms and Tranches

```purescript
type Position =
  { id :: PositionId
  , owner :: UserId
  , amount :: Number                -- Initial capital
  , tranche :: Tranche             -- Senior (1x) or Junior (3x)
  , term :: TermCommitment         -- Synchronized term for all positions
  , shares :: Number               -- Shares within their tranche
  , createdAt :: Timestamp
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount below floor
  }

data Tranche
  = Senior    -- 1x exposure, protected, lower yield
  | Junior    -- 3x exposure, first loss, higher yield

data TermCommitment
  = Spot                           -- Perpetual, no expiry
  | Hourly Timestamp               -- Expires on the hour
  | Daily Timestamp                -- Expires at 00:00 UTC
  | Weekly Timestamp               -- Expires Sunday 00:00 UTC

type ManagedLiquidity =
  { total :: Number                -- Total protocol-managed liquidity
  , senior :: TrancheBucket        -- Senior positions
  , junior :: TrancheBucket        -- Junior positions
  , pol :: Number                  -- Protocol-owned portion
  , distribution :: Distribution   -- How it's distributed across ticks
  , termBuckets :: Map TermType TermBucket  -- Organize by term
  }

type TrancheBucket =
  { amount :: Number               -- Total capital in this tranche
  , shares :: Number               -- Total shares issued
  , multiplier :: Number           -- Current exposure multiplier (1x for senior, up to 3x for junior)
  }

type TermBucket =
  { termType :: TermType
  , amount :: Number
  , positions :: Array PositionId
  , expiry :: Maybe Timestamp
  }
```

### Vertically Integrated Issuance

```purescript
type IssuanceState =
  { feelsSolOracle :: Number       -- External JitoSOL/SOL rate
  , tokenSupply :: Number          -- Current supply of quote token
  , lastIssuance :: Timestamp      
  , issuanceMetrics :: IssuanceMetrics
  , controller :: IssuanceController
  }

type IssuanceMetrics =
  { demandPressure :: Number       -- Buy vs sell imbalance
  , velocity :: Number             -- Trading volume / supply
  , depthRatio :: Number          -- Liquidity / market cap
  , priceDeviation :: Number      -- Distance from target
  }

type IssuanceController =
  { maxSupplyGrowth :: Number      -- Max % supply increase per day
  , minLiquidity :: Number         -- Min liquidity before issuance
  , targetUtilization :: Number    -- Target capital efficiency
  , priceStabilization :: Boolean  -- Whether to stabilize price
  }
```

### Data Flow

```
MarketEvent 
    ↓
Update Tick State (local)
    ↓
Update Pool Aggregates (incremental)
    ↓
Update POL Strategy (smooth)
    ↓
Check Issuance Needs (both tokens)
    ↓
Apply Changes to Ticks
    ↓
Update Position Values
```

## Key Mechanisms

### 1. Synchronized Term System

All positions align to global term boundaries for predictable liquidity events:

```purescript
-- Get next synchronized expiry time
getNextExpiry :: TermCommitment -> Timestamp -> Timestamp
getNextExpiry term currentTime =
  case term of
    Spot -> MAX_TIMESTAMP           -- Never expires
    Hourly _ -> nextHourBoundary currentTime
    Daily _ -> nextMidnightUTC currentTime  
    Weekly _ -> nextSundayMidnightUTC currentTime

-- Process all expired positions at once
processTermExpiries :: PoolState -> PoolState
processTermExpiries pool =
  let now = currentTime
      
      -- Find all expired positions
      (expired, active) = partition (isExpired now) (Map.values pool.positions.positions)
      
      -- Group by term type for batch processing
      expiredByTerm = groupBy getTermType expired
      
      -- Roll expired positions to spot
      rolledPositions = map rollToSpot expired
      
      -- Update position book
      newPositions = foldl (\book pos -> 
        Map.insert pos.id (rollToSpot pos) book
      ) pool.positions.positions rolledPositions
      
  in pool { positions = pool.positions { positions = newPositions } }

-- Roll expired position to spot
rollToSpot :: Position -> Position
rollToSpot position =
  position 
    { term = Spot
    , leverage = case position.leverage of
        Nothing -> Nothing
        Just lev -> Just lev { decayRate = lev.decayRate * 10.0 }  -- Accelerate decay
    }
```

### 2. Two-Tranche Leverage Model

Junior tranche provides leverage while protecting senior tranche:

```purescript
-- When distributing liquidity, junior provides 3x exposure
distributeManagedLiquidity :: ManagedLiquidity -> AggregateMetrics -> Array Tick -> Array Tick
distributeManagedLiquidity managed metrics ticks =
  let -- Junior tranche provides up to 3x their capital in exposure
      juniorExposure = managed.junior.amount * managed.junior.multiplier
      seniorExposure = managed.senior.amount * 1.0
      
      -- Total liquidity to distribute
      totalLiquidity = juniorExposure + seniorExposure + managed.pol
      
      -- Standard distribution across ticks
      distribution = calculateDistribution metrics totalLiquidity
      
  in applyDistribution distribution ticks

-- When calculating returns, junior absorbs losses first
calculateTrancheReturns :: PoolPnL -> ManagedLiquidity -> TrancheReturns
calculateTrancheReturns pnl managed =
  let if pnl.amount > 0 then
        -- Profits: Junior gets enhanced share (3x weight)
        let juniorWeight = managed.junior.amount * 3.0
            seniorWeight = managed.senior.amount * 1.0
            totalWeight = juniorWeight + seniorWeight
            
        in { senior: pnl.amount * (seniorWeight / totalWeight)
           , junior: pnl.amount * (juniorWeight / totalWeight)
           }
      else
        -- Losses: Junior takes first loss up to 90% of their capital
        let maxJuniorLoss = managed.junior.amount * 0.9
            juniorLoss = min maxJuniorLoss (abs pnl.amount)
            seniorLoss = max 0 (abs pnl.amount - juniorLoss)
            
        in { senior: -seniorLoss
           , junior: -juniorLoss
           }

-- Dynamic multiplier adjustment (never liquidates)
calculateJuniorMultiplier :: PoolState -> Number
calculateJuniorMultiplier pool =
  let baseMultiplier = 3.0
      juniorRatio = pool.managed.junior.amount / 
                   (pool.managed.junior.amount + pool.managed.senior.amount)
      
      -- Reduce multiplier as junior tranche depletes
      healthAdjustment = 
        if juniorRatio < 0.1 then 0.5      -- Minimum 1.5x when depleted
        else if juniorRatio < 0.2 then 0.7  -- Reduced to 2.1x
        else 1.0                            -- Full 3x when healthy
      
  in baseMultiplier * healthAdjustment

-- Update tranches after market moves
updateTranches :: PoolState -> PoolState
updateTranches pool =
  let currentValue = calculatePoolValue pool
      initialValue = pool.managed.senior.amount + pool.managed.junior.amount
      pnl = currentValue - initialValue
      
      returns = calculateTrancheReturns pnl pool.managed
      
      -- Update tranche values (junior never goes to zero)
      newSenior = pool.managed.senior 
        { amount = pool.managed.senior.amount + returns.senior }
      
      newJunior = pool.managed.junior
        { amount = max (pool.managed.junior.amount * 0.1)  -- Keep minimum 10%
                      (pool.managed.junior.amount + returns.junior)
        , multiplier = calculateJuniorMultiplier pool
        }
      
  in pool { managed = pool.managed { senior = newSenior, junior = newJunior } }
```

### 3. Position Creation with Terms and Leverage

### 3. Position Creation with Tranches and Terms

All positions choose a tranche (risk level) and term:

```purescript
-- Create position with tranche selection
createPosition :: UserId -> Number -> Tranche -> TermCommitment -> PoolState -> (Position, PoolState)
createPosition userId amount tranche term pool =
  let expiry = getNextExpiry term currentTime
      
      -- Get the appropriate bucket
      bucket = case tranche of
        Senior -> pool.managed.senior
        Junior -> pool.managed.junior
      
      -- Calculate shares within the tranche
      shares = if bucket.shares == 0.0
               then amount  -- First position sets baseline
               else amount * bucket.shares / bucket.amount
      
      -- Fee discount for longer terms
      termBonus = case term of
        Spot -> 1.0
        Hourly _ -> 1.05
        Daily _ -> 1.10
        Weekly _ -> 1.15
      
      -- Junior positions in longer terms get extra multiplier
      trancheBonus = case (tranche, term) of
        (Junior, Weekly _) -> 1.2   -- 3.6x instead of 3x
        (Junior, Daily _) -> 1.1    -- 3.3x instead of 3x
        (Senior, Weekly _) -> 1.05  -- Slight bonus for commitment
        _ -> 1.0
      
      adjustedShares = shares * termBonus * trancheBonus
      
      position = 
        { id: pool.positions.nextId
        , owner: userId
        , amount: amount
        , tranche: tranche
        , term: term
        , shares: adjustedShares
        , createdAt: currentTime
        , value: amount
        , lockedAmount: 0.0
        }
      
      -- Update the appropriate tranche
      newManaged = case tranche of
        Senior -> pool.managed 
          { senior = bucket 
            { amount = bucket.amount + amount
            , shares = bucket.shares + adjustedShares
            }
          }
        Junior -> pool.managed
          { junior = bucket 
            { amount = bucket.amount + amount
            , shares = bucket.shares + adjustedShares
            , multiplier = calculateJuniorMultiplier pool
            }
          }
      
      -- Redistribute total liquidity including new position
      newTicks = distributeManagedLiquidity newManaged pool.aggregate pool.tickBook.ticks
      
  in (position, pool { managed = newManaged, tickBook = pool.tickBook { ticks = newTicks } })

-- Fee distribution by tranche
distributeFees :: Number -> PoolState -> PoolState
distributeFees totalFees pool =
  let -- Base fee split (junior takes more risk, gets more reward)
      juniorFeeRate = 0.75  -- 75% of fee rate
      seniorFeeRate = 0.25  -- 25% of fee rate
      
      -- Weight by actual capital and exposure
      juniorExposure = pool.managed.junior.amount * pool.managed.junior.multiplier
      seniorExposure = pool.managed.senior.amount * 1.0
      totalExposure = juniorExposure + seniorExposure
      
      -- Calculate fee distribution
      juniorFees = totalFees * (juniorExposure / totalExposure) * juniorFeeRate
      seniorFees = totalFees * (seniorExposure / totalExposure) * seniorFeeRate
      
      -- Distribute to position holders by shares
      juniorPerShare = juniorFees / pool.managed.junior.shares
      seniorPerShare = seniorFees / pool.managed.senior.shares
      
      -- Update position values
      updatedPositions = Map.map (\pos ->
        let feeEarned = case pos.tranche of
              Junior -> pos.shares * juniorPerShare
              Senior -> pos.shares * seniorPerShare
        in pos { value = pos.value + feeEarned }
      ) pool.positions.positions
      
  in pool { positions = pool.positions { positions = updatedPositions } }
```

### 4. Dual Token Issuance

The protocol uniquely controls both tokens in each pair:

#### FeelsSOL Issuance (Oracle-Driven)
```purescript
-- FeelsSOL tracks external JitoSOL/SOL price
updateFeelsSolIssuance :: ExternalOracle -> PoolState -> PoolState
updateFeelsSolIssuance oracle pool =
  let deviation = (pool.aggregate.spot - oracle.jitoSolRate) / oracle.jitoSolRate
      
      action = 
        if deviation > 0.01 then          -- FeelsSOL too expensive
          MintFeelsSOL (deviation * ARBITRAGE_SIZE)
        else if deviation < -0.01 then    -- FeelsSOL too cheap
          BurnFeelsSOL (abs deviation * ARBITRAGE_SIZE)
        else NoAction
          
      -- Apply to specific ticks for market impact
      newTicks = applyIssuanceToTicks action pool.tickBook.ticks
      
  in pool { tickBook = pool.tickBook { ticks = newTicks } }
```

#### Quote Token Issuance (Market-Driven)
```purescript
-- Issue based on pool-specific metrics
updateTokenIssuance :: PoolState -> PoolState
updateTokenIssuance pool =
  let metrics = pool.issuance.issuanceMetrics
      
      -- Multiple signals for issuance decision
      issuanceAmount = 
        if metrics.velocity > HIGH_VELOCITY && metrics.demandPressure > 0 then
          -- High activity + demand: expand supply
          min (pool.issuance.tokenSupply * 0.001) (metrics.demandPressure * MULTIPLIER)
          
        else if metrics.demandPressure < -0.1 then
          -- Sell pressure: burn tokens to support price
          -(pool.issuance.tokenSupply * 0.0005)
          
        else if pool.aggregate.depth < MIN_DEPTH then
          -- Low liquidity: mint to provide depth
          calculateLiquidityNeed pool * 0.1
          
        else 0.0
      
      -- Add minted tokens to protocol-managed liquidity
      newManaged = if issuanceAmount > 0
        then pool.managed 
          { total = pool.managed.total + issuanceAmount
          , pol = pool.managed.pol + issuanceAmount  -- POL increases
          }
        else if issuanceAmount < 0
        then pool.managed 
          { total = pool.managed.total + issuanceAmount  -- Decrease
          , pol = max 0 (pool.managed.pol + issuanceAmount)
          }
        else pool.managed
      
      -- Redistribute all managed liquidity
      newTicks = distributeManagedLiquidity newManaged pool.aggregate pool.tickBook.ticks
      
  in pool { managed = newManaged, tickBook = pool.tickBook { ticks = newTicks } }
```

### 3. Liquidity Distribution

### 5. Term-Aware Liquidity Distribution

The protocol distributes managed liquidity considering term commitments:

```purescript
distributeWithTerms :: ManagedLiquidity -> AggregateMetrics -> Array Tick -> Array Tick
distributeWithTerms managed metrics ticks =
  let -- Aggregate liquidity by term
      spotLiquidity = getTermBucketAmount Spot managed.termBuckets
      hourlyLiquidity = getTermBucketAmount Hourly managed.termBuckets
      dailyLiquidity = getTermBucketAmount Daily managed.termBuckets
      weeklyLiquidity = getTermBucketAmount Weekly managed.termBuckets
      
      -- Different distribution strategies for different terms
      distributions = 
        [ (spotLiquidity, normalDistribution metrics.spot (metrics.volatility * 2.0))     -- Spot: wide
        , (hourlyLiquidity, normalDistribution metrics.spot (metrics.volatility * 0.5))   -- Hourly: tight
        , (dailyLiquidity, normalDistribution metrics.spot (metrics.volatility * 1.0))    -- Daily: medium
        , (weeklyLiquidity, normalDistribution metrics.spot (metrics.volatility * 3.0))   -- Weekly: wider
        ]
      
      -- Apply each distribution
      finalTicks = foldl (\ts (amount, dist) ->
        applyDistribution amount dist ts
      ) ticks distributions
      
  in finalTicks
```

### 6. Complete Pool Update

```purescript
-- Single atomic update for any trade
processEvent :: MarketEvent -> PoolState -> PoolState
processEvent event pool =
  let -- 1. Update affected tick
      tickIndex = findTickIndex event.price pool.tickBook
      oldTick = pool.tickBook.ticks !! tickIndex
      newTick = executeTrade event oldTick
      
      -- 2. Update pool aggregates incrementally
      newAggregate = updateAggregates event oldTick newTick pool.aggregate
      
      -- 3. Check for term expiries
      poolWithExpiries = if shouldCheckExpiries currentTime pool.termSchedule
                        then processTermExpiries pool
                        else pool
      
      -- 4. Update tranche values based on P&L
      poolWithTranches = updateTranches poolWithExpiries
      
      -- 5. Check if liquidity distribution needs updating
      shouldRedistribute = significantChange newAggregate pool.aggregate ||
                          trancheRebalanceNeeded poolWithTranches
      
      -- 6. Update issuance metrics
      newIssuance = updateIssuanceMetrics event poolWithTranches.issuance
      
      -- 7. Check if issuance needed (rate-limited)
      shouldIssue = currentTime - newIssuance.lastIssuance > ISSUANCE_INTERVAL
      
      -- 8. Apply changes
      finalPool = poolWithTranches
        # updateTick tickIndex newTick
        # updateAggregates newAggregate
        # (if shouldRedistribute then redistributeManaged else id)
        # (if shouldIssue then applyIssuance newIssuance else id)
      
  in finalPool

-- Check if tranche rebalancing needed
trancheRebalanceNeeded :: PoolState -> Boolean
trancheRebalanceNeeded pool =
  let juniorRatio = pool.managed.junior.amount / 
                   (pool.managed.junior.amount + pool.managed.senior.amount)
      
      -- Rebalance if junior depleted or multiplier changed significantly
  in juniorRatio < 0.15 || 
     abs (pool.managed.junior.multiplier - calculateJuniorMultiplier pool) > 0.2
```

### 7. Floor Mechanism

The floor emerges from actual market support rather than high-water marks:

```purescript
findFloorLevel :: PoolState -> Number
findFloorLevel pool =
  let -- Find ticks with proven support
      supportTicks = filter (\t -> 
        t.metrics.support > SUPPORT_THRESHOLD &&
        t.metrics.reliability > 0.8 &&
        t.price < pool.aggregate.spot
      ) pool.tickBook.ticks
      
      -- Conservative calculation
      volumeWeightedFloor = calculateVolumeWeightedSupport supportTicks
      timeWeightedFloor = pool.aggregate.spot * 0.5  -- 50% of TWAP
      feeBackedFloor = calculateFeeBackedFloor pool.pol.totalAmount
      
      -- Take minimum for safety
      floor = minimum [volumeWeightedFloor, timeWeightedFloor, feeBackedFloor]
      
      -- Floor only ratchets up
  in max pool.pol.floorLevel floor
```

## Benefits of Vertically Integrated Design

### 1. Conceptual Unification
- **Everything is Lending**: All liquidity provision is a position/lending operation
- **Staking = Managed Lending**: Users lend liquidity to protocol's distribution algorithm
- **Spot Orders = Direct Lending**: Users lend at specific price points
- **Issuance = POL Growth**: Minted tokens become protocol-owned liquidity

### 2. Market Stability
- Protocol can dampen volatility by counter-issuing into momentum
- Prevent death spirals through strategic burning
- Maintain healthy spreads through liquidity injection

### 2. Value Capture
- Protocol captures seigniorage from token creation
- Issuance fees contribute to POL growth
- Reduced need for external liquidity incentives

### 3. Superior Execution
- Always sufficient liquidity through issuance
- Tighter spreads from protocol market making
- Reduced slippage for large trades

### 4. Manipulation Resistance
- Harder to manipulate when protocol can counter-issue
- Floor based on actual volume, not price wicks
- Multiple metrics prevent gaming

### 5. Capital Efficiency
- Liquidity appears where needed through issuance
- No idle capital in cold ticks
- Dynamic rebalancing without external LPs

## Implementation Considerations

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

We recommend implementing the Pool-Centric Unified Tick System with Vertically Integrated Issuance as it provides unprecedented market stability, capital efficiency, and value capture while maintaining security and simplicity for users. The ability to control both tokens in each pair gives the protocol sovereign market-making capabilities that traditional AMMs cannot match.

## Appendix: Complete Pool Lifecycle

```purescript
-- Initialize a new pool
initializePool :: Token -> Number -> PoolState
initializePool token initialLiquidity =
  { pair: { base: FeelsSOL, quote: token }
  , tickBook: createInitialTicks token
  , aggregate: initializeAggregates
  , positions: 
    { positions: Map.empty
    , nextId: 1
    , totalManaged: 0.0
    , totalSpot: 0.0
    , lockedValue: 0.0
    }
  , managed:
    { total: initialLiquidity * 0.1  -- Start with 10% POL
    , totalShares: 0.0
    , pol: initialLiquidity * 0.1
    , distribution: defaultDistribution
    }
  , issuance: 
    { feelsSolOracle: 1.0
    , tokenSupply: INITIAL_SUPPLY
    , lastIssuance: currentTime
    , issuanceMetrics: initializeMetrics
    , controller: defaultController
    }
  }

-- Main update loop
updatePool :: Array MarketEvent -> PoolState -> PoolState
updatePool events pool =
  let -- Process all events atomically
      poolAfterEvents = foldl processEvent pool events
      
      -- Update floor level based on proven support
      poolWithFloor = updateFloorLevel poolAfterEvents
      
      -- Check external oracle for FeelsSOL
      poolWithOracle = updateFeelsSolIssuance getOraclePrice poolWithFloor
      
      -- Lock positions below floor
      finalPool = updatePositionLocks poolWithOracle
      
  in finalPool

-- User interaction example
userFlow :: UserId -> PoolState -> Effect PoolState
userFlow userId pool = do
  -- User creates managed position (stakes)
  let (position1, pool1) = createManagedPosition userId 1000.0 OneWeek pool
  
  -- User creates spot position (limit order)
  let (position2, pool2) = createSpotPosition userId 1.05 500.0 Sell pool1
  
  -- Time passes, user withdraws managed position
  let (value, pool3) = closePosition position1.id pool2
  
  pure pool3
```
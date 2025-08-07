# Lending Model with Adaptive Bands and Synchronized Terms

## Context

The Feels Protocol requires a lending model that integrates with spot trading and leverage while maintaining the core principle of liquidation-free positions. The current fixed-tick system creates friction for lenders who need to constantly reposition to maintain competitive rates, while the individualized staking periods fragment liquidity across temporal boundaries.

The protocol must support three core financial primitives within a single position type: spot swaps (infinite duration), term lending (fixed duration), and leveraged positions (amplified exposure). Each primitive has different requirements for price granularity, time commitment, and risk management, yet they must interact seamlessly within the same liquidity pools.

Additionally, market conditions require positions to adapt to price movements without breaking time commitments. Static positions become stale quickly, reducing capital efficiency and creating poor user experience for passive liquidity providers.

## Decision

We will implement a unified model based on adaptive price bands and synchronized global terms. This model treats all positions as points in a three-dimensional space of price (bands/ticks), time (terms), and leverage (multiplier).

### Core Position Structure

```purescript
type Position = {
  -- Core parameters
  amount :: Amount,
  tokenPair :: TokenPair,
  
  -- Price dimension
  priceStrategy :: PriceStrategy,
  
  -- Time dimension  
  term :: TermCommitment,
  
  -- Leverage dimension
  leverageConfig :: LeverageConfig
}

data PriceStrategy
  = BandAligned AdaptiveBand           -- Simplified, auto-adjusting
  | TickSpecific TickRange             -- Granular, fixed
  | BandConstrained AdaptiveBand TickRange  -- Hybrid approach

data AdaptiveBand = AdaptiveBand {
  tier :: BandTier,                    -- Tight/Medium/Wide
  centerTracking :: TrackingMode,      -- TWAP/Spot/Midpoint
  adaptiveWidth :: Bool,               -- Adjust to volatility
  lastUpdate :: Timestamp
}

data TermCommitment
  = Spot                               -- No expiry (∞ duration)
  | Hourly Timestamp                   -- Expires on the hour
  | Daily Timestamp                    -- Expires at 00:00 UTC
  | Weekly Timestamp                   -- Expires Sunday 00:00 UTC

data LeverageConfig = LeverageConfig {
  targetLeverage :: Number,            -- Desired leverage (1.0 - 10.0)
  mode :: LeverageMode,
  decayAfterTerm :: Bool               -- Whether to decay post-expiry
}

data LeverageMode
  = Static                             -- Fixed leverage
  | Dynamic HealthFactor               -- Adjusts with pool health
```

### Adaptive Band Mechanics

Bands automatically track market prices and adjust to volatility, eliminating manual rebalancing needs while maintaining time commitments:

```purescript
updateAdaptiveBand :: PoolState -> Position -> Position
updateAdaptiveBand pool pos@{priceStrategy: BandAligned band} =
  let currentPrice = getTrackingPrice pool band.centerTracking
      volatility = pool.impliedVolatility
      
      -- Base width from tier
      baseWidth = case band.tier of
        TightBand -> 0.01    -- ±1%
        MediumBand -> 0.05   -- ±5%
        WideBand -> 0.10     -- ±10%
      
      -- Adjust for volatility if enabled
      effectiveWidth = if band.adaptiveWidth
        then baseWidth * sqrt(volatility / baselineVol)
        else baseWidth
      
      -- Calculate new tick bounds
      lowerPrice = currentPrice * (1 - effectiveWidth)
      upperPrice = currentPrice * (1 + effectiveWidth)
      
  in pos { priceStrategy = BandAligned $ band {
    lastUpdate = currentTime,
    cachedBounds = (priceToTick lowerPrice, priceToTick upperPrice)
  }}
```

### Unified Financial Primitives

The three parameters create eight possible position types, with three pure primitives. Each parameter can be either "active" (configured with specific constraints) or "passive" (using default values). Active means: Price = concentrated in specific band/ticks (vs full range), Time = locked to term (vs spot/infinite), Leverage = >1.0x (vs standard 1.0x). This yields 2³ = 8 combinations:

**Spot Trading** (Price only):
- Term: Spot (infinite)
- Leverage: 1.0x
- Price: Any strategy
- Use case: Immediate liquidity provision

**Term Lending** (Time only):
- Term: Hourly/Daily/Weekly
- Leverage: 1.0x
- Price: Typically band-aligned
- Use case: Fixed-rate lending

**Leveraged Exposure** (Leverage only):
- Term: Spot
- Leverage: >1.0x
- Price: Typically wide bands
- Use case: Amplified price exposure

Combined positions activate multiple dimensions simultaneously, creating hybrid instruments like leveraged term loans or time-locked limit orders.

### Term Synchronization and Rollover

All positions with the same term type expire simultaneously at predictable times:

```purescript
getNextExpiry :: TermCommitment -> Timestamp -> Timestamp
getNextExpiry term currentTime =
  case term of
    Spot -> MaxTimestamp           -- Never expires
    Hourly _ -> nextHourBoundary currentTime
    Daily _ -> nextMidnightUTC currentTime  
    Weekly _ -> nextSundayMidnightUTC currentTime

-- Automatic rollover at expiry
handleTermExpiry :: Position -> PoolState -> Position
handleTermExpiry pos pool =
  let expired = currentTime >= getExpiry pos.term
  in if expired
     then pos { 
       term = Spot,  -- Becomes spot liquidity
       -- Leverage decays if configured
       leverageConfig = maybeDecayLeverage pos.leverageConfig
     }
     else pos
```

### Granularity Design

The system supports multiple granularity levels to serve different user sophistication:

**Band-level** (Default): Simplified interface where users select from three bands. Positions automatically track market prices within their band. Lower fees incentivize this choice.

**Tick-level** (Advanced): Professional users can specify exact tick ranges. Higher fees reflect increased system complexity.

**Hybrid** (Balanced): Tick-level precision constrained within band boundaries. Moderate fee tier.

### Fee Structure

Fees incorporate three factors with multiplicative effects:

```purescript
calculateFee :: Position -> PoolState -> Fee
calculateFee pos pool =
  let baseFee = pool.baseFeeRate
      
      -- Granularity multiplier
      granularityMult = case pos.priceStrategy of
        BandAligned _ -> 0.9        -- 10% discount
        BandConstrained _ _ -> 0.95 -- 5% discount
        TickSpecific _ -> 1.0       -- Full rate
      
      -- Term multiplier (rewards commitment)
      termMult = case pos.term of
        Spot -> 1.05                -- 5% premium
        Hourly _ -> 1.0             -- Baseline
        Daily _ -> 0.95             -- 5% discount
        Weekly _ -> 0.9             -- 10% discount
      
      -- Leverage risk multiplier
      leverageMult = 1.0 + (pos.leverageConfig.targetLeverage - 1.0) * 0.1
      
  in baseFee * granularityMult * termMult * leverageMult
```

### Protocol and User Liquidity Interaction

The protocol maintains base liquidity across bands while user positions provide dynamic depth:

```purescript
type BandLiquidity = {
  protocolOwned :: Amount,           -- Stable foundation
  userStaked :: Map TermCommitment Amount,  -- Term-separated user liquidity
  totalDepth :: Amount               -- Combined available
}

-- Protocol liquidity allocation strategy
allocateProtocolLiquidity :: PoolState -> Map BandTier Amount
allocateProtocolLiquidity pool =
  let total = pool.protocolOwnedLiquidity
      -- Counterbalance user preferences
      userDist = getUserDistribution pool
  in Map.fromList [
    (TightBand, total * 0.2 * (1.0 / (1.0 + userDist.tight))),
    (MediumBand, total * 0.5 * (1.0 / (1.0 + userDist.medium))),
    (WideBand, total * 0.3 * (1.0 / (1.0 + userDist.wide)))
  ]
```

This creates a self-balancing system where protocol liquidity provides stability while user positions add responsive depth. The protocol automatically rebalances to maintain risk equilibrium across bands.

## Consequences

**Positive outcomes** include elimination of manual rebalancing through adaptive bands, improved liquidity depth via synchronized term expiries, and seamless integration of all financial primitives within one position type. The progressive complexity model serves both retail and professional users effectively. Natural incentive alignment encourages behaviors that benefit the protocol.

**Challenges** involve increased computational complexity for band updates and price tracking. Gas costs may rise for frequent automated adjustments, though batch processing can mitigate this. The system requires reliable price oracles for accurate band tracking. Users need education on the new term synchronization model.

The unified model simplifies the codebase by eliminating special cases for different financial primitives while providing more flexibility than traditional siloed approaches. Market efficiency improves as all liquidity contributes to price discovery regardless of user intent.

### Liquidation-Free Leverage Design

Leveraged positions maintain value without liquidation risk through dynamic adjustment:

```purescript
-- Pool health calculation
calculatePoolHealth :: PoolState -> HealthFactor
calculatePoolHealth pool =
  let totalValue = pool.protocolOwnedLiquidity + pool.userStakedValue
      obligations = calculateTermObligations pool
  in totalValue / obligations

-- Dynamic leverage adjustment based on pool health
adjustLeverageForHealth :: Position -> HealthFactor -> Position
adjustLeverageForHealth pos health =
  case pos.leverageConfig.mode of
    Static -> pos  -- User chose fixed leverage
    Dynamic healthBased ->
      let stressAdjustment = max 0.5 (min 1.0 health)
          effectiveLeverage = pos.leverageConfig.targetLeverage * stressAdjustment
      in pos { leverageConfig = pos.leverageConfig { 
        currentLeverage = effectiveLeverage 
      }}
```

During stress periods (health factor < 1.2), the protocol protects all positions through graduated responses. Adaptive bands automatically widen to reduce price risk. Leverage dynamically reduces but never liquidates. Returns prioritize longer-term positions (senior claims). All positions maintain redeemable value, preventing total loss.

This creates sustainable leverage without cascade liquidation risks, as positions represent ownership shares rather than debt obligations.
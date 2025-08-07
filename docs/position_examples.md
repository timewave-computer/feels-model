# Position Examples

## Common Position Types

This guide provides examples of common position types in the Feels Protocol, demonstrating how to combine price strategies, term commitments, and leverage.

## 1. Basic Positions

### Simple Spot Lender
```purescript
-- Most basic position: lend at current market rates
position = createSpotPosition
  { id: 1
  , owner: "alice.sol"
  , amount: 1000.0
  , tokenPair: { base: FeelsSOL, quote: JitoSOL }
  , bandTier: MediumBand
  , timestamp: currentTime
  }

-- Characteristics:
-- - No term commitment (perpetual)
-- - 5% band width for flexibility
-- - No leverage (1x)
-- - 10% fee discount for band alignment
```

### Conservative Daily Lender
```purescript
-- Fixed daily term with wide bands for stability
position = createTermPosition
  { id: 2
  , owner: "bob.sol"
  , amount: 5000.0
  , tokenPair: { base: FeelsSOL, quote: USDC }
  , term: Daily (nextMidnightUTC currentTime)
  , timestamp: currentTime
  }

-- Characteristics:
-- - Daily term commitment
-- - Wide bands (default for term positions)
-- - Predictable returns
-- - 5% term discount on fees
```

## 2. Advanced Strategies

### Leveraged Yield Farmer
```purescript
-- Leveraged position with dynamic adjustment
position = createLeveragedPosition
  { id: 3
  , owner: "carol.sol"
  , amount: 10000.0
  , tokenPair: { base: FeelsSOL, quote: JitoSOL }
  , targetLeverage: 3.0
  , useDynamicMode: true
  , timestamp: currentTime
  }

-- Characteristics:
-- - 3x leverage
-- - Dynamic adjustment based on pool health
-- - Wide bands for leveraged positions
-- - Auto-deleveraging under stress
```

### Precision Market Maker
```purescript
-- Tick-specific positioning for professionals
position = createPosition
  { id: 4
  , owner: "david.sol"
  , amount: 50000.0
  , tokenPair: { base: FeelsSOL, quote: SOL }
  , priceStrategy: TickSpecific 
      { lowerTick: priceToTick 0.98
      , upperTick: priceToTick 1.02
      }
  , term: Spot
  , leverageConfig: 
      { targetLeverage: 1.0
      , currentLeverage: 1.0
      , mode: Static
      , decayAfterTerm: false
      }
  , timestamp: currentTime
  }

-- Characteristics:
-- - Exact tick range control
-- - No fee discounts
-- - Maximum capital efficiency
-- - Requires active management
```

## 3. Hybrid Approaches

### Band-Constrained Optimizer
```purescript
-- Best of both worlds: bands + custom range
position = createPosition
  { id: 5
  , owner: "eve.sol"
  , amount: 25000.0
  , tokenPair: { base: FeelsSOL, quote: JitoSOL }
  , priceStrategy: BandConstrained
      (initBand MediumBand)
      { lowerTick: priceToTick 0.97
      , upperTick: priceToTick 1.03
      }
  , term: Weekly (nextSundayMidnightUTC currentTime)
  , leverageConfig: staticLeverage 2.0
  , timestamp: currentTime
  }

-- Characteristics:
-- - Custom range within band limits
-- - 5% fee discount (hybrid)
-- - Weekly term for better rates
-- - Moderate 2x leverage
```

### Adaptive Volatility Trader
```purescript
-- Volatility-responsive position
position = createPosition
  { id: 6
  , owner: "frank.sol"
  , amount: 15000.0
  , tokenPair: { base: FeelsSOL, quote: USDT }
  , priceStrategy: BandAligned
      { tier: TightBand
      , centerTracking: TWAP
      , adaptiveWidth: true  -- Key feature
      , lastUpdate: currentTime
      , cachedBounds: Nothing
      }
  , term: Hourly (nextHourBoundary currentTime)
  , leverageConfig: dynamicLeverage 5.0
  , timestamp: currentTime
  }

-- Characteristics:
-- - Tight bands that widen with volatility
-- - TWAP tracking for stability
-- - Short hourly terms
-- - High leverage with dynamic adjustment
```

## 4. Portfolio Combinations

### Ladder Strategy
```purescript
-- Multiple positions with staggered terms
positions = 
  [ createDailyPosition 1000.0   -- Expires tomorrow
  , createWeeklyPosition 2000.0  -- Expires Sunday
  , createSpotPosition 3000.0    -- No expiry
  ]

-- Benefits:
-- - Consistent liquidity availability
-- - Averaged returns
-- - Reduced timing risk
```

### Barbell Strategy
```purescript
-- Combine conservative and aggressive
conservativePos = createTermPosition
  { amount: 8000.0
  , bandTier: WideBand
  , term: Weekly
  , leverage: 1.0
  }

aggressivePos = createLeveragedPosition
  { amount: 2000.0
  , bandTier: TightBand
  , targetLeverage: 5.0
  , useDynamicMode: true
  }

-- 80/20 split between safety and growth
```

## 5. Special Situations

### Stress-Responsive Position
```purescript
-- Automatically adapts to protocol stress
position = createPosition
  { priceStrategy: BandAligned (adaptiveBand MediumBand)
  , leverageConfig: 
      { targetLeverage: 4.0
      , currentLeverage: 4.0
      , mode: Dynamic 1.5  -- Health factor
      , decayAfterTerm: true
      }
  , term: Daily
  }

-- During stress:
-- - Bands widen automatically
-- - Leverage reduces dynamically
-- - Fees may increase
-- - Returns prioritized by seniority
```

### POL Counter-Position
```purescript
-- Position that benefits from POL counterbalancing
-- If most users are in tight bands, POL allocates to wide
position = createSpotPosition
  { amount: 20000.0
  , bandTier: WideBand  -- Where POL is concentrated
  , timestamp: currentTime
  }

-- Benefits:
-- - Better liquidity depth
-- - Potential POL rewards
-- - Reduced competition
```

## Helper Functions Reference

```purescript
-- Utility functions for common patterns
staticLeverage :: Number -> LeverageConfig
staticLeverage lev = 
  { targetLeverage: lev
  , currentLeverage: lev
  , mode: Static
  , decayAfterTerm: false
  }

dynamicLeverage :: Number -> LeverageConfig
dynamicLeverage lev =
  { targetLeverage: lev
  , currentLeverage: lev
  , mode: Dynamic 1.0
  , decayAfterTerm: true
  }

initBand :: BandTier -> AdaptiveBand
initBand tier =
  { tier: tier
  , centerTracking: SpotPrice
  , adaptiveWidth: true
  , lastUpdate: 0.0
  , cachedBounds: Nothing
  }

adaptiveBand :: BandTier -> AdaptiveBand
adaptiveBand tier =
  { tier: tier
  , centerTracking: TWAP
  , adaptiveWidth: true
  , lastUpdate: 0.0
  , cachedBounds: Nothing
  }
```

## Choosing the Right Position

| If you want... | Use position... |
|----------------|------------------------|
| Simple lending | Spot + Medium Band |
| Predictable returns | Daily/Weekly Term |
| Maximum efficiency | Tick-Specific |
| Higher returns | Add Leverage |
| Lower fees | Band-Aligned |
| Flexibility | Spot (no term) |
| Stability | Wide Bands + Terms |

# Term Synchronization Documentation

## Overview
The Feels Protocol uses synchronized global terms to ensure all positions with the same term commitment expire simultaneously. This creates predictable liquidity events and simplifies portfolio management.

## Term Types

### 1. Spot (Perpetual)
- **Duration**: Infinite (no expiry)
- **Use case**: Continuous liquidity provision
- **Rollover**: Not applicable
- **Key feature**: Positions remain active indefinitely

### 2. Hourly Terms
- **Duration**: Until next hour boundary
- **Expiry**: Every hour at :00 (e.g., 14:00, 15:00 UTC)
- **Use case**: Ultra-short term positions
- **Rollover**: Automatic conversion to spot

### 3. Daily Terms
- **Duration**: Until next midnight UTC
- **Expiry**: Daily at 00:00 UTC
- **Use case**: Day trading and short-term lending
- **Rollover**: Automatic conversion to spot

### 4. Weekly Terms
- **Duration**: Until next Sunday midnight UTC
- **Expiry**: Sundays at 00:00 UTC
- **Use case**: Medium-term positions
- **Rollover**: Automatic conversion to spot

## Synchronization Mechanism

### Global Expiry Events
All positions with the same term type expire together:
```
Block 1000: 13:59:55 UTC
- 50 hourly positions active

Block 1001: 14:00:00 UTC (TermExpiryCheck event)
- All 50 hourly positions expire simultaneously
- Positions automatically convert to spot
- New hourly term begins
```

### Batch Processing
The protocol processes all expiries in a single batch:
1. Clock triggers `TermExpiryCheck` event
2. PositionBook identifies all expired positions
3. Batch conversion to spot positions
4. Single state update for efficiency

## Position Lifecycle

### Creation
```purescript
-- Position created at 13:45 UTC
position = createTermPosition
  { amount: 1000.0
  , term: Hourly  -- Will expire at 14:00 UTC (15 minutes)
  }
```

### During Term
- Position earns term-specific rates
- Leverage remains stable
- No early withdrawal penalties

### At Expiry
1. Term commitment fulfilled
2. Position converts to spot automatically
3. User can:
   - Withdraw immediately
   - Re-commit to new term
   - Leave as spot position

## Benefits of Synchronization

### 1. Predictable Liquidity
- Known expiry times
- Concentrated liquidity events
- Better market depth at rollovers

### 2. Simplified Management
- No tracking individual expiries
- Batch operations possible
- Clear portfolio timelines

### 3. Gas Efficiency
- Single transaction for many expiries
- Reduced on-chain operations
- Lower costs for users

## Technical Implementation

### Time Calculation
```purescript
-- Get next hourly boundary
nextHourBoundary currentTime =
  let hourMs = 3600000  -- milliseconds in hour
      currentHour = floor (currentTime / hourMs)
  in (currentHour + 1) * hourMs

-- Get next daily boundary (midnight UTC)
nextMidnightUTC currentTime =
  let dayMs = 86400000  -- milliseconds in day
      currentDay = floor (currentTime / dayMs)
  in (currentDay + 1) * dayMs

-- Get next weekly boundary (Sunday midnight UTC)
nextSundayMidnightUTC currentTime =
  let weekMs = 604800000  -- milliseconds in week
      adjustedTime = currentTime + 345600000  -- Adjust for epoch
      currentWeek = floor (adjustedTime / weekMs)
  in (currentWeek + 1) * weekMs - 345600000
```

### State Transitions
```
Active Term Position → Expiry Event → Spot Position
     ↓                                      ↓
  Earning                              Can withdraw
  Term rate                            or re-commit
```

## Example Scenarios

### Scenario 1: Daily Lender
- Creates position Monday 10:00 UTC
- Position expires Tuesday 00:00 UTC (14 hours)
- Automatically becomes spot position
- Can withdraw or create new daily term

### Scenario 2: Weekly Yield Farmer
- Creates position Wednesday 15:00 UTC
- Position expires Sunday 00:00 UTC
- Earns weekly rate for full duration
- Higher returns for longer commitment

### Scenario 3: Hourly Arbitrageur
- Creates position at 14:30
- Expires at 15:00 (30 minutes)
- Quick turnaround for opportunities
- Can chain multiple hourly terms

## Best Practices

1. **Plan around expiries** - Know when your positions will expire
2. **Use appropriate terms** - Match term length to your strategy
3. **Consider gas costs** - Longer terms mean fewer transactions
4. **Monitor expiry events** - Opportunities often arise at rollovers
5. **Leverage term benefits** - Longer terms typically offer better rates

## FAQs

**Q: What happens if I miss my expiry?**
A: Positions automatically convert to spot. No penalties, just different rates.

**Q: Can I exit before term expiry?**
A: No early exits from term positions. Choose term length carefully.

**Q: Do all positions expire at once?**
A: Only positions with the same term type. Hourly, daily, and weekly expire separately.

**Q: What timezone is used?**
A: All times are in UTC for global consistency.
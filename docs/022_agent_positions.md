# Agent Positions and Preference Matrix

## Overview

This document maps market participants (agents) to their preferences across three dimensions and shows how these preferences translate into specific position types within the Feels Protocol. The system uses a 3D preference matrix to understand agent behaviors and design complementary position structures.

## Three-Dimensional Preference Matrix

Each agent type is characterized by their preferences/tolerances across three risk dimensions:

### 1. Time Preference → Credit Risk Tolerance
- **Long**: Tolerance for credit risk (willing to lock capital with counterparty)
- **Short**: Sensitivity to credit risk (needs immediate access to capital)

### 2. Leverage Preference → Directional Risk Appetite  
- **High**: Seeks magnified exposure to price movements
- **Low**: Avoids amplified directional risk

### 3. Liquidity Preference → Liquidity Risk Sensitivity
- **Strong**: Sensitivity to liquidity risk (needs deep markets, avoids slippage)
- **Weak**: Tolerance for liquidity risk (accepts wide spreads, thin markets)

## Agent Profiles in 3D Space

### Visualization
```
                    Liquidity Preference
                    Strong        Weak
                      │            │
    Time: Long    ┌───┼────────────┼───┐
    Lev: Low      │ Hodler     Issuer │
                  └───┼────────────┼───┘
                      │            │
    Time: Long    ┌───┼────────────┼───┐
    Lev: High     │Vol Harvester│   │
                  └───┼────────────┼───┘
                      │            │
    Time: Short   ┌───┼────────────┼───┐
    Lev: Low      │ Sniper        │   │
                  └───┼────────────┼───┘
                      │            │
    Time: Short   ┌───┼────────────┼───┐
    Lev: High     │          Momentum │
                  │           Trader  │
                  └───┼────────────┼───┘
```

### Detailed Agent Preferences

#### 1. **Issuers**
- **Time**: Long (tolerance for credit risk - permanent holders)
- **Leverage**: Low (avoid directional risk amplification)
- **Liquidity**: Weak (tolerance for liquidity risk - don't need exits)
- **Motivation**: Maximize long-term token appreciation
- **Behavior**: Stake to support protocol, provide foundational liquidity

#### 2. **Hodlers**
- **Time**: Long (tolerance for credit risk - months to years)
- **Leverage**: Low (avoid directional risk amplification)
- **Liquidity**: Strong (sensitivity to liquidity risk - want fair exit prices)
- **Motivation**: Steady appreciation with downside protection
- **Behavior**: Stake for yield, need liquid markets for eventual exit

#### 3. **Volatility Harvesters**
- **Time**: Long (tolerance for credit risk - willing to stake)
- **Leverage**: High (want to magnify fee yields)
- **Liquidity**: Strong (sensitivity to liquidity risk - need efficient markets)
- **Motivation**: Maximize fee yield while staying delta-neutral
- **Behavior**: Stake with enhanced fee multipliers to compensate for LVR

#### 4. **Momentum Traders**
- **Time**: Short (sensitivity to credit risk - need capital flexibility)
- **Leverage**: High (seek magnified directional exposure)
- **Liquidity**: Weak (tolerance for liquidity risk - accept slippage)
- **Motivation**: Capture short-term price movements with leverage
- **Behavior**: Use leveraged positions for amplified returns

#### 5. **Snipers**
- **Time**: Short (sensitivity to credit risk - immediate flips)
- **Leverage**: Low (already high risk from timing)
- **Liquidity**: Strong (sensitivity to liquidity risk - must exit at targets)
- **Motivation**: Exploit predictable price movements at launches
- **Behavior**: Provide initial liquidity and price discovery

## Position Construction

Based on agent preferences, positions are constructed using the protocol's parameters:

### Position Parameters
1. **Tranche**: Maps to both leverage preference and loss ordering
   - **Senior**: 1x exposure, protected from first losses
   - **Junior**: 3x exposure, absorbs first losses
2. **Duration**: Maps to time preference (credit risk tolerance)
   - **Spot**: Perpetual/infinite duration (can exit anytime but still lending)
   - **Monthly**: Fixed 28-day term (locked until expiry)
3. **Amount**: Capital committed to the position

**Important**: In the "everything is lending" model, even Spot positions are loans - they're just perpetual loans that can be recalled anytime, while Monthly positions are term loans that cannot be recalled early.

### How Preferences Map to Tranches

The tranche system bundles two concepts:
- **Leverage**: Senior = 1x, Junior = 3x
- **Loss Priority**: Junior takes first losses, protecting Senior

This creates natural agent-tranche alignment:
- **Low leverage preference** → Senior tranche (Issuers, Hodlers, Vol Harvesters)
- **High leverage preference** → Junior tranche (Momentum Traders)
- **Special cases**: Snipers (low leverage but may use Junior for other reasons)

### Agent-Specific Position Templates

#### Issuer Position
```purescript
issuerPosition = 
  { tranche: Senior          -- Low leverage preference (1x)
  , duration: Monthly        -- High credit risk tolerance
  , amount: largeAmount      -- Significant capital
  , purpose: "Long-term protocol support and appreciation"
  }
```

#### Hodler Position
```purescript
hodlerPosition = 
  { tranche: Senior          -- Low leverage preference (1x)
  , duration: Spot           -- Perpetual commitment (flexible exit)
  , amount: mediumAmount     -- Diversified allocation
  , purpose: "Steady yield with downside protection"
  }
```

#### Volatility Harvester Position
```purescript
volHarvesterPosition = 
  { tranche: Senior          -- Surprisingly, low leverage (1x)
  , duration: Monthly        -- High credit risk tolerance
  , amount: largeAmount      -- Maximize fee generation
  , feeMultiplier: 2.0       -- Enhanced fees compensate for LVR
  , purpose: "Fee farming with LVR compensation instead of leverage"
  }
```
*Note: Despite high leverage preference for yield, Vol Harvesters use Senior tranche because fee multipliers provide the yield enhancement without directional risk.

#### Momentum Trader Position
```purescript
momentumPosition = 
  { tranche: Junior          -- High leverage preference (3x)
  , duration: Spot           -- Low credit risk tolerance
  , amount: smallAmount      -- Limited risk capital
  , purpose: "Leveraged short-term trading"
  }
```

#### Sniper Position
```purescript
sniperPosition = 
  { tranche: Junior          -- Despite low leverage preference
  , duration: Spot           -- Low credit risk tolerance
  , amount: mediumAmount     -- Sized for impact
  , purpose: "Quick flips with first-mover advantage"
  }
```
*Note: Snipers may use Junior despite low leverage preference because they exit before losses accumulate.

## Complementary Dynamics

The system creates natural complementarities between agents:

### Credit Risk Dimension (Time)
```
HIGH TOLERANCE (Long)              LOW TOLERANCE (Short)
├── Issuers                        ├── Momentum Traders
├── Hodlers                        └── Snipers
└── Vol Harvesters                 

Provide: Patient capital           Provide: Trading volume
         Market stability                   Price discovery
         Long-term liquidity               Fee generation
```

### Directional Risk Dimension (Leverage) 
```
LOW APPETITE (1x)                  HIGH APPETITE (3x)
├── Issuers                        ├── Momentum Traders
├── Hodlers                        └── (Sometimes Snipers)
└── Vol Harvesters*                

Provide: Stable base               Provide: First-loss buffer
         Conservative liquidity             Leveraged activity
         Risk absorption                    Volume generation

*Vol Harvesters achieve yield through fees, not leverage
```

### Liquidity Risk Dimension
```
HIGH SENSITIVITY (Strong)          LOW SENSITIVITY (Weak)
├── Hodlers                        ├── Issuers  
├── Vol Harvesters                 ├── Momentum Traders
└── Snipers                        └── (Locked positions)

Need: Deep markets                 Accept: Wide spreads
      Fair pricing                         Slippage
      Quick exits                          Illiquidity
```

## LVR Protection for Volatility Harvesters

Since Vol Harvesters cannot actively rebalance their staked positions, the protocol provides several protection mechanisms:

1. **Enhanced Fee Share**: 2x base fee rate for monthly stakes
2. **Senior Tranche Priority**: Protected from first losses up to Junior buffer
3. **POL Buffer**: Protocol-owned liquidity absorbs arbitrage losses
4. **Duration Bonus**: Longer stakes receive exponentially better terms

## Market Equilibrium

The system naturally balances when:
- Sufficient stakers provide base liquidity (Issuers, Hodlers, Vol Harvesters)
- Active traders generate volume and fees (Momentum Traders, Snipers)
- Senior positions provide stability while Junior positions absorb volatility
- Each agent type finds their complementary partners in the ecosystem

This 3D preference model ensures that every participant can find a position type that matches their specific needs while contributing to overall market health.
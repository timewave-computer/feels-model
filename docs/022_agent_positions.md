# Agent Positions and Preference Matrix

## Overview

This document maps market participants (agents) to their preferences across three dimensions and shows how these preferences translate into specific position types within the Feels Protocol. The system uses a 3D preference matrix to understand agent behaviors and design complementary position structures.

## Three-Dimensional Preference Matrix

Each agent type is characterized by their preferences/tolerances across three risk dimensions:

### 1. Time Tolerance → Credit Risk Sensitivity
- **Infinite**: Theoretical tolerance for unbounded credit risk (Spot liquidity providers)
- **High**: Tolerance for credit risk (willing to lend for 28-day terms)
- **Low**: Sensitivity to credit risk (requires immediate return via Flash loans)

### 2. Leverage Tolerance → Principal Volatility Sensitivity
- **High**: Tolerance for principal volatility (accepts 3x price movements in both directions)
- **Low**: Sensitivity to principal volatility (prefers stable 1x exposure)

### 3. Liquidity Tolerance → Liquidity Risk Sensitivity
- **High**: Tolerance for liquidity risk (willing to hold/lend illiquid tokens)
- **Low**: Sensitivity to liquidity risk (prefers to hold/lend liquid FeelsSOL)

## Agent Profiles in 3D Space

### Visualization
```
                    Liquidity Tolerance
                    Low          High
                      │            │
    Time: High    ┌───┼────────────┼───┐
    Lev: Low      │Vol Harvester  Hodler
                  │   │           Issuer
                  └───┼────────────┼───┘
                      │            │
    Time: Low     ┌───┼────────────┼───┐
    Lev: Low      │ Sniper         │   │
                  └───┼────────────┼───┘
                      │            │
    Time: Low     ┌───┼────────────┼───┐
    Lev: High     │ Momentum Trader│   │
                  └───┼────────────┼───┘
```

### Detailed Agent Preferences

#### 1. **Issuers**
- **Time**: High tolerance for credit risk (28-day term lenders)
- **Leverage**: Low tolerance for principal volatility (prefer stable value)
- **Liquidity**: High tolerance for liquidity risk (don't need exits)
- **Motivation**: Maximize long-term token appreciation
- **Behavior**: Stake to support protocol, provide foundational liquidity

#### 2. **Hodlers**
- **Time**: High tolerance for credit risk (willing to lend for 28-day terms)
- **Leverage**: Low tolerance for principal volatility (seek predictable returns)
- **Liquidity**: High tolerance for liquidity risk (willing to hold illiquid tokens)
- **Motivation**: Steady appreciation with downside protection
- **Behavior**: Stake tokens for yield, accept illiquidity for returns

#### 3. **Volatility Harvesters**
- **Time**: High tolerance for credit risk (28-day term lenders)
- **Leverage**: Low tolerance for principal volatility (seek delta-neutral positions)
- **Liquidity**: Low tolerance for liquidity risk (need efficient markets)
- **Motivation**: Maximize fee yield while staying delta-neutral
- **Behavior**: Commit FeelsSOL to the protocol for a 28-day rollover term, receiving LVR compensation and eligibility for volatility yield.

#### 4. **Momentum Traders**
- **Time**: Low tolerance for credit risk (need capital flexibility)
- **Leverage**: High tolerance for principal volatility (accept 3x price swings)
- **Liquidity**: Low tolerance for liquidity risk (brief token exposure, return to FeelsSOL)
- **Motivation**: Capture short-term price movements with leverage
- **Behavior**: Use leveraged flash loans for quick in-and-out trades

#### 5. **Snipers**
- **Time**: Low tolerance for credit risk (immediate flips)
- **Leverage**: Low tolerance for principal volatility (precision matters more than amplification)
- **Liquidity**: Low tolerance for liquidity risk (must exit at targets)
- **Motivation**: Exploit predictable price movements at launches
- **Behavior**: Provide initial liquidity and price discovery

## Position Construction

Based on agent preferences, positions are constructed using the protocol's parameters:

### Position Parameters
1. **Tranche**: Maps to leverage tolerance (principal volatility)
   - **Senior**: 1x exposure, stable principal during rebases (low tolerance)
   - **Junior**: 3x exposure, amplified participation in rebases (high tolerance)
2. **Duration**: Maps to time tolerance (credit risk)
   - **Flash**: 0 duration, same-transaction return (low tolerance)
   - **Monthly**: Fixed 28-day term with return expectation (high tolerance)
   - **Spot**: Perpetual/infinite duration (infinite tolerance - see note below)
3. **Lend Asset**: Maps to liquidity tolerance (liquidity risk)
   - **FeelsSOL**: Lending the liquid asset (low tolerance)
   - **Token**: Lending the illiquid asset (high tolerance)
4. **Amount**: Capital committed to the position

**Important**: In the "everything is lending" model:
- **Flash positions** are same-transaction loans where capital must return immediately (low credit risk tolerance)
- **Monthly positions** are term loans with an expectation of capital return after 28 days (high credit risk tolerance)
- **Spot positions** are perpetual loans where the lender provides liquidity indefinitely. When borrowed, the collateral effectively becomes permanent capital for the protocol

**Note on Spot Liquidity**: The "Infinite" credit risk tolerance is a theoretical classification to complete our model. In practice, LPs providing spot liquidity reframe the transaction as an exchange rather than a loan - credit risk becomes asymptotically large and is removed from their decision calculus entirely.

### How Preferences Map to Position Choices

#### Leverage → Tranche Selection
- **Low tolerance for principal volatility** → Senior tranche (1x)
- **High tolerance for principal volatility** → Junior tranche (3x)

#### Time → Duration Selection
- **Low tolerance for credit risk** → Flash duration (capital returns same transaction)
- **High tolerance for credit risk** → Monthly duration (capital returns after 28 days)
- **Infinite tolerance for credit risk** → Spot duration (perpetual loan, may never return)

#### Liquidity → Asset Selection
- **Low tolerance for liquidity risk** → Lend FeelsSOL (remain liquid)
- **High tolerance for liquidity risk** → Lend Token (accept illiquidity)

This creates natural agent-position alignment based on their 3D preference profile.

### Agent-Specific Position Templates

#### Issuer Position
```purescript
issuerPosition = 
  { tranche: Senior          -- Low tolerance for principal volatility (1x)
  , duration: Monthly        -- High tolerance for credit risk (28-day terms)
  , lendAsset: Token         -- High tolerance for liquidity risk
  , amount: largeAmount      -- Significant capital
  , purpose: "Long-term protocol support and appreciation"
  }
```

#### Hodler Position
```purescript
hodlerPosition = 
  { tranche: Senior          -- Low tolerance for principal volatility (1x)
  , duration: Monthly        -- High tolerance for credit risk (28-day terms)
  , lendAsset: Token         -- High tolerance for liquidity risk
  , amount: mediumAmount     -- Diversified allocation
  , purpose: "Steady yield with downside protection"
  }
```

#### Volatility Harvester Position
```purescript
volHarvesterPosition = 
  { tranche: Senior          -- Low tolerance for principal volatility (1x)
  , duration: Monthly        -- High tolerance for credit risk (28-day terms)
  , lendAsset: FeelsSOL      -- Committed to the protocol for volatility monetization
  , amount: largeAmount      -- Capital committed for volatility monetization
  , purpose: "Monetizing volatility with LVR compensation and yield eligibility"
  }
```
*Note: Vol Harvesters lend FeelsSOL to maintain liquidity while earning enhanced fees.

#### Momentum Trader Position
```purescript
momentumPosition = 
  { tranche: Junior          -- High tolerance for principal volatility (3x)
  , duration: Flash          -- Low tolerance for credit risk (immediate return)
  , lendAsset: FeelsSOL      -- Low tolerance for liquidity risk
  , amount: smallAmount      -- Limited risk capital
  , purpose: "Leveraged short-term trading"
  }
```

#### Sniper Position
```purescript
sniperPosition = 
  { tranche: Junior          -- Despite low tolerance for principal volatility
  , duration: Flash          -- Low tolerance for credit risk (immediate return)
  , lendAsset: FeelsSOL      -- Low tolerance for liquidity risk
  , amount: mediumAmount     -- Sized for impact
  , purpose: "Quick flips with first-mover advantage"
  }
```
*Note: Snipers use Junior for first-mover advantage with Flash loans for immediate capital recovery.

## Complementary Dynamics

The system creates natural complementarities between agents:

### Credit Risk Dimension (Time)
```
INFINITE TOLERANCE                 HIGH TOLERANCE                   LOW TOLERANCE
└── (Spot LPs)*                    ├── Issuers                      ├── Momentum Traders
                                   ├── Hodlers                      └── Snipers
                                   └── Vol Harvesters               

Provide: Spot liquidity            Provide: Term capital            Provide: Flash liquidity
         (Exchange mindset)                28-day loans                     Trading volume
         No credit calculus                Bounded risk                     Price discovery

*Theoretical classification
```

### Principal Volatility Dimension (Leverage) 
```
LOW TOLERANCE (1x)                 HIGH TOLERANCE (3x)
├── Issuers                        ├── Momentum Traders
├── Hodlers                        └── (Sometimes Snipers)
└── Vol Harvesters*                

Provide: Stable base               Provide: Rebase absorption
         Conservative liquidity             Leveraged activity
         Principal stability                Volume generation

*Vol Harvesters achieve yield through fees, not leverage
```

### Liquidity Risk Dimension (Asset Choice)
```
LOW TOLERANCE                      HIGH TOLERANCE
├── Vol Harvesters                 ├── Issuers  
├── Momentum Traders               ├── Hodlers
└── Snipers                        └── (Token lenders)

Lend: FeelsSOL                     Lend: Tokens
      Maintain liquidity                  Accept illiquidity
      Stable exit value                   Token price exposure
```

## Market Equilibrium

The system naturally balances when:
- Sufficient stakers provide base liquidity (Issuers, Hodlers, Vol Harvesters)
- Active traders generate volume and fees (Momentum Traders, Snipers)
- Senior positions provide stability while Junior positions absorb volatility
- Each agent type finds their complementary partners in the ecosystem
- Liquidity-sensitive agents lending FeelsSOL are matched with liquidity-tolerant agents borrowing FeelsSOL

This 3D preference model ensures that every participant can find a position type that matches their specific needs while contributing to overall market health. Each dimension maps to a concrete choice in position construction: Duration (Flash/Monthly/Swap), Tranche (Senior/Junior), and Asset (FeelsSOL/Token).
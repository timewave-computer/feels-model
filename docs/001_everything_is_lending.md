# Everything is Lending

All financial primitives can be understood as a form of collateralized lending. This document presents a simplification where exchange, lending, and leverage are all expressed as variations of a single lending primitive.

## The Universal Lending Primitive

```purescript
type UniversalPosition =
  { lendAmount :: Number
  , lendAsset :: TokenType
  , collateralAmount :: Number
  , collateralAsset :: TokenType
  , duration :: Duration
  , returnType :: ReturnType
  }

data Duration 
  = Perpetual              -- Never needs repayment (swap)
  = Fixed Int              -- Must be repaid after N days
  
data ReturnType
  = FixedReturn Number     -- Standard loan: principal + interest
  = MarketReturn           -- Swap: no return expected
  = LeveragedReturn Number -- Redenomination: return tracks price multiple
```

## How Each Primitive Maps to Lending

### 1. Pure Exchange (Swap)
**Conceptual Mapping**: A swap is two perpetual loans with no expectation of repayment.

```purescript
-- "Swap 100 BaseToken for 5000 TokenA" becomes:
{ lendAmount: 100
, lendAsset: BaseToken
, collateralAmount: 5000
, collateralAsset: TokenA
, duration: Perpetual
, returnType: MarketReturn
}
```

**Key Properties**:
- No interest rate needed (the collateral IS the payment)
- Both parties are permanently satisfied
- The "exchange rate" is just the collateral ratio

### 2. Pure Lending
**Conceptual Mapping**: Traditional lending with expectation of return.

```purescript
-- "Lend 100 BaseToken for 30 days at 5% APR" becomes:
{ lendAmount: 100
, lendAsset: BaseToken
, collateralAmount: 100
, collateralAsset: ReceiptToken  -- Receipt token
, duration: Fixed 30
, returnType: FixedReturn 0.05
}
```

**Key Properties**:
- Collateral is a receipt token proving the loan
- Fixed duration with interest compensation
- Original asset expected back plus yield

### 3. Leverage (Redenomination)
**Conceptual Mapping**: Lending to a synthetic asset vault that issues leveraged tokens.

```purescript
-- "Get 2x leverage on 100 BaseToken" becomes:
{ lendAmount: 100
, lendAsset: BaseToken
, collateralAmount: 100
, collateralAsset: Token "2xBaseToken"
, duration: Perpetual  -- Or Fixed for term leverage
, returnType: LeveragedReturn 2.0
}
```

**Key Properties**:
- Collateral is synthetic tokens tracking leveraged price
- No liquidation risk (you're the lender, not borrower)
- Can "repay" by returning synthetic tokens

## A Reformulation of the Tick Model

Ticks now represent loan offers organized by collateralization terms:

```purescript
type UnifiedTick =
  { lendAsset :: TokenType
  , collateralRatio :: Number    -- Collateral per unit lent
  , tickType :: TickType
  , available :: Number
  }

data TickType
  = SwapTick                     -- Perpetual loan offers
  | LendingTick InterestRate     -- Fixed-term loan offers  
  | LeverageTick Multiple        -- Synthetic collateral offers
```

### Examples:

**Exchange Tick at price 50**:
"I'll lend 1 BaseToken if you give me 50 TokenA as perpetual collateral"

**Lending Tick at 5% for 30 days**:
"I'll lend 1 BaseToken if you give me 1 ReceiptToken receipt + promise 5% return"

**Leverage Tick at 3x**:
"I'll lend 1 BaseToken if you give me 1 3xBaseToken synthetic token as collateral"

## Mathematical Properties

### Associativity Through Lending Composition

```purescript
-- Sequential lending maintains consistent semantics
Lend(Lend(Asset, Terms1), Terms2) = Lend(Asset, ComposedTerms)

-- Example: Leveraged lending
Step1: Lend(100 BaseToken) → Receive(100 2xBaseToken)
Step2: Lend(100 2xBaseToken, 30 days) → Receive(100 2xBaseToken-Receipt)
Result: Time-locked leveraged position
```

### Risk Mapping

Each loan type maps cleanly to the original risk triangle:

| Loan Type | Primary Risk | Mitigation |
|-----------|--------------|------------|
| Perpetual Loan (Swap) | Liquidity Risk | Deep order books via ticks |
| Term Loan | Credit Risk | Over-collateralization |
| Leveraged Loan | Nominal Risk | No liquidation, bounded loss |

## Complex Position Construction

All complex positions are just combinations of loans:

### Leveraged Liquidity (3x leveraged LP position)
```purescript
positions = [
  -- First get leverage
  { lend: 100 BaseToken, 
    collateral: 100 "3xBaseToken", 
    returnType: LeveragedReturn 3.0 },
    
  -- Then provide liquidity with leveraged tokens
  { lend: 100 "3xBaseToken", 
    collateral: 15000 TokenA,  -- 3x the normal rate
    duration: Perpetual,
    returnType: MarketReturn }
]
```

### Staked Lending (Time-locked liquidity)
```purescript
positions = [
  -- Lend with fixed duration
  { lend: 100 BaseToken,
    collateral: 100 ReceiptToken,
    duration: Fixed 60,
    returnType: FixedReturn 0.08 },
    
  -- Use receipt tokens to provide liquidity
  { lend: 100 ReceiptToken,
    collateral: 5000 TokenA,
    duration: Perpetual,
    returnType: MarketReturn }
]
```

## Implementation Benefits

### 1. Single Execution Path
```purescript
executePosition :: UniversalPosition -> Effect PositionResult
executePosition pos = do
  -- All positions follow same flow:
  -- 1. Transfer lend amount from user
  -- 2. Transfer/mint collateral to user
  -- 3. Set up return conditions based on type
  -- 4. Record in universal position registry
```

### 2. Unified Risk Assessment
```purescript
assessRisk :: UniversalPosition -> RiskScore
assessRisk pos = 
  let durationRisk = assessDurationRisk pos.duration
      collateralRisk = assessCollateralRisk pos.collateralAsset
      leverageRisk = assessLeverageRisk pos.returnType
  in combineRisks [durationRisk, collateralRisk, leverageRisk]
```

### 3. Natural Fee Structure
```purescript
calculateFee :: UniversalPosition -> Number
calculateFee pos = baseFee * 
  durationMultiplier pos.duration *
  collateralMultiplier pos.collateralAsset *
  leverageMultiplier pos.returnType
```

## System Invariants

1. **Conservation of Value**: Total lent = Total collateral value (adjusted for leverage)
2. **No Naked Positions**: Every position has matching collateral
3. **Return Guarantee**: Non-perpetual loans have enforceable return conditions
4. **Leverage Bounds**: Synthetic tokens backed by actual assets

# Feels Protocol Developer Guide

## Getting Started

### Prerequisites
- Node.js (v16+)
- PureScript compiler
- Spago package manager

### Building the Project
```bash
spago build
```

### Running Tests
```bash
spago test
```

## Common Development Tasks

### 1. Adding a New Token

To add a new token to the protocol:

```purescript
-- In your command handler or action
import Actions.TokenActions (createToken)

-- Create the token
result <- createToken "TICKER" "Token Name" "creator_address" appState
case result of
  Left error -> -- Handle error
  Right metadata -> -- Token created successfully
```

### 2. Creating a Position

Positions are the core lending/borrowing unit:

```purescript
import Actions.PositionActions (createPosition)
import PositionIntent (PositionIntent(..), UnbondingPeriod(..))

-- For a spot position (immediate liquidity)
let intent = SwapIntent

-- For a staking position with 7-day commitment
let intent = StakingIntent SevenDays

-- Create the position
result <- createPosition 
  user 
  lendAsset 
  amount 
  collateralAsset 
  collateralAmount 
  intent 
  targetToken 
  appState
```

### 3. Entering the Gateway

To convert JitoSOL to FeelsSOL:

```purescript
import Actions.GatewayActions (enterGateway)

result <- enterGateway userAddress jitoSOLAmount appState
case result of
  Left error -> -- Handle error
  Right { user, feelsSOLMinted } -> -- Success
```

### 4. Querying Account Balance

```purescript
import Actions.AccountActions (getBalance)

balance <- getBalance userAddress tokenType appState
-- Returns the balance as a Number
```

### 5. Processing Commands and Queries

The state system uses a command/query pattern:

```purescript
-- Execute a command (modifies state)
import State (executeCommand)

result <- executeCommand runtime command
case result of
  Left error -> -- Handle error
  Right newState -> -- State updated

-- Execute a query (read-only)
import State (executeQuery)

result <- executeQuery runtime query
case result of
  Left error -> -- Handle error
  Right queryResult -> -- Process result
```

## Architecture Patterns

### Command Pattern
All state modifications go through commands:

```purescript
data AppCommand
  = CreateToken String String String
  | LaunchToken String Number
  | CreatePosition PositionIntent TokenAmount (Maybe String)
  -- etc.
```

### Query Pattern
All read operations use queries:

```purescript
data AppQuery
  = GetBalance String TokenType
  | GetPosition Int
  | GetTokenMetadata String
  -- etc.
```

### Action Functions
Business logic is extracted into action modules:

```purescript
-- Actions return Either ProtocolError Result
createToken :: String -> String -> String -> AppState 
            -> Effect (Either ProtocolError TokenMetadata)
```

## Block-Based Time

The protocol uses block numbers instead of timestamps:

```purescript
-- Constants
blocksPerHour = 120   -- 30-second blocks
blocksPerDay = 2880
blocksPerWeek = 20160

-- Get next daily expiry
let expiryBlock = getNextDailyExpiry currentBlock

-- Check if expired
if isExpired currentBlock position then
  -- Handle expiry
```

## Error Handling

Use the `ProtocolError` type for all errors:

```purescript
data ProtocolError
  = InvalidTokenError String
  | InsufficientBalanceError String
  | PositionNotFoundError Int
  | InvalidAmountError Number
  -- etc.
```

## Testing

### Unit Testing an Action
```purescript
testCreateToken = do
  state <- initTestState
  result <- createToken "TEST" "Test Token" "creator" state
  
  case result of
    Left error -> fail $ "Token creation failed: " <> show error
    Right metadata -> do
      metadata.ticker `shouldEqual` "TEST"
      metadata.name `shouldEqual` "Test Token"
```

### Integration Testing
```purescript
testGatewayFlow = do
  runtime <- initTestRuntime
  
  -- Enter gateway
  enterResult <- executeCommand runtime 
    (EnterGateway "user1" 100.0)
  
  -- Check balance
  balanceResult <- executeQuery runtime 
    (GetBalance "user1" FeelsSOL)
  
  -- Verify results
  case balanceResult of
    Right (BalanceResult balance) -> 
      balance `shouldBeGreaterThan` 0.0
```

## Best Practices

### 1. Always Validate Inputs
```purescript
if amount <= 0.0
  then pure $ Left $ InvalidAmountError amount
  else -- Continue processing
```

### 2. Use Type-Safe Wrappers
Instead of raw `Number` or `String`, consider domain types:
```purescript
newtype TokenAmount = TokenAmount Number
newtype UserAddress = UserAddress String
```

### 3. Handle All Error Cases
Always handle both success and failure:
```purescript
case result of
  Left error -> logError error *> pure unit
  Right value -> processValue value
```

### 4. Keep Functions Pure
Extract effects to the edges:
```purescript
-- Pure calculation
calculateFee :: Number -> Number -> Number
calculateFee amount rate = amount * rate

-- Effectful wrapper
applyFee :: Number -> Number -> AppState -> Effect Number
applyFee amount rate state = do
  let fee = calculateFee amount rate
  -- Update state with fee
  pure fee
```

## Debugging Tips

### 1. Check the Simulation
Run the simulation to see the protocol in action:
```bash
spago run --main Test.Simulation
```

### 2. Use Logging
```purescript
import Effect.Console (log)

log $ "Position created: " <> show position.id
```

### 3. Verify State Consistency
After commands, verify state invariants:
```purescript
-- Total FeelsSOL minted should equal gateway backing
totalMinted <- getTotalMinted gateway
totalBacking <- getTotalLocked gateway
when (abs (totalMinted - totalBacking) > 0.01) $
  log "Warning: Gateway backing mismatch"
```

## Common Gotchas

1. **Block vs Time**: Remember positions expire on block boundaries, not wall clock time
2. **Dual Accounts**: Users have both ChainAccount (JitoSOL) and FeelsAccount (protocol tokens)
3. **No Liquidations**: Positions never liquidate, they only adjust in value
4. **Tranche Multipliers**: Junior positions have up to 3x exposure but take first loss

## Module Import Order

When importing modules, follow this hierarchy:
1. Infrastructure (Prelude, Effect, Data.*)
2. Domain types (Token, Position, etc.)
3. Actions
4. State modules
5. UI modules (if in UI layer)

Example:
```purescript
import Prelude
import Effect (Effect)
import Data.Either (Either(..))

import Token (TokenType(..))
import Position (Position)
import Actions.TokenActions (createToken)
import State (AppState)
```
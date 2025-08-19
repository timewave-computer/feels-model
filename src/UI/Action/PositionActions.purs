-- | Position-related UI actions.
-- | This module orchestrates position creation and management for the frontend.
module UI.Action.PositionActions
  ( createPosition
  , closePosition
  , getPositionValue
  , initiateUnbonding
  , withdrawPosition
  , createVolHarvesterPosition
  -- Validation helpers
  , validatePositionParams
  , validatePositionOwnership
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Int (toNumber)
import Effect.Console (log)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.Token (TokenType(..))
import Protocol.PositionVault (Position, Duration(..), Leverage(..), isExpired, leverageMultiplier, isSpot, isVolHarvesterPosition)
import Protocol.PositionVault as P
import Data.Array ((:))
import UI.PoolRegistry (getPosition, removePosition, addPosition)
import UI.PoolRegistry as PR
import Protocol.Error (ProtocolError(..))
import UI.Account (getFeelsAccountBalance, updateFeelsAccountBalance)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if position is spot (no duration)
isSpotPosition :: Position -> Boolean
isSpotPosition = isSpot

-- | Validate position creation parameters
validatePositionParams :: String -> Number -> Effect (Either ProtocolError Unit)
validatePositionParams user amount = do
  if user == ""
    then do
      log "Position validation failed: Empty user"
      pure $ Left $ InvalidCommandError "User cannot be empty"
    else if amount <= 0.0
      then do
        log $ "Position validation failed: Invalid amount " <> show amount
        pure $ Left $ InvalidAmountError amount
      else do
        log $ "Position validation passed for user " <> user <> " with amount " <> show amount
        pure $ Right unit

-- | Validate position ownership
validatePositionOwnership :: String -> Position -> Effect (Either ProtocolError Unit)
validatePositionOwnership user position = 
  if position.owner /= user
    then do
      log $ "Position ownership validation failed: " <> user <> " does not own position " <> show position.id
      pure $ Left $ InvalidCommandError $ "User " <> user <> " does not own position " <> show position.id
    else do
      log $ "Position ownership validated for user " <> user <> " and position " <> show position.id
      pure $ Right unit

-- | Validate position can be closed
validatePositionExpiry :: Int -> Position -> Effect (Either ProtocolError Unit)
validatePositionExpiry currentBlock position =
  if not (isSpotPosition position) && not (isExpired currentBlock position)
    then do
      log $ "Position expiry validation failed: Position " <> show position.id <> " has not expired yet"
      pure $ Left $ InvalidCommandError $ "Position " <> show position.id <> " has not expired yet"
    else do
      log $ "Position expiry validated for position " <> show position.id
      pure $ Right unit

--------------------------------------------------------------------------------
-- Position Creation
--------------------------------------------------------------------------------

-- | Create a new position in the lending book
createPosition :: 
  String ->           -- user
  TokenType ->        -- lend asset
  Number ->           -- amount
  TokenType ->        -- collateral asset
  Number ->           -- collateral amount
  Duration ->         -- duration (Spot or Monthly)
  Leverage ->         -- leverage tier (Senior or Junior)
  Boolean ->          -- rollover
  Maybe String ->     -- target token for staking
  ProtocolState ->         -- current state
  Effect (Either ProtocolError { position :: Position, positionTokenMap :: Array { positionId :: Int, tokenTicker :: String } })
createPosition user lendAsset amount collateralAsset _collateralAmount duration leverage rollover targetToken state = do
  log $ "Creating position for user " <> user <> " with amount " <> show amount
  
  -- Validate parameters
  validationResult <- validatePositionParams user amount
  case validationResult of
    Left err -> pure $ Left err
    Right _ -> do
      log "Position parameters validated, proceeding with creation"
      
      -- Get next position ID
      nextId <- PR.getNextPositionId state.poolRegistry
      log $ "Assigned position ID: " <> show nextId
      
      -- Create a position in the new system
      let currentBlock = state.currentBlock
          poolId = "FeelsSOL/DEFAULT"  -- MVP: Single default pool
          
          -- Use the leverage parameter passed in
          price = 1.0        -- MVP: Default to par price
          
          -- Calculate shares based on leverage tier
          -- Senior: 1x shares, Junior: 3x shares
          shares = amount * leverageMultiplier leverage
          
          position = P.createPosition 
            nextId
            user
            amount
            price
            duration
            leverage
            lendAsset
            collateralAsset
            rollover
            shares
            currentBlock
      
      log $ "Created position object with shares: " <> show shares
      
      -- Add position to pool registry
      PR.addPosition poolId position state.poolRegistry
      log $ "Added position to pool registry: " <> poolId
      
      -- Update position-token mapping if target token specified
      let newMapping = case targetToken of
            Just ticker -> do
              log $ "Mapping position " <> show position.id <> " to token " <> ticker
              { positionId: position.id, tokenTicker: ticker } : state.positionTokenMap
            Nothing -> state.positionTokenMap
      
      log "Position creation completed successfully"
      pure $ Right { position, positionTokenMap: newMapping }

-- | Create a position optimized for volatility harvesting
-- | This creates a monthly liquidity position that:
-- | - Uses Monthly duration (28-day commitment)
-- | - Provides liquidity to the pool
-- | - Receives LVR compensation and volatility yield
-- | - Can optionally enable rollover for continuous yield
createVolHarvesterPosition :: 
  String ->           -- user
  Number ->           -- amount of FeelsSOL to commit
  Leverage ->         -- leverage tier (Senior or Junior)
  Boolean ->          -- enable auto-rollover
  ProtocolState ->    -- current state
  Effect (Either ProtocolError { position :: Position, positionTokenMap :: Array { positionId :: Int, tokenTicker :: String } })
createVolHarvesterPosition user amount leverage rollover state = do
  -- Volatility harvesting positions:
  -- - FeelsSOL as lend asset
  -- - Monthly duration for LVR compensation
  -- - Optional rollover for continuous harvesting
  -- - No specific target token
  createPosition 
    user 
    FeelsSOL        -- lend asset (FeelsSOL for liquidity)
    amount          -- amount to commit
    FeelsSOL        -- collateral asset (same as lend asset for liquidity provision)
    amount          -- collateral amount (1:1 for liquidity)
    Monthly         -- duration (Monthly for LVR compensation)
    leverage        -- leverage tier chosen by user
    rollover        -- rollover chosen by user
    Nothing         -- no target token
    state

--------------------------------------------------------------------------------
-- Position Management
--------------------------------------------------------------------------------

-- | Close a position
closePosition :: String -> Int -> ProtocolState -> Effect (Either ProtocolError Unit)
closePosition user positionId state = do
  log $ "Attempting to close position " <> show positionId <> " for user " <> user
  
  -- 1. Get the position
  maybePosition <- getPosition positionId state.poolRegistry
  case maybePosition of
    Nothing -> do
      log $ "Position not found: " <> show positionId
      pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " not found"
    Just position -> do
      log $ "Found position " <> show positionId <> " owned by " <> position.owner
      
      -- 2. Verify user owns the position
      ownershipResult <- validatePositionOwnership user position
      case ownershipResult of
        Left err -> pure $ Left err
        Right _ -> do
          -- 3. Check if position can be closed (for term positions, check expiry)
          expiryResult <- validatePositionExpiry state.currentBlock position
          case expiryResult of
            Left err -> pure $ Left err
            Right _ -> do
              log "Position validation passed, proceeding with closure"
              
              -- 4. Calculate returns (principal + any gains)
              let returnAmount = position.amount  -- Simplified: return principal
              log $ "Returning amount: " <> show returnAmount
              
              -- 5. Transfer funds back to user (positions are always in FeelsSOL)
              currentBalance <- getFeelsAccountBalance state.accounts user FeelsSOL
              updateResult <- updateFeelsAccountBalance state.accounts user FeelsSOL (currentBalance + returnAmount)
              case updateResult of
                Left err -> do
                  log $ "Failed to update balance: " <> err
                  pure $ Left $ InvalidCommandError err
                Right _ -> do
                  log "Balance updated successfully"
                  
                  -- 6. Remove position from pool
                  removePosition positionId state.poolRegistry
                  log $ "Position " <> show positionId <> " closed successfully"
                  pure $ Right unit

-- | Get the current value of a position
getPositionValue :: Int -> ProtocolState -> Effect (Either ProtocolError Number)
getPositionValue positionId state = do
  -- 1. Get position from lending book
  maybePosition <- getPosition positionId state.poolRegistry
  case maybePosition of
    Nothing -> pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " not found"
    Just position -> do
      -- 2. Calculate current value based on position type
      let baseValue = position.amount
      
      -- 3. Apply leverage multiplier (simplified)
      let leverageMult = case position.leverage of
            Senior -> 1.0    -- Senior tier: no leverage, stable returns
            Junior -> 1.2    -- Junior tier: 20% bonus for risk (simplified)
      
      -- 4. Apply time-based returns for term positions (simplified)
      currentBlock <- pure state.currentBlock
      let timeMultiplier = if isSpotPosition position
                          then 1.0
                          else 
                            let blocksHeld = currentBlock - position.createdAt
                                annualReturn = 0.05  -- 5% annual return
                                blocksPerYear = 2628000.0  -- ~5 blocks per minute * 60 * 24 * 365
                            in 1.0 + (annualReturn * (toNumber blocksHeld) / blocksPerYear)
      
      -- 5. Calculate final value
      let currentValue = baseValue * leverageMult * timeMultiplier
      pure $ Right currentValue

-- | Initiate unbonding for a position
initiateUnbonding :: String -> Int -> ProtocolState -> Effect (Either ProtocolError Unit)
initiateUnbonding user positionId state = do
  -- 1. Get the position
  maybePosition <- getPosition positionId state.poolRegistry
  case maybePosition of
    Nothing -> pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " not found"
    Just position -> do
      -- 2. Verify user owns the position
      if position.owner /= user
        then pure $ Left $ InvalidCommandError $ "User " <> user <> " does not own position " <> show positionId
        else do
          -- 3. Check if position can be unbonded
          -- For spot positions, unbonding is immediate
          -- For term positions, we must wait until expiry
          if isSpotPosition position
            then do
              -- Spot positions can be closed immediately
              closePosition user positionId state
            else do
              -- Term positions must wait until expiry
              -- Just verify it exists and is owned - actual withdrawal happens later
              pure $ Right unit

-- | Withdraw an unbonded position
withdrawPosition :: String -> Int -> ProtocolState -> Effect (Either ProtocolError Unit)
withdrawPosition user positionId state = do
  -- 1. Get the position
  maybePosition <- getPosition positionId state.poolRegistry
  case maybePosition of
    Nothing -> pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " not found"
    Just position -> do
      -- 2. Verify user owns the position
      if position.owner /= user
        then pure $ Left $ InvalidCommandError $ "User " <> user <> " does not own position " <> show positionId
        else do
          -- 3. For term positions, check if expired
          currentBlock <- pure state.currentBlock
          if not (isSpotPosition position) && not (isExpired currentBlock position)
            then pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " has not expired yet"
            else do
              -- 4. Use closePosition logic to handle the withdrawal
              closePosition user positionId state
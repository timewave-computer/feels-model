-- | Position-related UI actions.
-- | This module orchestrates position creation and management for the frontend.
module UI.Action.PositionActions
  ( createPosition
  , closePosition
  , getPositionValue
  , initiateUnbonding
  , withdrawPosition
  , createVolHarvesterPosition
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Int (toNumber)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.Token (TokenType(..))
import Protocol.Position (Position, Duration(..), Leverage(..), isExpired, leverageMultiplier, isSpot, isVolHarvesterPosition)
import Protocol.Position as P
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
  -- Validate amount
  if amount <= 0.0
    then pure $ Left $ InvalidAmountError amount
    else do
      -- Get next position ID
      nextId <- PR.getNextPositionId state.poolRegistry
      
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
      
      -- Add position to pool registry
      PR.addPosition position state.poolRegistry
      
      -- Update position-token mapping if target token specified
      let newMapping = case targetToken of
            Just ticker -> { positionId: position.id, tokenTicker: ticker } : state.positionTokenMap
            Nothing -> state.positionTokenMap
      
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
  -- 1. Get the position
  maybePosition <- getPosition positionId state.poolRegistry
  case maybePosition of
    Nothing -> pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " not found"
    Just position -> do
      -- 2. Verify user owns the position
      if position.owner /= user
        then pure $ Left $ InvalidCommandError $ "User " <> user <> " does not own position " <> show positionId
        else do
          -- 3. Check if position can be closed (for term positions, check expiry)
          currentBlock <- pure state.currentBlock
          if not (isSpotPosition position) && not (isExpired currentBlock position)
            then pure $ Left $ InvalidCommandError $ "Position " <> show positionId <> " has not expired yet"
            else do
              -- 4. Calculate returns (principal + any gains)
              let returnAmount = position.amount  -- Simplified: return principal
              
              -- 5. Transfer funds back to user (positions are always in FeelsSOL)
              currentBalance <- getFeelsAccountBalance state.accounts user FeelsSOL
              updateResult <- updateFeelsAccountBalance state.accounts user FeelsSOL (currentBalance + returnAmount)
              case updateResult of
                Left err -> pure $ Left $ InvalidCommandError err
                Right _ -> do
                  -- 6. Remove position from pool
                  removePosition positionId state.poolRegistry
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
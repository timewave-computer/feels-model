-- | Position-related UI actions.
-- | This module orchestrates position creation and management for the frontend.
module UI.Action.PositionActions
  ( createPosition
  , closePosition
  , getPositionValue
  , initiateUnbonding
  , withdrawPosition
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Int (toNumber)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.Token (TokenType(..))
import Protocol.Position (Position, TermCommitment, Tranche(..), isExpired)
import Protocol.Position as P
import Data.Array ((:))
import UI.PoolRegistry (getPosition, removePosition, addPosition)
import UI.PoolRegistry as PR
import Protocol.Error (ProtocolError(..))
import UI.Account (getFeelsAccountBalance, updateFeelsAccountBalance)

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if position is spot (no term commitment)
isSpotPosition :: Position -> Boolean
isSpotPosition pos = pos.term == P.spotTerm

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
  TermCommitment ->   -- term commitment
  Boolean ->          -- rollover
  Maybe String ->     -- target token for staking
  ProtocolState ->         -- current state
  Effect (Either ProtocolError { position :: Position, positionTokenMap :: Array { positionId :: Int, tokenTicker :: String } })
createPosition user _lendAsset amount _collateralAsset _collateralAmount term rollover targetToken state = do
  -- Validate amount
  if amount <= 0.0
    then pure $ Left $ InvalidAmountError amount
    else do
      -- Get next position ID
      nextId <- PR.getNextPositionId state.poolRegistry
      
      -- Create a position in the new system
      let currentBlock = state.currentBlock
          poolId = "FeelsSOL/DEFAULT"  -- MVP: Single default pool
          
          tranche = Senior  -- MVP: All positions use Senior tranche
          
          -- Calculate shares based on tranche multiplier
          -- Senior tranche: 1x shares per unit of capital
          -- Junior tranche: 3x shares per unit of capital (higher risk/reward)
          trancheMultiplier = case tranche of
            Senior -> 1.0
            Junior -> 3.0
          
          -- Calculate shares: amount * multiplier
          -- This represents the proportional claim on pool profits
          shares = amount * trancheMultiplier
          
          position = P.createPosition 
            nextId
            poolId
            user
            amount
            tranche
            term
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
      
      -- 3. Apply tranche multiplier (simplified)
      let trancheMultiplier = case position.tranche of
            Senior -> 1.0    -- Senior tranche: no leverage, stable returns
            Junior -> 1.2    -- Junior tranche: 20% bonus for risk (simplified)
      
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
      let currentValue = baseValue * trancheMultiplier * timeMultiplier
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
-- | Position-related actions for the Feels Protocol.
-- | This module extracts position creation and management logic from State.
module Actions.PositionActions
  ( createPosition
  , closePosition
  , getPositionValue
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- Import types
import State.Types (AppState)
import Token (TokenType)
import Position (Position, TermCommitment, Tranche(..))
import Position as P
import Data.Array ((:))
import PoolRegistry as PR
import Errors (ProtocolError(..))

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
  Maybe String ->     -- target token for staking
  AppState ->         -- current state
  Effect (Either ProtocolError { position :: Position, positionTokenMap :: Array { positionId :: Int, tokenTicker :: String } })
createPosition user _lendAsset amount _collateralAsset _collateralAmount term targetToken state = do
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
          
          -- Calculate shares (simplified for MVP)
          shares = amount  -- In real implementation, pool would calculate this
          
          position = P.createPosition 
            nextId
            poolId
            user
            amount
            tranche
            term
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

-- | Close a position (stub for now)
closePosition :: String -> Int -> AppState -> Effect (Either ProtocolError Unit)
closePosition _user positionId _state = do
  -- TODO: Implement position closing logic
  -- 1. Verify user owns the position
  -- 2. Check if position can be closed (term expired, etc.)
  -- 3. Calculate returns
  -- 4. Update lending book
  -- 5. Transfer funds
  pure $ Left $ InvalidCommandError $ "Position closing not yet implemented for position " <> show positionId

-- | Get the current value of a position (stub for now)
getPositionValue :: Int -> AppState -> Effect (Either ProtocolError Number)
getPositionValue _positionId _state = do
  -- TODO: Implement position valuation
  -- 1. Get position from lending book
  -- 2. Calculate current value based on:
  --    - Original amount
  --    - Tranche returns
  --    - Time elapsed
  --    - Market conditions
  pure $ Right 0.0
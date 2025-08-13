-- | Atomic operations that combine multiple steps into single user actions.
-- | These operations handle the JitoSOL -> FeelsSOL conversion transparently.
module UI.Action.AtomicActions
  ( createPositionFromJitoSOL
  , createTokenAndPositionFromJitoSOL
  , AtomicPositionResult
  , AtomicTokenPositionResult
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.Token (TokenType(..), TokenMetadata)
import Protocol.Position (Position, Duration, Leverage)
import Protocol.Error (ProtocolError(..))

-- Import actions
import UI.Action.FeelsSOLActions (enterFeelsSOL)
import UI.Action.PositionActions (createPosition)
import UI.Action.TokenActions (createToken)
import UI.Account (getChainAccountBalance)

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Result of atomic position creation from JitoSOL
type AtomicPositionResult =
  { position :: Position
  , feelsSOLUsed :: Number
  , jitoSOLUsed :: Number
  }

-- | Result of atomic token + position creation from JitoSOL
type AtomicTokenPositionResult =
  { token :: TokenMetadata
  , position :: Position
  , feelsSOLUsed :: Number
  , jitoSOLUsed :: Number
  }

--------------------------------------------------------------------------------
-- Atomic Operations
--------------------------------------------------------------------------------

-- | Create a position directly from JitoSOL in a single atomic operation.
-- | This transparently handles the JitoSOL -> FeelsSOL conversion.
createPositionFromJitoSOL :: 
  String ->           -- user
  Number ->           -- JitoSOL amount
  Duration ->         -- duration (Spot or Monthly)
  Leverage ->         -- leverage tier (Senior or Junior)
  Boolean ->          -- rollover
  Maybe String ->     -- target token for staking
  ProtocolState ->    -- current state
  Effect (Either ProtocolError AtomicPositionResult)
createPositionFromJitoSOL user jitoAmount duration leverage rollover targetToken state = do
  -- 1. Check JitoSOL balance
  jitoBalance <- getChainAccountBalance state.accounts user
  if jitoBalance < jitoAmount
    then pure $ Left $ InsufficientBalanceError $ 
      "Insufficient JitoSOL balance. Required: " <> show jitoAmount <> ", Available: " <> show jitoBalance
    else do
      -- 2. Convert JitoSOL to FeelsSOL (silent intermediate step)
      conversionResult <- enterFeelsSOL user jitoAmount state
      case conversionResult of
        Left err -> pure $ Left err
        Right { feelsSOLMinted } -> do
          -- 3. Create position with the FeelsSOL
          positionResult <- createPosition 
            user 
            FeelsSOL           -- Always lend FeelsSOL
            feelsSOLMinted     -- Use all converted FeelsSOL
            FeelsSOL           -- Collateral not used in new system
            0.0                -- No collateral amount
            duration
            leverage
            rollover
            targetToken
            state
          
          case positionResult of
            Left err -> pure $ Left err
            Right { position } -> 
              pure $ Right 
                { position: position
                , feelsSOLUsed: feelsSOLMinted
                , jitoSOLUsed: jitoAmount
                }

-- | Create a new token and position in that token atomically from JitoSOL.
-- | This combines token creation, liquidity provision, and position creation.
createTokenAndPositionFromJitoSOL ::
  String ->           -- user
  Number ->           -- JitoSOL amount  
  String ->           -- ticker
  String ->           -- token name
  Duration ->         -- duration for position
  Leverage ->         -- leverage tier
  Boolean ->          -- rollover
  ProtocolState ->    -- current state
  Effect (Either ProtocolError AtomicTokenPositionResult)
createTokenAndPositionFromJitoSOL user jitoAmount ticker name duration leverage rollover state = do
  -- 1. Check JitoSOL balance
  jitoBalance <- getChainAccountBalance state.accounts user
  if jitoBalance < jitoAmount
    then pure $ Left $ InsufficientBalanceError $ 
      "Insufficient JitoSOL balance. Required: " <> show jitoAmount <> ", Available: " <> show jitoBalance
    else do
      -- 2. Convert JitoSOL to FeelsSOL
      conversionResult <- enterFeelsSOL user jitoAmount state
      case conversionResult of
        Left err -> pure $ Left err
        Right { feelsSOLMinted } -> do
          -- 3. Create the new token
          tokenResult <- createToken user ticker name state
          case tokenResult of
            Left err -> pure $ Left err
            Right newToken -> do
              -- 4. Create position targeting the new token
              -- This uses the FeelsSOL to provide liquidity and earn from the new token
              positionResult <- createPosition
                user
                FeelsSOL           -- Always lend FeelsSOL
                feelsSOLMinted     -- Use all converted FeelsSOL
                FeelsSOL           -- Collateral not used
                0.0
                duration
                leverage
                rollover
                (Just ticker)      -- Target the newly created token
                state
              
              case positionResult of
                Left err -> pure $ Left err
                Right { position } ->
                  pure $ Right
                    { token: newToken
                    , position: position
                    , feelsSOLUsed: feelsSOLMinted
                    , jitoSOLUsed: jitoAmount
                    }
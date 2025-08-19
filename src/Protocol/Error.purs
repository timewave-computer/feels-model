-- | Protocol Error Types for the Feels Protocol
-- |
-- | This module defines all possible error conditions that can occur during
-- | protocol operations. Each error type provides structured information
-- | about what went wrong, enabling proper error handling and user feedback.
module Protocol.Error
  ( ProtocolError(..)
  ) where

import Prelude

--------------------------------------------------------------------------------
-- PROTOCOL ERROR TYPES
--------------------------------------------------------------------------------
-- Comprehensive error types covering all protocol operation failures

-- | All possible error conditions in the protocol
-- | Each variant provides specific context about the failure
data ProtocolError
  -- Command and validation errors
  = InvalidCommandError String          -- Invalid or malformed command
  | ValidationError String              -- Input validation failed
  | InvalidAmountError Number           -- Amount is negative, zero, or invalid
  
  -- Resource not found errors
  | TokenNotFoundError String           -- Token ticker doesn't exist
  | PositionNotFoundError Int           -- Position ID doesn't exist
  | UserNotFoundError String            -- User account doesn't exist
  
  -- Balance and transfer errors
  | InsufficientBalanceError String     -- Not enough balance for operation
  | InsufficientReserves                 -- Protocol reserves insufficient for solvency
  | TransferError String                -- Token transfer failed
  
  -- System and matching errors
  | MatchingError String                -- Position matching failed
  | SystemError String                  -- Internal system error

derive instance eqProtocolError :: Eq ProtocolError

--------------------------------------------------------------------------------
-- ERROR DISPLAY
--------------------------------------------------------------------------------
-- Human-readable error messages for UI display and logging

instance showProtocolError :: Show ProtocolError where
  show (InvalidCommandError msg) = "Invalid command: " <> msg
  show (ValidationError msg) = "Validation error: " <> msg
  show (InvalidAmountError amount) = "Invalid amount: " <> show amount
  show (TokenNotFoundError ticker) = "Token not found: " <> ticker
  show (PositionNotFoundError id) = "Position not found: " <> show id
  show (UserNotFoundError user) = "User not found: " <> user
  show (InsufficientBalanceError msg) = "Insufficient balance: " <> msg
  show InsufficientReserves = "Insufficient reserves: Protocol solvency check failed"
  show (TransferError msg) = "Transfer error: " <> msg
  show (MatchingError msg) = "Matching error: " <> msg
  show (SystemError msg) = "System error: " <> msg
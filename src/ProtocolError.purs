module ProtocolError
  ( ProtocolError(..)
  ) where

import Prelude

data ProtocolError
  = InvalidCommandError String
  | InsufficientBalanceError String
  | TokenNotFoundError String
  | PositionNotFoundError Int
  | UserNotFoundError String
  | InvalidAmountError Number
  | SystemError String
  | MatchingError String
  | TransferError String
  | ValidationError String

derive instance eqProtocolError :: Eq ProtocolError

instance showProtocolError :: Show ProtocolError where
  show (InvalidCommandError msg) = "Invalid command: " <> msg
  show (InsufficientBalanceError msg) = "Insufficient balance: " <> msg
  show (TokenNotFoundError ticker) = "Token not found: " <> ticker
  show (PositionNotFoundError id) = "Position not found: " <> show id
  show (UserNotFoundError user) = "User not found: " <> user
  show (InvalidAmountError amount) = "Invalid amount: " <> show amount
  show (SystemError msg) = "System error: " <> msg
  show (MatchingError msg) = "Matching error: " <> msg
  show (TransferError msg) = "Transfer error: " <> msg
  show (ValidationError msg) = "Validation error: " <> msg

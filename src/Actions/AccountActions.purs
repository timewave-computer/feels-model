-- | Account-related actions for the Feels Protocol.
-- | This module handles account operations and balance management.
module Actions.AccountActions
  ( getBalance
  , transfer
  , updateBalance
  ) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)

-- Import types
import State.Types (AppState)
import Token (TokenType)
import Accounts (getFeelsAccountBalance, updateFeelsAccountBalance)
import Errors (ProtocolError(..))

--------------------------------------------------------------------------------
-- Account Operations
--------------------------------------------------------------------------------

-- | Get account balance for a specific token
getBalance :: String -> TokenType -> AppState -> Effect Number
getBalance user tokenType state = do
  getFeelsAccountBalance state.accounts user tokenType

-- | Transfer tokens between accounts
transfer :: String -> String -> TokenType -> Number -> AppState -> Effect (Either ProtocolError Unit)
transfer from to token amount state = do
  if amount <= 0.0
    then pure $ Left $ InvalidAmountError amount
    else do
      -- Get sender balance
      fromBalance <- getFeelsAccountBalance state.accounts from token
      
      if fromBalance < amount
        then pure $ Left $ InsufficientBalanceError $ "Required: " <> show amount <> ", Available: " <> show fromBalance
        else do
          -- Deduct from sender
          _ <- updateFeelsAccountBalance state.accounts from token (fromBalance - amount)
          
          -- Add to receiver
          toBalance <- getFeelsAccountBalance state.accounts to token
          _ <- updateFeelsAccountBalance state.accounts to token (toBalance + amount)
          
          pure $ Right unit

-- | Update account balance directly (internal use only)
updateBalance :: String -> TokenType -> Number -> AppState -> Effect Unit
updateBalance user token newBalance state = do
  _ <- updateFeelsAccountBalance state.accounts user token newBalance
  pure unit
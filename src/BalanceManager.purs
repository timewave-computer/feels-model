-- Centralized balance management for the lending protocol.
-- Handles all balance operations including checking, updating, and transferring balances
-- between users. This module consolidates balance logic that was previously scattered
-- across Execution, Gateway, and other modules.
module BalanceManager
  ( BalanceRegistry
  , Balance
  , initBalanceRegistry
  , checkBalance
  , updateBalance
  , transferBalance
  , getBalance
  , getUserBalances
  , getTotalBalance
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (find, filter, (:))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Token (TokenType)
import Data.Foldable (sum)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Balance entry for a user and token
type Balance =
  { owner :: String
  , token :: TokenType
  , amount :: Number
  }

-- Registry of all balances
type BalanceRegistry = Ref (Array Balance)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize an empty balance registry
initBalanceRegistry :: Effect BalanceRegistry
initBalanceRegistry = new []

-- Initialize with initial balances
initBalanceRegistryWith :: Array Balance -> Effect BalanceRegistry
initBalanceRegistryWith initialBalances = new initialBalances

--------------------------------------------------------------------------------
-- Balance Operations
--------------------------------------------------------------------------------

-- Check if user has sufficient balance
checkBalance :: BalanceRegistry -> String -> TokenType -> Number -> Effect Boolean
checkBalance registry owner token requiredAmount = do
  balance <- getBalance registry owner token
  pure (balance >= requiredAmount)

-- Get balance for a user and token
getBalance :: BalanceRegistry -> String -> TokenType -> Effect Number
getBalance registry owner token = do
  balances <- read registry
  case find (\b -> b.owner == owner && b.token == token) balances of
    Just balance -> pure balance.amount
    Nothing -> pure 0.0

-- Update user balance (can be positive or negative delta)
updateBalance :: BalanceRegistry -> String -> TokenType -> Number -> Effect Unit
updateBalance registry owner token delta = do
  balances <- read registry
  let updated = updateBalanceArray balances owner token delta
  write updated registry

-- Transfer balance between users
transferBalance :: BalanceRegistry -> String -> String -> TokenType -> Number -> Effect (Either String Unit)
transferBalance registry fromUser toUser token amount = do
  -- Check sender has sufficient balance
  hasBalance <- checkBalance registry fromUser token amount
  if not hasBalance
    then pure $ Left $ "Insufficient balance: " <> fromUser <> " needs " <> show amount <> " " <> show token
    else do
      -- Deduct from sender
      updateBalance registry fromUser token (-amount)
      -- Add to receiver
      updateBalance registry toUser token amount
      pure $ Right unit

-- Get all balances for a user
getUserBalances :: BalanceRegistry -> String -> Effect (Array Balance)
getUserBalances registry owner = do
  balances <- read registry
  pure $ filter (\b -> b.owner == owner) balances

-- Get total balance of a token across all users
getTotalBalance :: BalanceRegistry -> TokenType -> Effect Number
getTotalBalance registry token = do
  balances <- read registry
  let tokenBalances = filter (\b -> b.token == token) balances
  pure $ sum (map _.amount tokenBalances)

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- Update balance array with delta
updateBalanceArray :: Array Balance -> String -> TokenType -> Number -> Array Balance
updateBalanceArray balances owner token delta =
  case find (\b -> b.owner == owner && b.token == token) balances of
    Just existing ->
      let newAmount = existing.amount + delta
      in if newAmount > 0.0
         then map (\b -> if b.owner == owner && b.token == token 
                        then b { amount = newAmount }
                        else b) balances
         else filter (\b -> not (b.owner == owner && b.token == token)) balances
    Nothing ->
      if delta > 0.0
      then { owner: owner, token: token, amount: delta } : balances
      else balances
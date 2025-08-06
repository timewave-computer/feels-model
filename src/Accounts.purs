-- Centralized account management for the Feels Protocol.
-- Handles unified Feels accounts that hold any token on behalf of users after they've
-- entered the gateway. All protocol activity occurs through these accounts.
-- Users receive tokens back to their normal accounts when they exit the gateway.
module Accounts
  ( AccountRegistry
  , Account
  , initAccountRegistry
  , checkAccountBalance
  , updateAccount
  , transferBetweenAccounts
  , getAccountBalance
  , getUserAccount
  , getTotalAccountBalance
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

-- Account entry for a user's Feels account holding a specific token
type Account =
  { owner :: String
  , token :: TokenType
  , amount :: Number
  }

-- Registry of all Feels accounts
type AccountRegistry = Ref (Array Account)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize an empty account registry
initAccountRegistry :: Effect AccountRegistry
initAccountRegistry = new []

-- Initialize with initial accounts
initAccountRegistryWith :: Array Account -> Effect AccountRegistry
initAccountRegistryWith initialAccounts = new initialAccounts

--------------------------------------------------------------------------------
-- Account Operations
--------------------------------------------------------------------------------

-- Check if user's Feels account has sufficient balance
checkAccountBalance :: AccountRegistry -> String -> TokenType -> Number -> Effect Boolean
checkAccountBalance registry owner token requiredAmount = do
  accountBalance <- getAccountBalance registry owner token
  pure (accountBalance >= requiredAmount)

-- Get account balance for a user and token
getAccountBalance :: AccountRegistry -> String -> TokenType -> Effect Number
getAccountBalance registry owner token = do
  accounts <- read registry
  case find (\acc -> acc.owner == owner && acc.token == token) accounts of
    Just account -> pure account.amount
    Nothing -> pure 0.0

-- Update user's Feels account (can be positive or negative delta)
updateAccount :: AccountRegistry -> String -> TokenType -> Number -> Effect Unit
updateAccount registry owner token delta = do
  accounts <- read registry
  let updated = updateAccountArray accounts owner token delta
  write updated registry

-- Transfer tokens between users' Feels accounts
transferBetweenAccounts :: AccountRegistry -> String -> String -> TokenType -> Number -> Effect (Either String Unit)
transferBetweenAccounts registry fromUser toUser token amount = do
  -- Check sender's Feels account has sufficient balance
  hasBalance <- checkAccountBalance registry fromUser token amount
  if not hasBalance
    then pure $ Left $ "Insufficient account balance: " <> fromUser <> " needs " <> show amount <> " " <> show token
    else do
      -- Deduct from sender's account
      updateAccount registry fromUser token (-amount)
      -- Add to receiver's account
      updateAccount registry toUser token amount
      pure $ Right unit

-- Get all token holdings in a user's Feels account
getUserAccount :: AccountRegistry -> String -> Effect (Array Account)
getUserAccount registry owner = do
  accounts <- read registry
  pure $ filter (\acc -> acc.owner == owner) accounts

-- Get total balance of a token across all Feels accounts
getTotalAccountBalance :: AccountRegistry -> TokenType -> Effect Number
getTotalAccountBalance registry token = do
  accounts <- read registry
  let tokenAccounts = filter (\acc -> acc.token == token) accounts
  pure $ sum (map _.amount tokenAccounts)

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- Update account array with delta
updateAccountArray :: Array Account -> String -> TokenType -> Number -> Array Account
updateAccountArray accounts owner token delta =
  case find (\acc -> acc.owner == owner && acc.token == token) accounts of
    Just existing ->
      let newAmount = existing.amount + delta
      in if newAmount > 0.0
         then map (\acc -> if acc.owner == owner && acc.token == token 
                          then acc { amount = newAmount }
                          else acc) accounts
         else filter (\acc -> not (acc.owner == owner && acc.token == token)) accounts
    Nothing ->
      if delta > 0.0
      then { owner: owner, token: token, amount: delta } : accounts
      else accounts
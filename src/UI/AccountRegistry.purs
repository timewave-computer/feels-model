-- | Account registry management for the UI.
-- | Manages local tracking of FeelsAccounts and ChainAccounts.
-- | In the actual Solana implementation, accounts would be PDAs.
module UI.AccountRegistry
  ( AccountRegistry
  , FeelsAccount
  , ChainAccount
  , TokenBalance
  , initAccountRegistry
  , getOrCreateFeelsAccount
  , getOrCreateChainAccount
  , getFeelsAccountBalance
  , updateFeelsAccountBalance
  , transferBetweenFeelsAccounts
  , getChainAccountBalance
  , updateChainAccountBalance
  , depositFromChain
  , withdrawToChain
  , getAllFeelsAccountBalances
  , getTotalTokenBalance
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Protocol.Token (TokenType(..))
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Balance of a specific token
type TokenBalance = 
  { token :: TokenType
  , amount :: Number
  }

-- FeelsAccount holds all protocol assets for a user
type FeelsAccount =
  { owner :: String
  , balances :: Map TokenType Number  -- All token balances
  }

-- ChainAccount holds JitoSOL outside the protocol (simulates an EOA or external program account)
type ChainAccount =
  { owner :: String
  , jitoSOLBalance :: Number
  }

-- Registry containing both account types
type AccountRegistry =
  { feelsAccounts :: Ref (Map String FeelsAccount)
  , chainAccounts :: Ref (Map String ChainAccount)
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize an empty account registry
initAccountRegistry :: Effect AccountRegistry
initAccountRegistry = do
  feelsAccounts <- new Map.empty
  chainAccounts <- new Map.empty
  pure { feelsAccounts, chainAccounts }

--------------------------------------------------------------------------------
-- FeelsAccount Operations
--------------------------------------------------------------------------------

-- Get or create a FeelsAccount for a user
getOrCreateFeelsAccount :: AccountRegistry -> String -> Effect FeelsAccount
getOrCreateFeelsAccount registry owner = do
  accounts <- read registry.feelsAccounts
  case Map.lookup owner accounts of
    Just account -> pure account
    Nothing -> do
      let newAccount = 
            { owner: owner
            , balances: Map.empty
            }
      modify_ (Map.insert owner newAccount) registry.feelsAccounts
      pure newAccount

-- Get balance of a specific token in FeelsAccount
getFeelsAccountBalance :: AccountRegistry -> String -> TokenType -> Effect Number
getFeelsAccountBalance registry owner token = do
  account <- getOrCreateFeelsAccount registry owner
  pure $ fromMaybe 0.0 (Map.lookup token account.balances)

-- Update balance of a specific token in FeelsAccount
updateFeelsAccountBalance :: AccountRegistry -> String -> TokenType -> Number -> Effect (Either String Unit)
updateFeelsAccountBalance registry owner token newBalance = do
  account <- getOrCreateFeelsAccount registry owner
  
  if newBalance < 0.0
    then pure $ Left $ "Balance cannot be negative: " <> show newBalance
    else do
      let updatedBalances = if newBalance == 0.0
                           then Map.delete token account.balances
                           else Map.insert token newBalance account.balances
          updatedAccount = account { balances = updatedBalances }
      
      modify_ (Map.insert owner updatedAccount) registry.feelsAccounts
      pure $ Right unit

-- Transfer tokens between FeelsAccounts
transferBetweenFeelsAccounts :: AccountRegistry -> String -> String -> TokenType -> Number -> Effect (Either String Unit)
transferBetweenFeelsAccounts registry fromUser toUser token amount = do
  if amount <= 0.0
    then pure $ Left "Transfer amount must be positive"
    else do
      -- Check sender balance
      fromBalance <- getFeelsAccountBalance registry fromUser token
      if fromBalance < amount
        then pure $ Left $ "Insufficient balance: " <> fromUser <> " has " <> show fromBalance <> " " <> show token
        else do
          -- Update balances
          _ <- updateFeelsAccountBalance registry fromUser token (fromBalance - amount)
          toBalance <- getFeelsAccountBalance registry toUser token
          updateFeelsAccountBalance registry toUser token (toBalance + amount)

-- Get all token balances for a user's FeelsAccount
getAllFeelsAccountBalances :: AccountRegistry -> String -> Effect (Array TokenBalance)
getAllFeelsAccountBalances registry owner = do
  account <- getOrCreateFeelsAccount registry owner
  pure $ map (\(Tuple token amount) -> { token: token, amount: amount }) (Map.toUnfoldable account.balances)

-- Get total balance of a token across all FeelsAccounts
getTotalTokenBalance :: AccountRegistry -> TokenType -> Effect Number
getTotalTokenBalance registry token = do
  accounts <- read registry.feelsAccounts
  let allBalances = Array.fromFoldable $ Map.values accounts >>= \account ->
        case Map.lookup token account.balances of
          Just amount -> pure amount
          Nothing -> mempty
  pure $ sum allBalances

--------------------------------------------------------------------------------
-- ChainAccount Operations
--------------------------------------------------------------------------------

-- Get or create a ChainAccount for a user
getOrCreateChainAccount :: AccountRegistry -> String -> Effect ChainAccount
getOrCreateChainAccount registry owner = do
  accounts <- read registry.chainAccounts
  case Map.lookup owner accounts of
    Just account -> pure account
    Nothing -> do
      let newAccount = { owner: owner, jitoSOLBalance: 0.0 }
      modify_ (Map.insert owner newAccount) registry.chainAccounts
      pure newAccount

-- Get JitoSOL balance in ChainAccount
getChainAccountBalance :: AccountRegistry -> String -> Effect Number
getChainAccountBalance registry owner = do
  account <- getOrCreateChainAccount registry owner
  pure account.jitoSOLBalance

-- Update JitoSOL balance in ChainAccount
updateChainAccountBalance :: AccountRegistry -> String -> Number -> Effect (Either String Unit)
updateChainAccountBalance registry owner delta = do
  account <- getOrCreateChainAccount registry owner
  let newBalance = account.jitoSOLBalance + delta
  
  if newBalance < 0.0
    then pure $ Left $ "Insufficient JitoSOL balance: " <> owner <> " has " <> show account.jitoSOLBalance
    else do
      let updatedAccount = account { jitoSOLBalance = newBalance }
      modify_ (Map.insert owner updatedAccount) registry.chainAccounts
      pure $ Right unit

--------------------------------------------------------------------------------
-- Gateway Operations
--------------------------------------------------------------------------------

-- Deposit JitoSOL from ChainAccount to get FeelsSOL in FeelsAccount
-- This simulates the gateway conversion
depositFromChain :: AccountRegistry -> String -> Number -> Effect (Either String Unit)
depositFromChain registry owner amount = do
  if amount <= 0.0
    then pure $ Left "Deposit amount must be positive"
    else do
      -- Deduct JitoSOL from ChainAccount
      deductResult <- updateChainAccountBalance registry owner (-amount)
      case deductResult of
        Left err -> pure $ Left err
        Right _ -> do
          -- Add FeelsSOL to FeelsAccount (1:1 conversion for simplicity)
          currentBalance <- getFeelsAccountBalance registry owner FeelsSOL
          updateFeelsAccountBalance registry owner FeelsSOL (currentBalance + amount)

-- Withdraw FeelsSOL from FeelsAccount to get JitoSOL in ChainAccount
-- This simulates the gateway exit
withdrawToChain :: AccountRegistry -> String -> Number -> Effect (Either String Unit)
withdrawToChain registry owner amount = do
  if amount <= 0.0
    then pure $ Left "Withdrawal amount must be positive"
    else do
      -- Check FeelsSOL balance
      feelsBalance <- getFeelsAccountBalance registry owner FeelsSOL
      if feelsBalance < amount
        then pure $ Left $ "Insufficient FeelsSOL balance: " <> owner <> " has " <> show feelsBalance
        else do
          -- Update balances
          _ <- updateFeelsAccountBalance registry owner FeelsSOL (feelsBalance - amount)
          updateChainAccountBalance registry owner amount
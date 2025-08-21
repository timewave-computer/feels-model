-- | Vault Abstraction - Generic ledger-based vault framework
-- |
-- | This module provides a powerful ledger-based vault abstraction that maintains
-- | a complete audit trail of all user actions. Each vault defines custom entry types
-- | for maximum flexibility while sharing common ledger operations.
-- |
-- | Key concepts:
-- | - Ledger: Complete history of all user deposits/actions
-- | - Entry Type: Vault-specific data for each ledger entry
-- | - Aggregation: Automatic balance calculation from ledger entries
-- | - Type Safety: Each vault defines its own entry type
module Protocol.Vault
  ( -- Balance Sheet types
    LedgerEntry(..)
  , ShareAmount
  , DepositResult
  , WithdrawResult
  -- State types
  , LedgerVaultState
  , LedgerVault
  , LedgerOpsDict
  -- Ledger operations class
  , class LedgerOps
  , entryValue
  , isActive
  , aggregateEntries
  , applyStrategy
  , selectForWithdrawal
  -- Query functions
  , getAccountBalance
  , getAccountEntries
  , getActiveEntries
  , getTotalBalance
  , getAllAccounts
  -- Mutation functions
  , addLedgerEntry
  , updateLedgerEntry
  , removeLedgerEntry
  -- Vault creation functions
  , createEmptyLedgerState
  , createLedgerVault
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:), filter, find, findIndex, updateAt, deleteAt, modifyAt)
import Data.Array as Array
import Data.Foldable (sum, foldl)
import Data.Tuple (Tuple(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Ref (Ref, read, write, modify_)
import Protocol.Common (BlockNumber)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- BALANCE SHEET
--------------------------------------------------------------------------------

-- | Generic ledger entry type parameterized by custom data
newtype LedgerEntry a = LedgerEntry
  { accountId :: String          -- Account identifier
  , timestamp :: Number          -- When entry was created
  , blockNumber :: BlockNumber   -- Block when entry was created
  , data :: a                    -- Vault-specific entry data
  }

derive instance eqLedgerEntry :: Eq a => Eq (LedgerEntry a)

-- | Share amount type for type safety
type ShareAmount = Number

-- | Result of deposit operation
type DepositResult a s = 
  { state :: LedgerVaultState a s
  , shares :: ShareAmount 
  , entry :: LedgerEntry a
  }

-- | Result of withdraw operation
type WithdrawResult a s = 
  { state :: LedgerVaultState a s
  , amount :: Number
  , removedEntries :: Array (LedgerEntry a)
  }

--------------------------------------------------------------------------------
-- STATE
--------------------------------------------------------------------------------

-- | Ledger-based vault state
type LedgerVaultState a s =
  { ledger :: Map String (Array (LedgerEntry a))  -- AccountId -> Entries
  , totalBalance :: Number                         -- Cached total balance
  , totalEntries :: Int                            -- Total number of entries
  , lastUpdateBlock :: BlockNumber                 -- Last state update
  , strategyState :: s                             -- Vault-specific strategy
  }

-- | Ledger-based vault type with operations
type LedgerVault a s =
  { name :: String                                         -- Vault identifier
  , state :: Ref (LedgerVaultState a s)                   -- Current vault state
  -- Core vault operations
  , deposit :: String -> a -> BlockNumber -> Effect (DepositResult a s)
  , withdraw :: String -> ShareAmount -> BlockNumber -> Effect (Maybe (WithdrawResult a s))
  , getBalance :: String -> Effect ShareAmount
  , getTotalBalance :: Effect ShareAmount
  -- Strategy operations
  , allocate :: Effect Unit
  , updateStrategy :: s -> Effect Unit
  -- Ledger operations instance
  , ledgerOps :: LedgerOpsDict a s
  }

-- | Dictionary for ledger operations (to avoid orphan instances)
type LedgerOpsDict a s =
  { entryValue :: LedgerEntry a -> ShareAmount
  , isActive :: BlockNumber -> LedgerEntry a -> Boolean
  , aggregateEntries :: Array (LedgerEntry a) -> ShareAmount
  , applyStrategy :: s -> Array (LedgerEntry a) -> Array (LedgerEntry a)
  , selectForWithdrawal :: ShareAmount -> Array (LedgerEntry a) -> Maybe (Array { entry :: LedgerEntry a, index :: Int })
  }

-- | Type class for ledger entry operations
class LedgerOps a where
  -- | Extract balance contribution from entry
  entryValue :: LedgerEntry a -> ShareAmount
  
  -- | Check if entry is still valid/active at given block
  isActive :: BlockNumber -> LedgerEntry a -> Boolean
  
  -- | Aggregate multiple entries into total balance
  aggregateEntries :: Array (LedgerEntry a) -> ShareAmount
  
  -- | Apply strategy transformations to entries
  applyStrategy :: forall s. s -> Array (LedgerEntry a) -> Array (LedgerEntry a)
  
  -- | Select entries for withdrawal based on vault-specific strategy
  -- | Returns entries and their indices in priority order
  selectForWithdrawal :: ShareAmount -> Array (LedgerEntry a) -> Maybe (Array { entry :: LedgerEntry a, index :: Int })

--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------

-- Query Functions

-- | Get account balance from ledger
getAccountBalance :: forall a s. LedgerOps a => LedgerVaultState a s -> String -> BlockNumber -> ShareAmount
getAccountBalance state accountId currentBlock =
  case Map.lookup accountId state.ledger of
    Nothing -> 0.0
    Just entries ->
      let activeEntries = filter (isActive currentBlock) entries
      in aggregateEntries activeEntries

-- | Get all entries for an account
getAccountEntries :: forall a s. LedgerVaultState a s -> String -> Array (LedgerEntry a)
getAccountEntries state accountId =
  fromMaybe [] (Map.lookup accountId state.ledger)

-- | Get all active entries across all accounts
getActiveEntries :: forall a s. LedgerOps a => LedgerVaultState a s -> BlockNumber -> Array (LedgerEntry a)
getActiveEntries state currentBlock =
  let allEntries = Array.concat $ Array.fromFoldable $ Map.values state.ledger
  in filter (isActive currentBlock) allEntries

-- | Get total balance across all accounts
getTotalBalance :: forall a s. LedgerOps a => LedgerVaultState a s -> BlockNumber -> ShareAmount
getTotalBalance state currentBlock =
  let activeEntries = getActiveEntries state currentBlock
  in aggregateEntries activeEntries

-- | Get list of all accounts
getAllAccounts :: forall a s. LedgerVaultState a s -> Array String
getAllAccounts state = Array.fromFoldable $ Map.keys state.ledger

-- Mutation Functions

-- | Add entry to ledger
addLedgerEntry :: forall a s. LedgerOps a => 
  LedgerVaultState a s -> 
  String -> 
  a -> 
  BlockNumber -> 
  Number -> 
  Tuple (LedgerVaultState a s) (LedgerEntry a)
addLedgerEntry state accountId entryData blockNumber timestamp =
  let
    -- Create new entry
    newEntry = LedgerEntry
      { accountId: accountId
      , timestamp: timestamp
      , blockNumber: blockNumber
      , data: entryData
      }
    
    -- Get existing entries for account
    existingEntries = fromMaybe [] (Map.lookup accountId state.ledger)
    updatedEntries = newEntry : existingEntries
    
    -- Update ledger
    updatedLedger = Map.insert accountId updatedEntries state.ledger
    
    -- Recalculate total balance
    entryVal = entryValue newEntry
    newTotalBalance = state.totalBalance + entryVal
    
    -- Update state
    updatedState = state
      { ledger = updatedLedger
      , totalBalance = newTotalBalance
      , totalEntries = state.totalEntries + 1
      , lastUpdateBlock = blockNumber
      }
    
  in Tuple updatedState newEntry

-- | Update an existing ledger entry
updateLedgerEntry :: forall a s. LedgerOps a =>
  LedgerVaultState a s ->
  String ->
  Int ->  -- Entry index
  (LedgerEntry a -> LedgerEntry a) ->
  Maybe (LedgerVaultState a s)
updateLedgerEntry state accountId index updateFn =
  case Map.lookup accountId state.ledger of
    Nothing -> Nothing
    Just entries ->
      case Array.modifyAt index updateFn entries of
        Nothing -> Nothing
        Just updatedEntries ->
          let
            -- Recalculate total balance
            oldTotal = aggregateEntries entries
            newTotal = aggregateEntries updatedEntries
            balanceDiff = newTotal - oldTotal
            
            updatedLedger = Map.insert accountId updatedEntries state.ledger
            updatedState = state
              { ledger = updatedLedger
              , totalBalance = state.totalBalance + balanceDiff
              }
          in Just updatedState

-- | Remove entry from ledger
removeLedgerEntry :: forall a s. LedgerOps a =>
  LedgerVaultState a s ->
  String ->
  Int ->  -- Entry index
  Maybe (Tuple (LedgerVaultState a s) (LedgerEntry a))
removeLedgerEntry state accountId index =
  case Map.lookup accountId state.ledger of
    Nothing -> Nothing
    Just entries ->
      case Array.index entries index of
        Nothing -> Nothing
        Just removedEntry ->
          case deleteAt index entries of
            Nothing -> Nothing
            Just remainingEntries ->
              let
                -- Update or remove account entry
                updatedLedger = if Array.null remainingEntries
                                then Map.delete accountId state.ledger
                                else Map.insert accountId remainingEntries state.ledger
                
                -- Update total balance
                removedValue = entryValue removedEntry
                updatedState = state
                  { ledger = updatedLedger
                  , totalBalance = state.totalBalance - removedValue
                  , totalEntries = state.totalEntries - 1
                  }
              in Just (Tuple updatedState removedEntry)

-- Vault Creation Functions

-- | Create empty ledger state
createEmptyLedgerState :: forall a s. s -> BlockNumber -> LedgerVaultState a s
createEmptyLedgerState strategyState currentBlock =
  { ledger: Map.empty
  , totalBalance: 0.0
  , totalEntries: 0
  , lastUpdateBlock: currentBlock
  , strategyState: strategyState
  }

-- | Create a ledger-based vault
createLedgerVault :: forall a s. LedgerOps a =>
  String ->
  LedgerVaultState a s ->
  Ref (LedgerVaultState a s) ->
  LedgerVault a s
createLedgerVault name initialState stateRef =
  { name: name
  , state: stateRef
  
  -- Deposit implementation
  , deposit: \accountId entryData currentBlock -> do
      timestamp <- currentTime
      modify_ (\s -> 
        let (Tuple newState entry) = addLedgerEntry s accountId entryData currentBlock timestamp
        in newState
      ) stateRef
      
      currentState <- read stateRef
      let entry = LedgerEntry { accountId, timestamp, blockNumber: currentBlock, data: entryData }
          shares = entryValue entry
      pure { state: currentState, shares: shares, entry: entry }
  
  -- Withdraw implementation
  , withdraw: \accountId requestedAmount currentBlock -> do
      currentState <- read stateRef
      let balance = getAccountBalance currentState accountId currentBlock
      
      if balance < requestedAmount
        then pure Nothing
        else do
          -- Get entries and use vault-specific selection strategy
          let entries = getAccountEntries currentState accountId
              activeEntries = filter (isActive currentBlock) entries
              
              -- Use the selectForWithdrawal strategy from LedgerOps
              selectedEntries = selectForWithdrawal requestedAmount activeEntries
          
          case selectedEntries of
            Nothing -> pure Nothing
            Just entriesToRemove -> do
              -- Remove entries in reverse order to maintain indices
              let sortedEntries = Array.reverse $ Array.sortBy (\a b -> compare a.index b.index) entriesToRemove
              
              removedEntries <- foldl (\acc entryInfo -> do
                accVal <- acc
                currentState' <- read stateRef
                case removeLedgerEntry currentState' accountId entryInfo.index of
                  Nothing -> pure accVal
                  Just (Tuple newState removed) -> do
                    _ <- write newState stateRef
                    pure (removed : accVal)
              ) (pure []) sortedEntries
              
              finalState <- read stateRef
              pure $ Just { state: finalState, amount: requestedAmount, removedEntries: removedEntries }
  
  -- Get balance implementation
  , getBalance: \accountId -> do
      currentState <- read stateRef
      let currentBlock = currentState.lastUpdateBlock
      pure $ getAccountBalance currentState accountId currentBlock
  
  -- Get total balance implementation
  , getTotalBalance: do
      currentState <- read stateRef
      pure currentState.totalBalance
  
  -- Allocate implementation (applies strategy to entries)
  , allocate: do
      modify_ (\s ->
        let allEntries = Array.concat $ Array.fromFoldable $ Map.values s.ledger
            transformedEntries = applyStrategy s.strategyState allEntries
            -- Rebuild ledger from transformed entries
            newLedger = foldl (\acc (LedgerEntry e) ->
              let existing = fromMaybe [] (Map.lookup e.accountId acc)
              in Map.insert e.accountId (LedgerEntry e : existing) acc
            ) Map.empty transformedEntries
        in s { ledger = newLedger }
      ) stateRef
  
  -- Update strategy implementation
  , updateStrategy: \newStrategy -> do
      modify_ (\s -> s { strategyState = newStrategy }) stateRef
  
  -- Ledger operations dictionary
  , ledgerOps:
      { entryValue: entryValue
      , isActive: isActive
      , aggregateEntries: aggregateEntries
      , applyStrategy: applyStrategy
      , selectForWithdrawal: selectForWithdrawal
      }
  }
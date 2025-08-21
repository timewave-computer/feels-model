-- | Launch Vault - Token launch system using ledger-based vault abstraction
-- |
-- | This module implements token launches as a ledger-based vault where:
-- | - Ledger entries track user token purchases with phase and lock information
-- | - Each entry represents a token purchase with maturity based on phase
-- | - Strategy state manages launch phases and pricing
-- |
-- | Key Features:
-- | - Multi-phase launches with ascending price discovery
-- | - Term commitment requirements enforced through entry maturity
-- | - Complete audit trail of all token purchases
-- | - Automatic balance aggregation and lock enforcement
module Protocol.LaunchVault
  ( -- Balance Sheet types
    LaunchEntry
  , LaunchPhase(..)
  , LaunchStatus
  , DepositForLaunchParams
  , WithdrawFromLaunchParams
  -- State types
  , LaunchStrategy
  , PhaseConfig
  , LaunchConfig
  -- Vault creation functions
  , createLaunchVault
  , initializeLaunchVault
  -- Phase management functions
  , getCurrentPhase
  , transitionPhase
  , checkPhaseComplete
  -- Token operation functions
  , depositForLaunchTokens
  , withdrawFromLaunch
  -- Query functions
  , getUserPurchases
  , getTotalRaised
  , getLaunchStatus
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (filter, find, mapWithIndex, sortBy, (:), uncons)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Ord (compare)
import Data.Number (abs)
import Protocol.Vault 
  ( LedgerEntry(..), LedgerVaultState, LedgerVault, ShareAmount
  , class LedgerOps, createEmptyLedgerState, createLedgerVault
  , getAccountEntries, getAllAccounts, entryValue, aggregateEntries
  )
import Protocol.Common (BlockNumber)
import Protocol.Pool (Duration(..))
import Protocol.Error (ProtocolError(..))
import Protocol.Config (defaultProtocolConfig)

--------------------------------------------------------------------------------
-- BALANCE SHEET
--------------------------------------------------------------------------------

-- | Launch-specific ledger entry
data LaunchEntry = LaunchEntry
  { shares :: ShareAmount        -- Number of tokens purchased
  , phase :: LaunchPhase         -- Phase when purchased
  , pricePerShare :: Number      -- Price paid per share
  , duration :: Duration         -- Commitment duration
  , maturityBlock :: Maybe BlockNumber  -- When tokens unlock
  }

-- | Launch phases with increasing prices
data LaunchPhase
  = MonthlyPhase   -- Phase 1: Lowest prices, requires monthly commitment
  | SwapPhase      -- Phase 2: Higher prices, no commitment required
  | Completed      -- Final state: Launch concluded

derive instance eqLaunchPhase :: Eq LaunchPhase
derive instance ordLaunchPhase :: Ord LaunchPhase

instance showLaunchPhase :: Show LaunchPhase where
  show MonthlyPhase = "Monthly"
  show SwapPhase = "Swap"
  show Completed = "Completed"

-- | Launch status for UI queries
type LaunchStatus =
  { phase :: LaunchPhase
  , tokensRemaining :: Number
  , currentPrice :: Number
  , totalRaised :: Number
  , totalParticipants :: Int
  , isActive :: Boolean
  }

--------------------------------------------------------------------------------
-- STATE
--------------------------------------------------------------------------------

-- | Launch strategy state
type LaunchStrategy =
  { config :: LaunchConfig
  , currentPhase :: LaunchPhase
  , phaseStartBlock :: BlockNumber
  , tokensSold :: Number          -- Tokens sold so far
  , totalRaised :: Number         -- Total FeelsSOL raised
  }

-- | Configuration for launch phases
type PhaseConfig =
  { phase :: LaunchPhase
  , tokenAmount :: Number         -- Tokens allocated to this phase
  , pricePerShare :: Number       -- Price per token in this phase
  , lockDuration :: BlockNumber   -- Lock duration in blocks
  }

-- | Complete launch configuration
type LaunchConfig =
  { tokenTicker :: String         -- Token symbol
  , totalTokens :: Number         -- Total tokens for launch
  , phases :: Array PhaseConfig   -- Phase configurations
  , treasuryAddress :: String     -- Treasury receiving proceeds
  }

-- | Ledger operations instance for launch entries
instance ledgerOpsLaunch :: LedgerOps LaunchEntry where
  -- Extract share value from entry
  entryValue (LedgerEntry e) = case e.data of
    LaunchEntry rec -> rec.shares
  
  -- Check if entry is active (unlocked)
  isActive currentBlock (LedgerEntry e) = case e.data of
    LaunchEntry rec -> case rec.maturityBlock of
      Nothing -> true
      Just maturity -> currentBlock >= maturity
  
  -- Sum all share values
  aggregateEntries entries = sum $ map (\(LedgerEntry e) -> case e.data of
    LaunchEntry rec -> rec.shares
  ) entries
  
  -- No strategy transformations needed for launch entries
  applyStrategy _ entries = entries
  
  -- Select entries for withdrawal - FIFO (first in, first out)
  selectForWithdrawal requestedAmount entries =
    let
      -- Sort by purchase order (using block number from LedgerEntry) with indices
      indexedEntries = Array.mapWithIndex (\idx entry -> { entry: entry, index: idx }) entries
      sortedByPurchaseTime = Array.sortBy (\a b -> 
        case a.entry, b.entry of
          LedgerEntry ae, LedgerEntry be -> compare ae.blockNumber be.blockNumber
      ) indexedEntries
      
      -- Select entries in FIFO order
      selectFromEntries :: Array { entry :: LedgerEntry LaunchEntry, index :: Int } -> 
                          ShareAmount -> 
                          Array { entry :: LedgerEntry LaunchEntry, index :: Int } -> 
                          Maybe (Array { entry :: LedgerEntry LaunchEntry, index :: Int })
      selectFromEntries [] remaining selected =
        if remaining > 0.0 then Nothing else Just selected
      selectFromEntries entries remaining selected =
        case uncons entries of
          Nothing -> if remaining > 0.0 then Nothing else Just selected
          Just { head: e, tail: es } ->
            let value = entryValue e.entry
                newRemaining = remaining - value
            in if newRemaining <= 0.0
               then Just (e : selected)
               else selectFromEntries es newRemaining (e : selected)
    
    in selectFromEntries sortedByPurchaseTime requestedAmount []

--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------

-- Vault Creation Functions

-- | Initialize a new launch vault
initializeLaunchVault :: LaunchConfig -> BlockNumber -> Effect (Either ProtocolError (Ref (LedgerVault LaunchEntry LaunchStrategy)))
initializeLaunchVault config currentBlock = do
  -- Validate configuration
  let phaseTotal = sum $ map _.tokenAmount config.phases
  
  if abs (phaseTotal - config.totalTokens) > defaultProtocolConfig.launch.phaseAllocationTolerance
    then pure $ Left $ InvalidCommandError "Phase allocations don't match total tokens"
    else do
      let initialStrategy =
            { config: config
            , currentPhase: MonthlyPhase
            , phaseStartBlock: currentBlock
            , tokensSold: 0.0
            , totalRaised: 0.0
            }
          
          initialState = createEmptyLedgerState initialStrategy currentBlock
      
      vault <- createLaunchVault ("Launch-" <> config.tokenTicker) initialState
      pure $ Right vault

-- | Create launch vault with ledger operations
createLaunchVault :: String -> LedgerVaultState LaunchEntry LaunchStrategy -> Effect (Ref (LedgerVault LaunchEntry LaunchStrategy))
createLaunchVault name initialState = do
  stateRef <- new initialState
  let vault = createLedgerVault name initialState stateRef
  new vault

-- Phase Management Functions

-- | Get current launch phase
getCurrentPhase :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> Effect LaunchPhase
getCurrentPhase vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  pure state.strategyState.currentPhase

-- | Check if current phase is complete
checkPhaseComplete :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> Effect Boolean
checkPhaseComplete vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      phaseConfig = findPhaseConfig strategy.config strategy.currentPhase
  
  case phaseConfig of
    Nothing -> pure false
    Just config -> pure $ strategy.tokensSold >= config.tokenAmount

-- | Transition to next phase
transitionPhase :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> BlockNumber -> Effect (Either ProtocolError LaunchPhase)
transitionPhase vaultRef currentBlock = do
  vault <- read vaultRef
  
  -- Check if phase is complete
  isComplete <- checkPhaseComplete vaultRef
  if not isComplete
    then pure $ Left $ InvalidCommandError "Current phase not complete"
    else do
      state <- read vault.state
      let strategy = state.strategyState
          nextPhase = case strategy.currentPhase of
            MonthlyPhase -> SwapPhase
            SwapPhase -> Completed
            Completed -> Completed
          
          newStrategy = strategy 
            { currentPhase = nextPhase
            , phaseStartBlock = currentBlock
            }
      
      vault.updateStrategy newStrategy
      pure $ Right nextPhase

-- Token Operation Functions

-- | Parameters for depositing into launch vault
type DepositForLaunchParams =
  { vaultRef :: Ref (LedgerVault LaunchEntry LaunchStrategy)
  , user :: String           -- User address
  , feelsAmount :: Number    -- FeelsSOL amount
  , duration :: Duration     -- Commitment duration
  , currentBlock :: BlockNumber  -- Current block
  }

-- | Parameters for withdrawing from launch vault
type WithdrawFromLaunchParams =
  { vaultRef :: Ref (LedgerVault LaunchEntry LaunchStrategy)
  , user :: String           -- User address
  , shares :: ShareAmount    -- Shares to claim
  , currentBlock :: BlockNumber  -- Current block
  }

-- | Deposit FeelsSOL to purchase launch tokens
depositForLaunchTokens :: DepositForLaunchParams -> Effect (Either ProtocolError ShareAmount)
depositForLaunchTokens params = do
  vault <- read params.vaultRef
  state <- read vault.state
  let strategy = state.strategyState
  
  -- Check if launch is active
  if strategy.currentPhase == Completed
    then pure $ Left $ InvalidCommandError "Launch has completed"
    else do
      -- Find phase configuration
      case findPhaseConfig strategy.config strategy.currentPhase of
        Nothing -> pure $ Left $ InvalidCommandError "Invalid phase configuration"
        Just phaseConfig -> do
          -- Validate duration eligibility
          if not (canParticipate strategy.currentPhase params.duration)
            then pure $ Left $ InvalidCommandError $ 
              "Duration " <> show params.duration <> " not eligible for " <> show strategy.currentPhase
            else do
              -- Calculate shares to allocate
              let sharesToAllocate = params.feelsAmount / phaseConfig.pricePerShare
                  tokensRemaining = phaseConfig.tokenAmount - strategy.tokensSold
              
              if sharesToAllocate > tokensRemaining
                then pure $ Left $ InvalidCommandError "Insufficient tokens remaining in phase"
                else do
                  -- Create ledger entry
                  let maturityBlock = if phaseConfig.lockDuration > 0
                                     then Just (params.currentBlock + phaseConfig.lockDuration)
                                     else Nothing
                      
                      entry = LaunchEntry
                        { shares: sharesToAllocate
                        , phase: strategy.currentPhase
                        , pricePerShare: phaseConfig.pricePerShare
                        , duration: params.duration
                        , maturityBlock: maturityBlock
                        }
                  
                  -- Record purchase
                  result <- vault.deposit params.user entry params.currentBlock
                  
                  -- Update strategy
                  let newStrategy = strategy
                        { tokensSold = strategy.tokensSold + sharesToAllocate
                        , totalRaised = strategy.totalRaised + params.feelsAmount
                        }
                  vault.updateStrategy newStrategy
                  
                  pure $ Right result.shares

-- | Withdraw launch tokens after maturity  
withdrawFromLaunch :: WithdrawFromLaunchParams -> Effect (Either ProtocolError Number)
withdrawFromLaunch params = do
  vault <- read params.vaultRef
  
  -- Check user has unlocked shares
  userEntries <- getUserPurchases params.vaultRef params.user
  let unlockedEntries = filter (\(LedgerEntry e) -> 
        case e.data of
          LaunchEntry rec -> case rec.maturityBlock of
            Nothing -> true
            Just maturity -> params.currentBlock >= maturity
      ) userEntries
      unlockedShares = sum $ map (\(LedgerEntry e) -> case e.data of
        LaunchEntry rec -> rec.shares
      ) unlockedEntries
  
  if unlockedShares < params.shares
    then pure $ Left $ InvalidCommandError "Insufficient unlocked shares"
    else do
      -- Process withdrawal
      result <- vault.withdraw params.user params.shares params.currentBlock
      case result of
        Nothing -> pure $ Left $ InvalidCommandError "Withdrawal failed"
        Just withdrawResult -> pure $ Right withdrawResult.amount

-- Query Functions

-- | Get all purchases for a user
getUserPurchases :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> String -> Effect (Array (LedgerEntry LaunchEntry))
getUserPurchases vaultRef user = do
  vault <- read vaultRef
  state <- read vault.state
  pure $ getAccountEntries state user

-- | Get total amount raised
getTotalRaised :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> Effect Number
getTotalRaised vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  pure state.strategyState.totalRaised

-- | Get launch status
getLaunchStatus :: Ref (LedgerVault LaunchEntry LaunchStrategy) -> Effect LaunchStatus
getLaunchStatus vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      phaseConfig = findPhaseConfig strategy.config strategy.currentPhase
      currentPrice = case phaseConfig of
        Nothing -> 0.0
        Just config -> config.pricePerShare
      tokensRemaining = case phaseConfig of
        Nothing -> 0.0
        Just config -> config.tokenAmount - strategy.tokensSold
      participants = getAllAccounts state
  
  pure
    { phase: strategy.currentPhase
    , tokensRemaining: tokensRemaining
    , currentPrice: currentPrice
    , totalRaised: strategy.totalRaised
    , totalParticipants: Array.length participants
    , isActive: strategy.currentPhase /= Completed && tokensRemaining > 0.0
    }

-- Helper Functions

-- | Find phase configuration
findPhaseConfig :: LaunchConfig -> LaunchPhase -> Maybe PhaseConfig
findPhaseConfig config phase = 
  Array.find (\p -> p.phase == phase) config.phases

-- | Check participation eligibility
canParticipate :: LaunchPhase -> Duration -> Boolean
canParticipate phase duration = case phase, duration of
  MonthlyPhase, Monthly -> true
  SwapPhase, _ -> true
  _, _ -> false
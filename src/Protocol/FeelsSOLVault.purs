-- | FeelsSOL Vault - JitoSOL → FeelsSOL synthetic token system using ledger-based vault abstraction
-- |
-- | This module implements the FeelsSOL system as a ledger-based vault where:
-- | - Ledger entries track JitoSOL deposits with oracle pricing
-- | - Each entry represents a JitoSOL deposit at a specific oracle price
-- | - Strategy state manages oracle integration and buffer management
-- |
-- | Key Features:
-- | - Oracle-based pricing with TWAP integration
-- | - Withdrawal buffer management for instant liquidity
-- | - Complete audit trail of all deposits and withdrawals
-- | - Entry/exit fee tracking in ledger entries
module Protocol.FeelsSOLVault
  ( -- Balance Sheet types
    FeelsSOLEntry
  , OraclePrice
  , BufferStatus
  -- State types
  , FeelsSOLStrategy
  -- Vault creation functions
  , createFeelsSOLVault
  , initializeFeelsSOLVault
  -- Oracle management functions
  , updateOraclePrice
  , getOraclePrice
  -- Deposit/withdrawal functions  
  , depositForFeelsSOL
  , withdrawFromFeelsSOL
  -- Buffer management functions
  , getBufferStatus
  , rebalanceBuffer
  -- Query functions
  , getUserBalance
  , getTotalJitoSOL
  , getEffectiveExchangeRate
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (filter, partition, mapWithIndex, (:), uncons)
import Data.Array as Array
import Data.Foldable (sum)
import Data.Number (abs)
import Protocol.Vault 
  ( LedgerEntry(..), LedgerVaultState, LedgerVault, ShareAmount
  , class LedgerOps, createEmptyLedgerState, createLedgerVault
  , getAccountEntries, getAllAccounts, entryValue, aggregateEntries
  )
import Protocol.Common (BlockNumber)
import Protocol.Token (TokenType(..))
import Protocol.Error (ProtocolError(..))
import Protocol.Config (defaultProtocolConfig)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- BALANCE SHEET
--------------------------------------------------------------------------------

-- | FeelsSOL-specific ledger entry
data FeelsSOLEntry = FeelsSOLEntry
  { jitoSOLAmount :: ShareAmount    -- JitoSOL deposited
  , entryPrice :: Number            -- Oracle price at deposit
  , entryFee :: Number              -- Fee paid on deposit
  , exitFee :: Number               -- Fee to be paid on withdrawal
  , isBuffer :: Boolean             -- Whether this entry is buffer allocation
  }

-- | Oracle price information
type OraclePrice =
  { price :: Number      -- JitoSOL/SOL exchange rate
  , timestamp :: Number  -- When price was fetched
  }

-- | Buffer status information
type BufferStatus =
  { bufferAmount :: Number
  , totalBacking :: Number
  , bufferRatio :: Number
  , isHealthy :: Boolean
  }

--------------------------------------------------------------------------------
-- STATE
--------------------------------------------------------------------------------

-- | FeelsSOL strategy state
type FeelsSOLStrategy =
  { priceOracle :: Effect Number           -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Number             -- Last oracle update timestamp
  , cachedPrice :: Maybe OraclePrice       -- Cached oracle price
  , entryFee :: Number                     -- Fee for JitoSOL → FeelsSOL conversion
  , exitFee :: Number                      -- Fee for FeelsSOL → JitoSOL conversion
  , polAllocationRate :: Number            -- Portion of fees allocated to POL system
  , bufferTargetRatio :: Number            -- Target withdrawal buffer ratio
  , totalJitoSOL :: Number                 -- Total JitoSOL in vault
  , bufferJitoSOL :: Number                -- JitoSOL reserved for buffer
  , totalFeesCollected :: Number           -- Total fees collected
  }

-- | Ledger operations instance for FeelsSOL entries
instance ledgerOpsFeelsSOL :: LedgerOps FeelsSOLEntry where
  -- Extract JitoSOL value from entry
  entryValue (LedgerEntry e) = case e.data of
    FeelsSOLEntry rec -> rec.jitoSOLAmount
  
  -- All entries are always active (no time locks)
  isActive _ _ = true
  
  -- Sum all JitoSOL amounts
  aggregateEntries entries = sum $ map (\(LedgerEntry e) -> case e.data of
    FeelsSOLEntry rec -> rec.jitoSOLAmount
  ) entries
  
  -- Apply buffer rebalancing strategy
  applyStrategy strategy entries = 
    -- For now, no transformation needed
    -- Future: could mark entries as buffer allocations
    entries
  
  -- Select entries for withdrawal - prioritize buffer entries first
  selectForWithdrawal requestedAmount entries =
    let
      -- Separate buffer and non-buffer entries with indices
      indexedEntries = Array.mapWithIndex (\idx entry -> { entry: entry, index: idx }) entries
      partitionResult = Array.partition (\e -> case e.entry of
        LedgerEntry le -> case le.data of
          FeelsSOLEntry rec -> rec.isBuffer
      ) indexedEntries
      bufferEntries = partitionResult.yes
      regularEntries = partitionResult.no
      
      -- Try to fulfill from buffer first
      selectFromEntries :: Array { entry :: LedgerEntry FeelsSOLEntry, index :: Int } -> 
                          ShareAmount -> 
                          Array { entry :: LedgerEntry FeelsSOLEntry, index :: Int } -> 
                          Maybe (Array { entry :: LedgerEntry FeelsSOLEntry, index :: Int })
      selectFromEntries [] remaining selected =
        if remaining > 0.0 then Nothing else Just selected
      selectFromEntries entries remaining selected =
        case Array.uncons entries of
          Nothing -> if remaining > 0.0 then Nothing else Just selected
          Just { head: e, tail: es } ->
            let value = entryValue e.entry
                newRemaining = remaining - value
            in if newRemaining <= 0.0
               then Just (e : selected)
               else selectFromEntries es newRemaining (e : selected)
      
      -- First try buffer entries, then regular entries
      bufferResult = selectFromEntries bufferEntries requestedAmount []
    in case bufferResult of
      Just result -> Just result
      Nothing -> 
        -- Need to use both buffer and regular entries
        let bufferTotal = aggregateEntries (map _.entry bufferEntries)
            remainingNeeded = requestedAmount - bufferTotal
        in case selectFromEntries regularEntries remainingNeeded bufferEntries of
          Nothing -> Nothing
          Just combined -> Just combined

--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------

-- Vault Creation Functions

-- | Initialize a new FeelsSOL vault
initializeFeelsSOLVault :: Effect Number -> BlockNumber -> Effect (Either ProtocolError (Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy)))
initializeFeelsSOLVault priceOracle currentBlock = do
  currentTime' <- currentTime
  initialPrice <- priceOracle
  
  let initialStrategy =
        { priceOracle: priceOracle
        , lastOracleUpdate: currentTime'
        , cachedPrice: Just { price: initialPrice, timestamp: currentTime' }
        , entryFee: defaultProtocolConfig.feelsSOL.entryFee
        , exitFee: defaultProtocolConfig.feelsSOL.exitFee
        , polAllocationRate: defaultProtocolConfig.feelsSOL.polAllocationRate
        , bufferTargetRatio: defaultProtocolConfig.buffer.targetRatio
        , totalJitoSOL: 0.0
        , bufferJitoSOL: 0.0
        , totalFeesCollected: 0.0
        }
      
      initialState = createEmptyLedgerState initialStrategy currentBlock
  
  vault <- createFeelsSOLVault "FeelsSOL" initialState
  pure $ Right vault

-- | Create FeelsSOL vault with ledger operations
createFeelsSOLVault :: String -> LedgerVaultState FeelsSOLEntry FeelsSOLStrategy -> Effect (Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy))
createFeelsSOLVault name initialState = do
  stateRef <- new initialState
  let vault = createLedgerVault name initialState stateRef
  new vault

-- Oracle Management Functions

-- | Get current oracle price with caching
getOraclePrice :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> Effect OraclePrice
getOraclePrice vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
  currentTime' <- currentTime
  
  -- Use cached price if fresh enough
  case strategy.cachedPrice of
    Just cached | (currentTime' - strategy.lastOracleUpdate) < defaultProtocolConfig.oracle.fiveMinuteWindow -> 
      pure cached
    _ -> do
      -- Fetch fresh price
      newPrice <- strategy.priceOracle
      let oraclePrice = { price: newPrice, timestamp: currentTime' }
          newStrategy = strategy 
            { cachedPrice = Just oraclePrice
            , lastOracleUpdate = currentTime'
            }
      vault.updateStrategy newStrategy
      pure oraclePrice

-- | Update oracle price manually
updateOraclePrice :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> Number -> Effect Unit
updateOraclePrice vaultRef newPrice = do
  vault <- read vaultRef
  currentTime' <- currentTime
  let oraclePrice = { price: newPrice, timestamp: currentTime' }
  
  state <- read vault.state
  let newStrategy = state.strategyState
        { cachedPrice = Just oraclePrice
        , lastOracleUpdate = currentTime'
        }
  vault.updateStrategy newStrategy

-- Deposit/Withdrawal Functions

-- | Deposit JitoSOL and receive FeelsSOL shares
depositForFeelsSOL ::
  Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) ->
  String ->           -- User address
  Number ->           -- JitoSOL amount
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError ShareAmount)
depositForFeelsSOL vaultRef user jitoSOLAmount currentBlock = do
  vault <- read vaultRef
  
  -- Get current oracle price
  oraclePrice <- getOraclePrice vaultRef
  
  state <- read vault.state
  let strategy = state.strategyState
      
      -- Calculate fees
      entryFeeAmount = jitoSOLAmount * strategy.entryFee
      netJitoSOL = jitoSOLAmount - entryFeeAmount
      
      -- Calculate FeelsSOL to mint based on oracle price
      -- 1 FeelsSOL = oracle price JitoSOL
      feelsSOLAmount = netJitoSOL / oraclePrice.price
      
      -- Create ledger entry
      entry = FeelsSOLEntry
        { jitoSOLAmount: netJitoSOL
        , entryPrice: oraclePrice.price
        , entryFee: entryFeeAmount
        , exitFee: strategy.exitFee
        , isBuffer: false
        }
  
  -- Record deposit
  result <- vault.deposit user entry currentBlock
  
  -- Update strategy totals
  let newStrategy = strategy
        { totalJitoSOL = strategy.totalJitoSOL + netJitoSOL
        , totalFeesCollected = strategy.totalFeesCollected + entryFeeAmount
        }
  vault.updateStrategy newStrategy
  
  -- Rebalance buffer if needed
  _ <- rebalanceBuffer vaultRef currentBlock
  
  pure $ Right feelsSOLAmount

-- | Withdraw FeelsSOL shares and receive JitoSOL
withdrawFromFeelsSOL ::
  Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) ->
  String ->           -- User address
  ShareAmount ->      -- FeelsSOL amount to burn
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError Number)
withdrawFromFeelsSOL vaultRef user feelsSOLAmount currentBlock = do
  vault <- read vaultRef
  
  -- Get current oracle price
  oraclePrice <- getOraclePrice vaultRef
  
  state <- read vault.state
  let strategy = state.strategyState
      
      -- Calculate JitoSOL to return based on oracle price
      jitoSOLAmount = feelsSOLAmount * oraclePrice.price
  
  -- Check if user has enough balance
  userBalance <- getUserBalance vaultRef user
  if userBalance < feelsSOLAmount
    then pure $ Left $ InvalidCommandError "Insufficient FeelsSOL balance"
    else do
      -- Process withdrawal
      result <- vault.withdraw user jitoSOLAmount currentBlock
      case result of
        Nothing -> pure $ Left $ InvalidCommandError "Withdrawal failed"
        Just withdrawResult -> do
          -- Calculate exit fee
          let exitFeeAmount = withdrawResult.amount * strategy.exitFee
              netJitoSOL = withdrawResult.amount - exitFeeAmount
              
              -- Update strategy totals
              newStrategy = strategy
                { totalJitoSOL = strategy.totalJitoSOL - withdrawResult.amount
                , totalFeesCollected = strategy.totalFeesCollected + exitFeeAmount
                }
          vault.updateStrategy newStrategy
          
          pure $ Right netJitoSOL

-- Buffer Management Functions

-- | Get buffer status
getBufferStatus :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> Effect BufferStatus
getBufferStatus vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      bufferRatio = if strategy.totalJitoSOL > 0.0
                   then strategy.bufferJitoSOL / strategy.totalJitoSOL
                   else 0.0
  
  pure
    { bufferAmount: strategy.bufferJitoSOL
    , totalBacking: strategy.totalJitoSOL
    , bufferRatio: bufferRatio
    , isHealthy: bufferRatio >= defaultProtocolConfig.buffer.minRatio
    }

-- | Rebalance buffer to target ratio
rebalanceBuffer :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> BlockNumber -> Effect Unit
rebalanceBuffer vaultRef currentBlock = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      targetBuffer = strategy.totalJitoSOL * strategy.bufferTargetRatio
      currentBuffer = strategy.bufferJitoSOL
      difference = targetBuffer - currentBuffer
  
  -- Only rebalance if difference exceeds threshold
  if abs difference > (strategy.totalJitoSOL * defaultProtocolConfig.buffer.rebalanceThreshold)
    then do
      let newStrategy = strategy { bufferJitoSOL = targetBuffer }
      vault.updateStrategy newStrategy
    else pure unit

-- Query Functions

-- | Get user's FeelsSOL balance
getUserBalance :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> String -> Effect ShareAmount
getUserBalance vaultRef user = do
  vault <- read vaultRef
  vault.getBalance user

-- | Get total JitoSOL in vault
getTotalJitoSOL :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> Effect Number
getTotalJitoSOL vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  pure state.strategyState.totalJitoSOL

-- | Get effective exchange rate (FeelsSOL/JitoSOL)
getEffectiveExchangeRate :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) -> Effect Number
getEffectiveExchangeRate vaultRef = do
  oraclePrice <- getOraclePrice vaultRef
  pure oraclePrice.price
-- | Protocol state management for the Feels Protocol.
-- | This module contains all protocol state types and initialization.
module UI.ProtocolState
  ( -- State types
    ProtocolState
  , AppRuntime
  -- Command and Query types
  , ProtocolCommand(..)
  , IndexerQuery(..)
  -- State initialization
  , initState
  -- State management
  , addListener
  , removeListener
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Array ((:), uncons)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Data.Traversable (traverse)

-- Import result types
import Protocol.Common (CommandResult, QueryResult)

-- Import domain types needed for state
import Protocol.Token (TokenType(..), TokenMetadata)
import UI.TokenRegistry (TokenRegistry, initTokenRegistry)
import Protocol.Pool (Duration, Leverage)
import Protocol.PositionVault (VaultPosition)
import UI.PoolRegistry (PoolRegistry, initPoolRegistry, addPool)
import Protocol.FeelsSOLVault (FeelsSOLStrategy, FeelsSOLEntry, initializeFeelsSOLVault)
import Protocol.Vault (LedgerVault)
import Protocol.ProtocolVault (initializeProtocolVault, ProtocolStrategy, ProtocolEntry, getProtocolMetrics)
import Protocol.Oracle (Oracle, initOracle)
import UI.Account (AccountRegistry, initAccountRegistry, updateChainAccountBalance)
import Protocol.LaunchVault (LaunchEntry, LaunchStrategy)
import Effect.Ref (Ref)
import Protocol.Error (ProtocolError)
import FFI (currentTime)
-- Commands module will be imported by consumers to avoid circular dependency

--------------------------------------------------------------------------------
-- Command and Query Types
--------------------------------------------------------------------------------

-- | Commands that modify protocol state (on-chain operations)
data ProtocolCommand
  = CreateToken String String String  
    -- ^ creator, ticker, name
  | CreatePosition String TokenType Number TokenType Number Duration Leverage Boolean (Maybe String)  
    -- ^ user, lendAsset, amount, collateralAsset, collateralAmount, term, leverage, rollover, targetToken
  | TransferTokens String String TokenType Number  
    -- ^ from, to, token, amount
  | EnterFeelsSOL String Number  
    -- ^ user, jitoSOLAmount
  | ExitFeelsSOL String Number   
    -- ^ user, feelsSOLAmount
  | InitiateUnbonding String Int  
    -- ^ user, positionId
  | WithdrawPosition String Int   
    -- ^ user, positionId
  | CreateLaunch String Number (Array { phase :: String, tokens :: Number, priceLower :: Number, priceUpper :: Number })
    -- ^ ticker, totalTokens, phases
  | StartLaunchPhase String
    -- ^ poolId
  | CompleteLaunchPhase String
    -- ^ poolId

derive instance eqProtocolCommand :: Eq ProtocolCommand

-- | Queries that read protocol state (off-chain operations)
data IndexerQuery
  = GetUserTokens String
  | GetAllTokens
  | GetUserPositions String
  | GetUserBalance String TokenType
  | GetTokenByTicker String
  | GetLenderOffers
  | GetSystemStats
  | GetPOLMetrics
  | GetPositionTargetToken Int
  | GetActiveLaunches
  | GetLaunchStatus String

derive instance eqIndexerQuery :: Eq IndexerQuery

--------------------------------------------------------------------------------
-- State Types
--------------------------------------------------------------------------------

-- | Complete protocol state containing all protocol subsystems
type ProtocolState =
  { tokenRegistry :: TokenRegistry
  , poolRegistry :: PoolRegistry
  , feelsSOL :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy)
  , polState :: Ref (LedgerVault ProtocolEntry ProtocolStrategy)
  , oracle :: Oracle
  , accounts :: AccountRegistry
  , launches :: Map String (Ref (LedgerVault LaunchEntry LaunchStrategy))     -- Pool ID -> Launch vault
  , positionTokenMap :: Array { positionId :: Int, tokenTicker :: String }
  , currentUser :: String  -- For demo purposes, in production would be wallet-based
  , currentBlock :: Int     -- Current block number
  , timestamp :: Number     -- Timestamp for UI display only
  , lastJitoSOLPrice :: Number  -- Track JitoSOL price for rebase capture
  , priceHistory :: Array { timestamp :: Number, block :: Int, price :: Number, polValue :: Number }  -- Historical price data
  }

-- | Runtime wrapper for protocol state with event system
type AppRuntime =
  { state :: Ref ProtocolState
  , listeners :: Ref (Array { id :: Int, callback :: ProtocolState -> Effect Unit })
  , nextListenerId :: Ref Int
  }


--------------------------------------------------------------------------------
-- State Initialization
--------------------------------------------------------------------------------

-- | Initialize application state
initState :: Effect AppRuntime
initState = initStateImpl

initStateImpl :: Effect AppRuntime
initStateImpl = do
  -- Initialize each subsystem separately with type annotations
  tokenRegistry :: TokenRegistry <- initTokenRegistry
  poolRegistry :: PoolRegistry <- initPoolRegistry
  accounts :: AccountRegistry <- initAccountRegistry
  
  -- Initialize oracle and POL
  let oracleInit = initOracle 1.05
  oracle :: Oracle <- oracleInit
  -- Initialize protocol vault instead of separate POL state
  polStateResult <- initializeProtocolVault 1000
  polState <- case polStateResult of
    Right vault -> pure vault
    Left err -> do
      log $ "Failed to initialize POL: " <> show err
      -- Return a placeholder - this shouldn't happen in practice
      initializeProtocolVault 1000 >>= case _ of
        Right v -> pure v
        Left _ -> pure (unsafeCoerce unit) -- This is a fallback that should never be reached
  
  -- Debug: Check POL initialization
  metrics <- getProtocolMetrics polState
  log $ "POL initialized with totalPOL: " <> show metrics.totalPOL
  
  -- Initialize FeelsSOL vault with oracle
  feelsSOLResult <- initializeFeelsSOLVault (pure 1.05) 1000
  feelsSOL :: Ref (LedgerVault FeelsSOLEntry FeelsSOLStrategy) <- case feelsSOLResult of
    Right vault -> pure vault
    Left err -> do
      log $ "Failed to initialize FeelsSOL: " <> show err
      -- Return a placeholder - this shouldn't happen in practice
      initializeFeelsSOLVault (pure 1.05) 1000 >>= case _ of
        Right v -> pure v
        Left _ -> pure (unsafeCoerce unit) -- This is a fallback that should never be reached
  
  -- Get current time
  timestamp <- currentTime
  
  -- Create initial state
  let initialState =
        { tokenRegistry: tokenRegistry
        , poolRegistry: poolRegistry
        , feelsSOL: feelsSOL
        , polState: polState
        , oracle: oracle
        , accounts: accounts
        , launches: Map.empty
        , positionTokenMap: []
        , currentUser: "demo-user"
        , currentBlock: 1000
        , timestamp: timestamp
        , lastJitoSOLPrice: 1.05
        , priceHistory: []
        }
  
  -- Skip initial pool creation for now due to type mismatch
  -- TODO: Fix pool initialization to match the new Pool type structure
  pure unit
  
  -- Set up initial balances for default users
  _ <- updateChainAccountBalance accounts "main-user" 1000.0
  _ <- updateChainAccountBalance accounts "demo-user" 1000.0
  
  -- Create runtime
  stateRef <- new initialState
  listenersRef <- new []
  listenerIdRef <- new 0
  
  pure { state: stateRef
       , listeners: listenersRef
       , nextListenerId: listenerIdRef
       }

--------------------------------------------------------------------------------
-- State Management Helpers
--------------------------------------------------------------------------------

-- | Add state change listener
addListener :: (ProtocolState -> Effect Unit) -> AppRuntime -> Effect Int
addListener callback runtime = do
  id <- read runtime.nextListenerId
  modify_ (\listeners -> { id, callback } : listeners) runtime.listeners
  modify_ (_ + 1) runtime.nextListenerId
  pure id

-- | Remove state change listener
removeListener :: Int -> AppRuntime -> Effect Unit
removeListener id runtime = do
  modify_ (filter (\l -> l.id /= id)) runtime.listeners
  where
    filter :: forall a. (a -> Boolean) -> Array a -> Array a
    filter _ [] = []
    filter pred arr = case uncons arr of
      Nothing -> []
      Just { head: x, tail: xs } -> 
        if pred x then x : filter pred xs else filter pred xs
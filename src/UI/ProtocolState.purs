-- | Protocol state management for the Feels Protocol.
-- | This module contains all protocol state types and initialization.
module UI.ProtocolState
  ( -- State types
    ProtocolState
  , AppRuntime
  -- Command and Query types
  , ProtocolCommand(..)
  , IndexerQuery(..)
  , CommandResult(..)
  , QueryResult(..)
  -- State initialization
  , initState
  -- State management
  , addListener
  , removeListener
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Array ((:))
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Data.Traversable (traverse)

-- Import result types
import Protocol.Common (CommandResult(..), QueryResult(..))

-- Import domain types needed for state
import Protocol.Token (TokenType(..), TokenMetadata)
import UI.TokenRegistry (TokenRegistry, initTokenRegistry)
import Protocol.Position (Position, TermCommitment)
import UI.PoolRegistry (PoolRegistry, initPoolRegistry, addPool)
import Protocol.FeelsSOL (FeelsSOLState, initFeelsSOL)
import Protocol.POL (POLState, initPOL, contribute)
import Protocol.Oracle (Oracle, initOracle)
import Protocol.Incentives (MarketDynamics, initMarketDynamics)
import UI.AccountRegistry (AccountRegistry, initAccountRegistry)
import Protocol.Offering (OfferingState)
import Protocol.Tick (createTick)
import Protocol.Errors (ProtocolError)
import FFI (currentTime)
-- Commands module will be imported by consumers to avoid circular dependency

--------------------------------------------------------------------------------
-- Command and Query Types
--------------------------------------------------------------------------------

-- | Commands that modify protocol state (on-chain operations)
data ProtocolCommand
  = CreateToken String String String  
    -- ^ creator, ticker, name
  | CreatePosition String TokenType Number TokenType Number TermCommitment (Maybe String)  
    -- ^ user, lendAsset, amount, collateralAsset, collateralAmount, term, targetToken
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
  | CreateOffering String Number Array { phase :: String, tokens :: Number, priceLower :: Number, priceUpper :: Number }
    -- ^ ticker, totalTokens, phases
  | StartOfferingPhase String
    -- ^ poolId
  | CompleteOfferingPhase String
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
  | GetActiveOfferings
  | GetOfferingStatus String

derive instance eqIndexerQuery :: Eq IndexerQuery

--------------------------------------------------------------------------------
-- State Types
--------------------------------------------------------------------------------

-- | Complete protocol state containing all protocol subsystems
type ProtocolState =
  { tokenRegistry :: TokenRegistry
  , poolRegistry :: PoolRegistry
  , feelsSOL :: FeelsSOLState
  , polState :: POLState
  , oracle :: Oracle
  , marketDynamics :: MarketDynamics
  , accounts :: AccountRegistry
  , offerings :: Map String OfferingState  -- Pool ID -> Offering state
  , positionTokenMap :: Array { positionId :: Int, tokenTicker :: String }
  , currentUser :: String  -- For demo purposes, in production would be wallet-based
  , currentBlock :: Int     -- Current block number
  , timestamp :: Number     -- Timestamp for UI display only
  , lastJitoSOLPrice :: Number  -- Track JitoSOL price for rebase capture
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
initState = do
  -- Initialize all subsystems
  tokenRegistry <- initTokenRegistry
  poolRegistry <- initPoolRegistry
  
  -- Initialize account registry
  accounts <- initAccountRegistry
  
  -- Initialize market dynamics
  marketDynamics <- initMarketDynamics
  
  -- Initialize oracle with starting price for JitoSOL/SOL
  oracle <- initOracle 1.05  -- JitoSOL typically trades at slight premium
  
  -- Initialize POL state
  polState <- initPOL
  
  -- Create a simple oracle function for FeelsSOL
  let priceOracle = pure 1.05  -- Fixed price for demo
  
  -- Initialize FeelsSOL with oracle
  feelsSOL <- initFeelsSOL priceOracle 0.001 0.002
  
  -- Initialize empty offerings map
  let offerings = Map.empty
  
  -- Get current time
  timestamp <- currentTime
  
  -- Create initial state
  let initialState =
        { tokenRegistry
        , poolRegistry
        , feelsSOL
        , polState
        , oracle
        , marketDynamics
        , accounts
        , offerings
        , positionTokenMap: []
        , currentUser: "demo-user"
        , currentBlock: 1000
        , timestamp
        , lastJitoSOLPrice: 1.05
        }
  
  -- Add initial pool for demo
  _ <- addPool "FeelsSOL/BONK"
    { pair: { base: FeelsSOL, quote: Token "BONK" }
    , tickBook: 
      { ticks: [createTick 1.0 10000.0]
      , tickSpacing: 0.01
      , activeTicks: 1
      }
    , aggregate:
      { spot: 1.0
      , volume24h: 0.0
      , feesUSD24h: 0.0
      , liquidity: 10000.0
      , sqrtPrice: 1.0
      , tick: 0
      }
    , positions: Map.empty
    }
    poolRegistry
  
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
    filter pred (x:xs) = if pred x then x : filter pred xs else filter pred xs
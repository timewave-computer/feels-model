-- | Protocol state management for the Feels Protocol.
-- | This module contains all protocol state types and initialization.
module UI.ProtocolState
  ( -- State types
    ProtocolState
  , AppRuntime
  -- Command and Query types
  , ProtocolCommand(..)
  , IndexerQuery(..)
  -- , CommandResult(..) -- TODO: Fix import
  -- , QueryResult(..) -- TODO: Fix import
  -- State initialization
  , initState
  -- State management
  , addListener
  , removeListener
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Array ((:), uncons)
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
import Protocol.Position (Position, Duration, Leverage)
import UI.PoolRegistry (PoolRegistry, initPoolRegistry, addPool)
import Protocol.FeelsSOL (FeelsSOLState, initFeelsSOL)
import Protocol.POL (POLState, initPOL, contribute)
import Protocol.Oracle (Oracle, initOracle)
import Protocol.Incentive (MarketDynamics, initMarketDynamics)
import UI.Account (AccountRegistry, initAccountRegistry)
import Protocol.Offering (OfferingState)
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
  | CreateOffering String Number (Array { phase :: String, tokens :: Number, priceLower :: Number, priceUpper :: Number })
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
  
  -- Initialize oracle and POL first since marketDynamics needs them
  let oracleInit = initOracle 1.05
  oracle :: Oracle <- oracleInit
  let polInit = initPOL
  polState :: POLState <- polInit
  
  -- Initialize marketDynamics with oracle and POL
  marketDynamics :: MarketDynamics <- initMarketDynamics oracle polState
  
  feelsSOL :: FeelsSOLState <- initFeelsSOL (pure 1.05) 0.001 0.002
  
  -- Get current time
  timestamp <- currentTime
  
  -- Create initial state
  let initialState =
        { tokenRegistry: tokenRegistry
        , poolRegistry: poolRegistry
        , feelsSOL: feelsSOL
        , polState: polState
        , oracle: oracle
        , marketDynamics: marketDynamics
        , accounts: accounts
        , offerings: Map.empty
        , positionTokenMap: []
        , currentUser: "demo-user"
        , currentBlock: 1000
        , timestamp: timestamp
        , lastJitoSOLPrice: 1.05
        , priceHistory: []
        }
  
  -- Add initial pool for demo
  _ <- addPool "FeelsSOL/BONK"
    { token0: FeelsSOL
    , token1: Token "BONK"
    , sqrtPriceX96: 79228162514264337593543950336.0
    , liquidity: 10000.0
    , tick: 0
    , feeGrowthGlobal0X128: 0.0
    , feeGrowthGlobal1X128: 0.0
    , protocolFee: 30.0
    , unlocked: true
    , offering: Nothing
    , leverageState: 
      { totalValue: 10000.0
      , leverageGroups:
        [ { leverage: 1.0   -- Senior
          , value: 0.0
          , shares: 0.0
          }
        , { leverage: 3.0   -- Junior
          , value: 0.0
          , shares: 0.0
          }
        ]
      }
    , totalValue: 10000.0
    , lastUpdateBlock: 0
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
    filter pred arr = case uncons arr of
      Nothing -> []
      Just { head: x, tail: xs } -> 
        if pred x then x : filter pred xs else filter pred xs
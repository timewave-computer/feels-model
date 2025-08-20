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
import Protocol.FeelsSOLVault (FeelsSOLState, createFeelsSOLVault)
import Protocol.ProtocolVault (contributeToProtocol, getTotalProtocolPOL, createProtocolVault, ProtocolVault)
import Protocol.Oracle (Oracle, initOracle)
import UI.Account (AccountRegistry, initAccountRegistry, updateChainAccountBalance)
import Protocol.LaunchVault (LaunchVault)
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
  , feelsSOL :: FeelsSOLState
  , polState :: Ref ProtocolVault
  , oracle :: Oracle
  , accounts :: AccountRegistry
  , launches :: Map String (Ref LaunchVault)     -- Pool ID -> Launch vault
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
  polState <- createProtocolVault "POL-System"
  
  -- Debug: Check POL initialization
  totalPOL <- getTotalProtocolPOL polState
  log $ "POL initialized with totalPOL: " <> show totalPOL
  
  -- Initialize FeelsSOL vault with oracle and fee configuration
  let feelsSOLStrategy = 
        { priceOracle: pure 1.05
        , lastOracleUpdate: 0.0
        , cachedPrice: Nothing
        , entryFee: 0.001
        , exitFee: 0.002
        , polAllocationRate: 0.25
        , bufferTargetRatio: 0.01
        , jitoSOLBuffer: 0.0
        }
  feelsSOL :: FeelsSOLState <- createFeelsSOLVault "FeelsSOL-System" feelsSOLStrategy
  
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
    , launch: Nothing
    , leverageState: 
      { totalValue: 10000.0
      , seniorValue: 0.0
      , seniorShares: 0.0
      , juniorValue: 0.0
      , juniorShares: 0.0
      }
    , totalValue: 10000.0
    , lastUpdateBlock: 0
    , positionTickRanges: Map.empty
    , volatility: 0.0
    , priceHistory: []
    , utilization: 0.0
    , liquidityDepth: 0.5
    , feeParameters: 
      { feelsSOLMintFee: 10.0
      , feelsSOLBurnFee: 10.0
      , swapFeeRate: 30.0
      , flashLoanFee: 5.0
      , swapPositionFee: 30.0
      , monthlyPositionFee: 20.0
      , seniorLeverageFee: 1.0
      , juniorLeverageFee: 1.5
      }
    , metricsState:
      { volatility: 0.0
      , utilization: 0.0
      , liquidityDepth: 0.5
      , volume24h: 0.0
      , tvl: 10000.0
      , lastUpdate: 0
      }
    , yieldParameters:
      { baseYieldRate: 5.0
      , spotYieldMultiplier: 1.0
      , monthlyYieldMultiplier: 1.5
      , seniorYieldShare: 0.3
      , juniorYieldShare: 0.7
      , volHarvesterBonus: 2.0
      }
    , lastYieldBlock: 0
    }
    poolRegistry
  
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
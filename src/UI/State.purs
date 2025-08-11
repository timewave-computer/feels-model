-- UI State Management for the Feels Protocol application
-- Manages all UI state types, initial state, and state coordination
module UI.State
  ( UIState
  , LaunchInfo
  , Action(..)
  , initialUIState
  , defaultSimulationConfig
  , parseTokenType
  ) where

import Data.Maybe (Maybe(..))

import State.State (AppRuntime)
import Token (TokenType(..), TokenMetadata)
import Position (Position)
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..))

--------------------------------------------------------------------------------
-- UI State Types
--------------------------------------------------------------------------------

type UIState =
  { api :: Maybe AppRuntime
  , currentUser :: String
  -- Position Creation
  , inputAmount :: Number
  , selectedAsset :: TokenType
  , collateralAsset :: TokenType
  , selectedTermType :: String  -- "spot", "hourly", "daily", "weekly"
  -- Gateway
  , showGateway :: Boolean
  , jitoSOLAmount :: Number
  , feelsSOLAmount :: Number
  -- Wallet
  , jitoSOLBalance :: Number
  , feelsSOLBalance :: Number
  -- Simulation
  , simulationConfig :: SimulationConfig
  , simulationResults :: Maybe SimulationResults
  , simulationRunning :: Boolean
  -- Chart data
  , priceHistory :: Array 
      { timestamp :: Number
      , block :: Int  -- Block number
      , price :: Number  -- JitoSOL/FeelsSOL price
      , polValue :: Number  -- POL floor for FeelsSOL
      , tokens :: Array 
          { ticker :: String
          , price :: Number  -- Token/FeelsSOL price
          , polFloor :: Number  -- POL floor for this token
          , live :: Boolean  -- Whether token is live
          }
      }
  -- Cached data from protocol
  , userTokens :: Array TokenMetadata
  , userPositions :: Array Position
  , lenderOffers :: Array Position
  , protocolStats :: Maybe 
      { totalValueLocked :: Number
      , totalUsers :: Int
      , activePositions :: Int
      , liveTokens :: Int
      , totalLenderOffers :: Int
      , polBalance :: Number
      , feelsSOLSupply :: Number
      , jitoSOLLocked :: Number
      }
  , loading :: Boolean
  , error :: Maybe String
  -- Token Creation
  , tokenTicker :: String
  , tokenName :: String
  , tokenValidationErrors :: Array String
  -- Launch System
  , selectedLaunchId :: Maybe String
  , launchBidAmount :: Number
  , launchPriorityFeePercent :: Number
  , activeLaunches :: Array LaunchInfo
  }

-- Launch info type
type LaunchInfo =
  { launchId :: String
  , tokenTicker :: String
  , currentPhase :: String
  , currentPrice :: Number
  , totalDistributed :: Number
  }

-- Component Actions
data Action 
  = Initialize
  | RefreshData
  | RenderChart
  -- Position Management
  | UpdateInputAmount Number
  | SelectAsset TokenType
  | SelectCollateralAsset TokenType
  | SetTermType String
  | CreatePosition
  -- Token Creation
  | CreateTokenUI
  | UpdateTokenTicker String
  | UpdateTokenName String
  -- Gateway
  | ToggleGateway
  | UpdateJitoSOLAmount Number
  | UpdateFeelsSOLAmount Number
  | EnterGateway
  | ExitGateway
  -- Launch System
  | SelectLaunch String
  | UpdateLaunchBidAmount Number
  | UpdateLaunchPriorityFee Number
  | SubmitLaunchBid
  | RefreshLaunches
  | ProcessLaunchBatch String
  -- Simulation
  | UpdateSimulationConfig (SimulationConfig -> SimulationConfig)
  | RunSimulation

--------------------------------------------------------------------------------
-- State Initialization
--------------------------------------------------------------------------------

initialUIState :: UIState
initialUIState =
  { api: Nothing
  , currentUser: "main-user"
  -- Position Creation
  , inputAmount: 100.0
  , selectedAsset: FeelsSOL
  , collateralAsset: JitoSOL
  , selectedTermType: "spot"
  , showGateway: true
  , jitoSOLAmount: 100.0
  , feelsSOLAmount: 100.0
  , jitoSOLBalance: 5000.0
  , feelsSOLBalance: 5000.0
  , simulationConfig: defaultSimulationConfig
  , simulationResults: Nothing
  , simulationRunning: false
  , priceHistory: []
  , userTokens: []
  , userPositions: []
  , lenderOffers: []
  , protocolStats: Nothing
  , loading: true
  , error: Nothing
  , tokenTicker: ""
  , tokenName: ""
  , tokenValidationErrors: []
  -- Launch System
  , selectedLaunchId: Nothing
  , launchBidAmount: 100.0
  , launchPriorityFeePercent: 10.0
  , activeLaunches: []
  }

-- Default simulation config
defaultSimulationConfig :: SimulationConfig
defaultSimulationConfig =
  { scenario: BullMarket
  , numAccounts: 5      -- Reduced from 10
  , simulationBlocks: 100
  , initialJitoSOLPrice: 1.22  -- Correct JitoSOL/SOL price
  , priceVolatility: 0.02
  , accountProfiles: [Whale, Aggressive, Conservative]
  , actionFrequency: 1.0  -- Reduced from 2.0
  , juniorTranchePreference: 0.3
  }

--------------------------------------------------------------------------------
-- State Utilities
--------------------------------------------------------------------------------

-- Parse token type from string value
parseTokenType :: String -> TokenType
parseTokenType "JitoSOL" = JitoSOL
parseTokenType "FeelsSOL" = FeelsSOL
parseTokenType ticker = Token ticker
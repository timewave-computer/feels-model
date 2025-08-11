-- UI Component State for the Feels Protocol application
-- Manages only UI-specific state (form inputs, display preferences, etc.)
module UI.State
  ( UIState
  , LaunchInfo
  , Action(..)
  , initialUIState
  , defaultSimulationConfig
  , parseTokenType
  ) where

import Data.Maybe (Maybe(..))

import Protocol.Token (TokenType(..))
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..))

--------------------------------------------------------------------------------
-- UI State Types
--------------------------------------------------------------------------------

-- UI-only state (form inputs, display preferences, etc.)
type UIState =
  { -- Current user context
    currentUser :: String
  -- Position Creation Form
  , inputAmount :: Number
  , selectedAsset :: TokenType
  , collateralAsset :: TokenType
  , selectedTermType :: String  -- "spot", "hourly", "daily", "weekly"
  -- Gateway Form
  , showGateway :: Boolean
  , jitoSOLAmount :: Number
  , feelsSOLAmount :: Number
  -- Simulation Form
  , simulationConfig :: SimulationConfig
  , simulationResults :: Maybe SimulationResults
  , simulationRunning :: Boolean
  -- Token Creation Form
  , tokenTicker :: String
  , tokenName :: String
  , tokenValidationErrors :: Array String
  -- Launch System Form
  , selectedLaunchId :: Maybe String
  , launchBidAmount :: Number
  , launchPriorityFeePercent :: Number
  , activeLaunches :: Array LaunchInfo
  -- UI State
  , loading :: Boolean
  , error :: Maybe String
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
  { currentUser: "main-user"
  -- Position Creation Form
  , inputAmount: 100.0
  , selectedAsset: FeelsSOL
  , collateralAsset: JitoSOL
  , selectedTermType: "spot"
  -- Gateway Form
  , showGateway: true
  , jitoSOLAmount: 100.0
  , feelsSOLAmount: 100.0
  -- Simulation Form
  , simulationConfig: defaultSimulationConfig
  , simulationResults: Nothing
  , simulationRunning: false
  -- Token Creation Form
  , tokenTicker: ""
  , tokenName: ""
  , tokenValidationErrors: []
  -- Launch System Form
  , selectedLaunchId: Nothing
  , launchBidAmount: 100.0
  , launchPriorityFeePercent: 10.0
  , activeLaunches: []
  -- UI State
  , loading: true
  , error: Nothing
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
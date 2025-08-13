-- UI Component State for the Feels Protocol application
-- Manages only UI-specific state (form inputs, display preferences, etc.)
module UI.State
  ( UIState
  , LaunchInfo
  , SystemStats
  , Action(..)
  , initialUIState
  , defaultSimulationConfig
  , parseTokenType
  ) where

import Data.Maybe (Maybe(..))

import Protocol.Token (TokenType(..))
import Protocol.Common (Position, TokenMetadata)
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..))
import UI.ProtocolState (AppRuntime)

--------------------------------------------------------------------------------
-- UI State Types
--------------------------------------------------------------------------------

-- UI-only state (form inputs, display preferences, etc.)
type UIState =
  { -- Current user context
    currentUser :: String
  -- Protocol API runtime
  , api :: Maybe AppRuntime
  -- Position Creation Form
  , inputAmount :: Number
  , selectedAsset :: TokenType
  , collateralAsset :: TokenType
  , selectedTermType :: String  -- "spot", "monthly"
  -- FeelsSOL Form
  , showFeelsSOL :: Boolean
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
  -- Protocol Data (cached for UI display)
  , userPositions :: Array Position
  , lenderOffers :: Array Position
  , protocolStats :: Maybe SystemStats
  , userTokens :: Array TokenMetadata
  , jitoBalance :: Number
  , feelsBalance :: Number
  , priceHistory :: Array { timestamp :: Number, block :: Int, price :: Number, polValue :: Number, tokens :: Array { ticker :: String, price :: Number, polFloor :: Number, live :: Boolean } }
  }

-- Launch info type
type LaunchInfo =
  { launchId :: String
  , tokenTicker :: String
  , currentPhase :: String
  , currentPrice :: Number
  , totalDistributed :: Number
  }

-- System stats type (cached from protocol queries)
type SystemStats =
  { totalValueLocked :: Number
  , totalUsers :: Int
  , activePositions :: Int
  , liveTokens :: Int
  , totalLenderOffers :: Int
  , polBalance :: Number
  , feelsSOLSupply :: Number
  , jitoSOLLocked :: Number
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
  -- FeelsSOL
  | ToggleFeelsSOL
  | UpdateJitoSOLAmount Number
  | UpdateFeelsSOLAmount Number
  | EnterFeelsSOL
  | ExitFeelsSOL
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
  , api: Nothing
  -- Position Creation Form
  , inputAmount: 100.0
  , selectedAsset: FeelsSOL
  , collateralAsset: JitoSOL
  , selectedTermType: "spot"
  -- FeelsSOL Form
  , showFeelsSOL: true
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
  -- Protocol Data (cached for UI display)
  , userPositions: []
  , lenderOffers: []
  , protocolStats: Nothing
  , userTokens: []
  , jitoBalance: 0.0
  , feelsBalance: 0.0
  , priceHistory: []
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
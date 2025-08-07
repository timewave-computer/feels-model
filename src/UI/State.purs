-- UI State Management for the Feels Protocol application
-- Manages all UI state types, initial state, and state coordination
module UI.State
  ( UIState
  , Action(..)
  , initialUIState
  , defaultSimulationConfig
  , parseTokenType
  ) where

import Prelude
import Data.Maybe (Maybe(..))

import API (APIRuntime)
import Token (TokenType(..), TokenMetadata)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..))
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..))

--------------------------------------------------------------------------------
-- UI State Types
--------------------------------------------------------------------------------

type UIState =
  { api :: Maybe APIRuntime
  , currentUser :: String
  -- Position Creation
  , inputAmount :: Number
  , selectedAsset :: TokenType
  , collateralAsset :: TokenType
  , unbondingPeriod :: UnbondingPeriod
  , leverage :: Number
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
  , userPositions :: Array LendingRecord
  , lenderOffers :: Array LendingRecord
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
  | SetUnbondingPeriod UnbondingPeriod
  | SetLeverage Number
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
  , unbondingPeriod: NoBonding
  , leverage: 2.0
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
  , leveragePreference: 0.3
  , stakingPreference: 0.5
  }

--------------------------------------------------------------------------------
-- State Utilities
--------------------------------------------------------------------------------

-- Parse token type from string value
parseTokenType :: String -> TokenType
parseTokenType "JitoSOL" = JitoSOL
parseTokenType "FeelsSOL" = FeelsSOL
parseTokenType ticker = Token ticker
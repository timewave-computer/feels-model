-- | Type definitions for the Feels Protocol application state.
-- | This module contains all state-related types with no logic,
-- | serving as the shared contract between state modules.
module State.Types
  ( AppState
  , AppRuntime
  , AppCommand(..)
  , AppQuery(..)
  , AppResult(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Ref (Ref)

-- Import domain types needed for state
import Token (TokenType, TokenMetadata, TokenRegistry)
import Position (Position, TermCommitment)
import PoolRegistry (PoolRegistry)
import Gateway (GatewayState)
import POL (POLState)
import Oracle (Oracle)
import Incentives (MarketDynamics)
import Accounts (AccountRegistry)
import Launch.Orchestrator (LaunchOrchestrator, ActiveLaunch, LaunchStatus)

--------------------------------------------------------------------------------
-- State Types
--------------------------------------------------------------------------------

-- | Complete application state containing all protocol subsystems
type AppState =
  { tokenRegistry :: TokenRegistry
  , poolRegistry :: PoolRegistry
  , gateway :: GatewayState
  , polState :: POLState
  , oracle :: Oracle
  , marketDynamics :: MarketDynamics
  , accounts :: AccountRegistry
  , launchOrchestrator :: LaunchOrchestrator
  , positionTokenMap :: Array { positionId :: Int, tokenTicker :: String }
  , currentUser :: String  -- For demo purposes, in production would be wallet-based
  , currentBlock :: Int     -- Current block number
  , timestamp :: Number     -- Timestamp for UI display only
  , lastJitoSOLPrice :: Number  -- Track JitoSOL price for rebase capture
  }

-- | Runtime wrapper for application state with event system
type AppRuntime =
  { state :: Ref AppState
  , listeners :: Ref (Array { id :: Int, callback :: AppState -> Effect Unit })
  , nextListenerId :: Ref Int
  }

--------------------------------------------------------------------------------
-- Command Types
--------------------------------------------------------------------------------

-- | Commands that modify protocol state
data AppCommand
  = CreateToken String String String  
    -- ^ creator, ticker, name
  | CreatePosition String TokenType Number TokenType Number TermCommitment (Maybe String)  
    -- ^ user, lendAsset, amount, collateralAsset, collateralAmount, term, targetToken
  | TransferTokens String String TokenType Number  
    -- ^ from, to, token, amount
  | EnterGateway String Number  
    -- ^ user, jitoSOLAmount
  | ExitGateway String Number   
    -- ^ user, feelsSOLAmount
  | InitiateUnbonding String Int  
    -- ^ user, positionId
  | WithdrawPosition String Int   
    -- ^ user, positionId
  | StartLaunch String String Number Number
    -- ^ creator, ticker, initialPrice, totalTokens
  | SubmitLaunchBid String String Number Number
    -- ^ launchId, bidder, baseAmount, priorityFee
  | ProcessLaunchBatch String
    -- ^ launchId

derive instance eqAppCommand :: Eq AppCommand

--------------------------------------------------------------------------------
-- Query Types
--------------------------------------------------------------------------------

-- | Queries that read protocol state
data AppQuery
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

derive instance eqAppQuery :: Eq AppQuery

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Results from protocol operations
data AppResult
  = TokenCreated TokenMetadata
  | PositionCreated Position
  | TokensTransferred { from :: String, to :: String, token :: TokenType, amount :: Number }
  | GatewayEntered { user :: String, feelsSOLMinted :: Number }
  | GatewayExited { user :: String, jitoSOLReceived :: Number }
  | UnbondingInitiated Int
  | PositionWithdrawn Int
  | TokenList (Array TokenMetadata)
  | PositionList (Array Position)
  | Balance Number
  | TokenInfo (Maybe TokenMetadata)
  | LenderOfferList (Array Position)
  | SystemStatsResult 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , polBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
  | POLMetricsResult 
    { balance :: Number
    , growthRate24h :: Number
    }
  | TargetTokenInfo (Maybe String)
  | LaunchCreated String  -- launchId
  | LaunchBidSubmitted
  | LaunchBatchProcessed 
    { tokensDistributed :: Number
    , newPhase :: Maybe String
    , isComplete :: Boolean
    }
  | ActiveLaunchList (Array ActiveLaunch)
  | LaunchStatusResult (Maybe LaunchStatus)

derive instance eqAppResult :: Eq AppResult
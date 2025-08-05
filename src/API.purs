-- Clean web application interface for the Feels Protocol.
-- Provides a simplified API that delegates to the core state management system.
module API
  ( APIState
  , APIRuntime
  , APICommand(..)
  , APIQuery(..)
  , APIResult(..)
  , APIError(..)
  , initAPI
  , executeCommand
  , executeQuery
  , subscribe
  , unsubscribe
  ) where

import Prelude
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- Import core state management
import State as State
import State (AppState, AppRuntime)

-- Import types needed for API
import Token (TokenType, TokenMetadata)
import LendingRecord (LendingRecord, LendingTerms)

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- API state is just the core app state
type APIState = AppState

-- API runtime is just the core app runtime  
type APIRuntime = AppRuntime

-- API commands map directly to app commands
data APICommand
  = CreateToken String String String
  | CreateLendingPosition String TokenType Number TokenType Number LendingTerms (Maybe String)
  | TransferTokens String String TokenType Number
  | EnterGateway String Number
  | ExitGateway String Number
  | InitiateUnbonding String Int
  | WithdrawPosition String Int

-- API queries map directly to app queries
data APIQuery
  = GetUserTokens String
  | GetAllTokens
  | GetUserPositions String
  | GetUserBalance String TokenType
  | GetTokenByTicker String
  | GetLenderOffers
  | GetSystemStats
  | GetNFVMetrics
  | GetPositionTargetToken Int

-- API results map directly to app results
data APIResult
  = TokenCreated TokenMetadata
  | PositionCreated LendingRecord
  | TokensTransferred { from :: String, to :: String, token :: TokenType, amount :: Number }
  | GatewayEntered { user :: String, feelsSOLMinted :: Number }
  | GatewayExited { user :: String, jitoSOLReceived :: Number }
  | UnbondingInitiated Int
  | PositionWithdrawn Int
  | TokenList (Array TokenMetadata)
  | PositionList (Array LendingRecord)
  | Balance Number
  | TokenInfo (Maybe TokenMetadata)
  | LenderOfferList (Array LendingRecord)
  | SystemStatsResult 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , nfvBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
  | NFVMetricsResult 
    { balance :: Number
    , growthRate24h :: Number
    , utilizationRate :: Number
    }
  | TargetTokenInfo (Maybe String)

-- API errors map directly to app errors
data APIError
  = InvalidCommand String
  | InsufficientBalance String
  | TokenNotFound String
  | PositionNotFound Int
  | UserNotFound String
  | InvalidAmount Number
  | SystemError String

derive instance eqAPIError :: Eq APIError

instance showAPIError :: Show APIError where
  show (InvalidCommand msg) = "Invalid command: " <> msg
  show (InsufficientBalance msg) = "Insufficient balance: " <> msg
  show (TokenNotFound ticker) = "Token not found: " <> ticker
  show (PositionNotFound id) = "Position not found: " <> show id
  show (UserNotFound user) = "User not found: " <> user
  show (InvalidAmount amount) = "Invalid amount: " <> show amount
  show (SystemError msg) = "System error: " <> msg

--------------------------------------------------------------------------------
-- API Interface Functions
--------------------------------------------------------------------------------

-- Initialize API by delegating to state initialization
initAPI :: Effect APIRuntime
initAPI = State.initState

-- Execute command by converting and delegating to state
executeCommand :: APIRuntime -> APICommand -> Effect (Either APIError APIResult)
executeCommand runtime cmd = do
  let stateCmd = convertCommand cmd
  result <- State.executeCommand runtime stateCmd
  pure $ convertCommandResult result

-- Execute query by converting and delegating to state
executeQuery :: APIRuntime -> APIQuery -> Effect (Either APIError APIResult) 
executeQuery runtime query = do
  let stateQuery = convertQuery query  
  result <- State.executeQuery runtime stateQuery
  pure $ convertQueryResult result

-- Subscribe to state changes (delegates directly)
subscribe :: APIRuntime -> (APIState -> Effect Unit) -> Effect Int
subscribe = State.subscribe

-- Unsubscribe from state changes (delegates directly)
unsubscribe :: APIRuntime -> Int -> Effect Unit
unsubscribe = State.unsubscribe

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- Convert API command to State command
convertCommand :: APICommand -> State.AppCommand
convertCommand = case _ of
  CreateToken creator ticker name -> State.CreateToken creator ticker name
  CreateLendingPosition user lendAsset amount collateralAsset collateralAmount terms targetToken ->
    State.CreateLendingPosition user lendAsset amount collateralAsset collateralAmount terms targetToken
  TransferTokens from to token amount -> State.TransferTokens from to token amount
  EnterGateway user jitoAmount -> State.EnterGateway user jitoAmount
  ExitGateway user feelsAmount -> State.ExitGateway user feelsAmount
  InitiateUnbonding user positionId -> State.InitiateUnbonding user positionId  
  WithdrawPosition user positionId -> State.WithdrawPosition user positionId

-- Convert API query to State query
convertQuery :: APIQuery -> State.AppQuery
convertQuery = case _ of
  GetUserTokens user -> State.GetUserTokens user
  GetAllTokens -> State.GetAllTokens
  GetUserPositions user -> State.GetUserPositions user
  GetUserBalance user tokenType -> State.GetUserBalance user tokenType
  GetTokenByTicker ticker -> State.GetTokenByTicker ticker
  GetLenderOffers -> State.GetLenderOffers
  GetSystemStats -> State.GetSystemStats
  GetNFVMetrics -> State.GetNFVMetrics
  GetPositionTargetToken positionId -> State.GetPositionTargetToken positionId

-- Convert State command result to API result
convertCommandResult :: Either State.AppError State.AppResult -> Either APIError APIResult
convertCommandResult = case _ of
  Left err -> Left $ convertError err
  Right result -> Right $ convertResult result

-- Convert State query result to API result  
convertQueryResult :: Either State.AppError State.AppResult -> Either APIError APIResult
convertQueryResult = case _ of
  Left err -> Left $ convertError err
  Right result -> Right $ convertResult result

-- Convert State error to API error
convertError :: State.AppError -> APIError
convertError = case _ of
  State.InvalidCommand msg -> InvalidCommand msg
  State.InsufficientBalance msg -> InsufficientBalance msg
  State.TokenNotFound ticker -> TokenNotFound ticker
  State.PositionNotFound id -> PositionNotFound id
  State.UserNotFound user -> UserNotFound user
  State.InvalidAmount amount -> InvalidAmount amount
  State.SystemError msg -> SystemError msg

-- Convert State result to API result
convertResult :: State.AppResult -> APIResult
convertResult = case _ of
  State.TokenCreated meta -> TokenCreated meta
  State.PositionCreated record -> PositionCreated record
  State.TokensTransferred transferData -> TokensTransferred transferData
  State.GatewayEntered entryData -> GatewayEntered entryData
  State.GatewayExited exitData -> GatewayExited exitData
  State.UnbondingInitiated id -> UnbondingInitiated id
  State.PositionWithdrawn id -> PositionWithdrawn id
  State.TokenList tokens -> TokenList tokens
  State.PositionList positions -> PositionList positions
  State.Balance amount -> Balance amount
  State.TokenInfo info -> TokenInfo info
  State.LenderOfferList offers -> LenderOfferList offers
  State.SystemStatsResult stats -> SystemStatsResult stats
  State.NFVMetricsResult metrics -> NFVMetricsResult metrics
  State.TargetTokenInfo info -> TargetTokenInfo info
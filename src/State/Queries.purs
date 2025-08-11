-- | Query processing for the Feels Protocol.
-- | This module handles all read-only operations on protocol state.
module State.Queries
  ( queryHandlers
  , executeQuery
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (filter, find, nub, length)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Ref (read)

-- Import state types
import State.Types (AppState, AppRuntime, AppQuery(..), AppResult(..))

-- Import domain modules
import Token (TokenType(..), getAllTokens)
import PoolRegistry (getUserPositions, getAllPositions)
import POL (getTotalPOL, getPOLMetrics)
import Accounts (getFeelsAccountBalance, getTotalTokenBalance)
import Errors (ProtocolError(..))
import Launch.Orchestrator as LO

--------------------------------------------------------------------------------
-- Query Handlers
--------------------------------------------------------------------------------

-- | Handle user tokens query
handleGetUserTokens :: String -> AppState -> Effect (Either ProtocolError AppResult)
handleGetUserTokens _ _state = do
  -- In a real implementation, we'd filter tokens by owner
  -- For now, return empty array as we don't track ownership in TokenRegistry
  pure $ Right $ TokenList []

-- | Handle all tokens query
handleGetAllTokens :: AppState -> Effect (Either ProtocolError AppResult)
handleGetAllTokens state = do
  allTokens <- getAllTokens state.tokenRegistry
  pure $ Right $ TokenList allTokens

-- | Handle user positions query
handleGetUserPositions :: String -> AppState -> Effect (Either ProtocolError AppResult)
handleGetUserPositions user state = do
  positions <- getUserPositions user state.poolRegistry
  pure $ Right $ PositionList positions

-- | Handle user balance query
handleGetUserBalance :: String -> TokenType -> AppState -> Effect (Either ProtocolError AppResult)
handleGetUserBalance user tokenType state = do
  balance <- getFeelsAccountBalance state.accounts user tokenType
  pure $ Right $ Balance balance

-- | Handle lender offers query
handleGetLenderOffers :: AppState -> Effect (Either ProtocolError AppResult)
handleGetLenderOffers state = do
  offers <- getAllPositions state.poolRegistry
  pure $ Right $ LenderOfferList offers

-- | Handle system stats query
handleGetSystemStats :: AppState -> Effect (Either ProtocolError AppResult)
handleGetSystemStats state = do
  activePositions <- getAllPositions state.poolRegistry
  -- Calculate total value locked (sum of all position amounts)
  let totalValueLocked = sum (map (\r -> r.amount) activePositions)
  -- Count unique users from all positions
  let uniqueUsers = nub (map (\r -> r.owner) activePositions)
  let userCount = if length uniqueUsers == 0 then 1 else length uniqueUsers
  -- Count live tokens from token registry
  tokenList <- getAllTokens state.tokenRegistry
  let liveCount = length (filter (\t -> t.live) tokenList)
  -- Get POL balance
  polBalance <- getTotalPOL state.polState
  -- Get total lender offers (all active positions can be offers)
  -- Calculate FeelsSOL supply and JitoSOL locked
  feelsSOLSupply <- getTotalTokenBalance state.accounts FeelsSOL
  jitoSOLLocked <- getTotalTokenBalance state.accounts JitoSOL
  pure $ Right $ SystemStatsResult
    { totalValueLocked: totalValueLocked  
    , totalUsers: userCount  
    , activePositions: length activePositions
    , liveTokens: liveCount
    , totalLenderOffers: length activePositions
    , polBalance: polBalance
    , feelsSOLSupply: feelsSOLSupply
    , jitoSOLLocked: jitoSOLLocked
    }

-- | Handle POL metrics query
handleGetPOLMetrics :: AppState -> Effect (Either ProtocolError AppResult)
handleGetPOLMetrics state = do
  balance <- getTotalPOL state.polState
  _metrics <- getPOLMetrics state.polState
  pure $ Right $ POLMetricsResult
    { balance
    , growthRate24h: 0.0  -- TODO: Implement growth rate calculation
    }

-- | Handle position target token query
handleGetPositionTargetToken :: Int -> AppState -> Effect (Either ProtocolError AppResult)
handleGetPositionTargetToken positionId state = do
  let targetToken = case find (\m -> m.positionId == positionId) state.positionTokenMap of
        Just mapping -> Just mapping.tokenTicker
        Nothing -> Nothing
  pure $ Right $ TargetTokenInfo targetToken

-- | Handle get active launches query
handleGetActiveLaunches :: AppState -> Effect (Either ProtocolError AppResult)
handleGetActiveLaunches state = do
  launches <- LO.getActiveLaunches state.launchOrchestrator
  pure $ Right $ ActiveLaunchList launches

-- | Handle get launch status query
handleGetLaunchStatus :: String -> AppState -> Effect (Either ProtocolError AppResult)
handleGetLaunchStatus launchId state = do
  status <- LO.getLaunchStatus launchId state.launchOrchestrator
  pure $ Right $ LaunchStatusResult status

--------------------------------------------------------------------------------
-- Query Processing
--------------------------------------------------------------------------------

-- | Define query handlers for all state reads
queryHandlers :: AppState -> AppQuery -> Effect (Either ProtocolError AppResult)
queryHandlers state = case _ of
  GetUserTokens user -> 
    handleGetUserTokens user state
  
  GetAllTokens -> 
    handleGetAllTokens state
  
  GetUserPositions user -> 
    handleGetUserPositions user state
  
  GetUserBalance user tokenType -> 
    handleGetUserBalance user tokenType state
  
  GetLenderOffers -> 
    handleGetLenderOffers state
  
  GetSystemStats -> 
    handleGetSystemStats state
  
  GetPOLMetrics -> 
    handleGetPOLMetrics state
  
  GetPositionTargetToken positionId -> 
    handleGetPositionTargetToken positionId state
  
  GetActiveLaunches ->
    handleGetActiveLaunches state
  
  GetLaunchStatus launchId ->
    handleGetLaunchStatus launchId state
  
  _ -> pure $ Left $ InvalidCommandError "Query not implemented"

-- | Execute a query that reads state
executeQuery :: AppRuntime -> AppQuery -> Effect (Either ProtocolError AppResult)
executeQuery runtime query = do
  state <- read runtime.state
  queryHandlers state query
-- | Query types and handlers for off-chain protocol reads.
-- | This module handles all read-only operations on indexed protocol state.
module UI.Query
  ( -- Query execution
    executeQuery
  , -- Query handlers (exported for testing)
    handleGetUserTokens
  , handleGetAllTokens
  , handleGetUserPositions
  , handleGetUserBalance
  , handleGetTokenByTicker
  , handleGetLenderOffers
  , handleGetSystemStats
  , handleGetPOLMetrics
  , handleGetPositionTargetToken
  , handleGetActiveLaunches
  , handleGetLaunchStatus
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (filter, find, nub, length)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Ref (Ref, read)
import Unsafe.Coerce (unsafeCoerce)
import UI.Util.Codecs (decodeTokenArray, decodePositionArray)

-- Import protocol modules for data access
import UI.TokenRegistry (getAllTokens)
import UI.PoolRegistry (getUserPositions, getAllPositions, getPool)
import Protocol.ProtocolVault (getProtocolMetrics)
-- syncPositionValue removed as it doesn't exist
import UI.Account (getFeelsAccountBalance, getTotalTokenBalance)
import Protocol.Error (ProtocolError(..))
import Protocol.LaunchVault as Launch
import Protocol.LaunchVault (LaunchEntry, LaunchStrategy)
import Protocol.Vault (LedgerVault)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Function (identity)
import Effect.Ref (Ref, read)
import Protocol.PositionVault as P

-- Import app state and result types
import UI.ProtocolState (ProtocolState, IndexerQuery(..))
import Protocol.Common (QueryResult(..), TokenMetadata, Position, LaunchResult)
import Protocol.Token (TokenType(..))

--------------------------------------------------------------------------------
-- Query Execution
--------------------------------------------------------------------------------

-- | Execute an indexer query
executeQuery :: IndexerQuery -> ProtocolState -> Effect (Either ProtocolError QueryResult)
executeQuery query state = case query of
  GetUserTokens user -> handleGetUserTokens user state
  GetAllTokens -> handleGetAllTokens state
  GetUserPositions user -> handleGetUserPositions user state
  GetUserBalance user tokenType -> handleGetUserBalance user tokenType state
  GetTokenByTicker ticker -> handleGetTokenByTicker ticker state
  GetLenderOffers -> handleGetLenderOffers state
  GetSystemStats -> handleGetSystemStats state
  GetPOLMetrics -> handleGetPOLMetrics state
  GetPositionTargetToken positionId -> handleGetPositionTargetToken positionId state
  GetActiveLaunches -> handleGetActiveLaunches state
  GetLaunchStatus poolId -> handleGetLaunchStatus poolId state

--------------------------------------------------------------------------------
-- Query Handlers
--------------------------------------------------------------------------------

-- | Handle user tokens query
handleGetUserTokens :: String -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetUserTokens user state = do
  -- Get all tokens and filter by creator
  allTokens <- getAllTokens state.tokenRegistry
  let userTokens = filter (\t -> t.creator == user) allTokens
      -- Convert from Token.TokenMetadata to foreign TokenMetadata type
      convertedTokens = unsafeCoerce userTokens :: Array TokenMetadata
  pure $ Right $ TokenList convertedTokens

-- | Handle all tokens query
handleGetAllTokens :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetAllTokens state = do
  allTokens <- getAllTokens state.tokenRegistry
  -- Convert from Token.TokenMetadata to foreign TokenMetadata type
  let convertedTokens = unsafeCoerce allTokens :: Array TokenMetadata
  pure $ Right $ TokenList convertedTokens

-- | Handle user positions query
handleGetUserPositions :: String -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetUserPositions user state = do
  positions <- getUserPositions user state.poolRegistry
  
  -- For MVP, return positions as-is (sync functionality not implemented)
  let syncedPositions = positions
  
  -- Convert from P.Position to foreign Position type
  let convertedPositions = unsafeCoerce syncedPositions :: Array Position
  pure $ Right $ PositionList convertedPositions

-- | Handle user balance query
handleGetUserBalance :: String -> TokenType -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetUserBalance user tokenType state = do
  balance <- getFeelsAccountBalance state.accounts user tokenType
  pure $ Right $ Balance balance

-- | Handle token by ticker query
handleGetTokenByTicker :: String -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetTokenByTicker ticker state = do
  allTokens <- getAllTokens state.tokenRegistry
  let maybeToken = find (\t -> t.ticker == ticker) allTokens
      -- Convert from Token.TokenMetadata to foreign TokenMetadata type
      convertedToken = unsafeCoerce maybeToken :: Maybe TokenMetadata
  pure $ Right $ TokenInfo convertedToken

-- | Handle lender offers query
handleGetLenderOffers :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetLenderOffers state = do
  offers <- getAllPositions state.poolRegistry
  -- Convert from P.Position to foreign Position type
  let convertedOffers = unsafeCoerce offers :: Array Position
  pure $ Right $ LenderOfferList convertedOffers

-- | Handle system stats query
handleGetSystemStats :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetSystemStats state = do
  activePositions <- getAllPositions state.poolRegistry
  -- Calculate total value locked (sum of all position amounts)
  let totalValueLocked = sum (map (\r -> r.metrics.amount) activePositions)
  -- Count unique users from all positions
  let uniqueUsers = nub (map (\r -> r.owner) activePositions)
  let userCount = if length uniqueUsers == 0 then 1 else length uniqueUsers
  -- Count live tokens from token registry
  tokenList <- getAllTokens state.tokenRegistry
  let liveCount = length (filter (\t -> t.live) tokenList)
  -- Get POL balance
  polBalance <- read state.polState >>= _.getTotalBalance
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
handleGetPOLMetrics :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetPOLMetrics state = do
  balance <- read state.polState >>= _.getTotalBalance
  _metrics <- getProtocolMetrics state.polState
  let growthRate = 0.0  -- TODO: Implement growth rate calculation
  pure $ Right $ POLMetricsResult
    { balance
    , growthRate24h: growthRate
    }

-- | Handle position target token query
handleGetPositionTargetToken :: Int -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetPositionTargetToken positionId state = do
  let maybeTarget = find (\m -> m.positionId == positionId) state.positionTokenMap
  case maybeTarget of
    Just mapping -> pure $ Right $ TargetTokenInfo (Just mapping.tokenTicker)
    Nothing -> pure $ Right $ TargetTokenInfo Nothing

-- | Handle active launches query
handleGetActiveLaunches :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetActiveLaunches state = do
  -- Get all launches from the map
  let launchPairs = Map.toUnfoldable state.launches :: Array (Tuple String (Ref (LedgerVault LaunchEntry LaunchStrategy)))
  activeLaunches <- traverse (\(Tuple poolId launchRef) -> do
    status <- Launch.getLaunchStatus launchRef
    if status.isActive
      then pure $ Just { poolId, phase: show status.phase }
      else pure Nothing
  ) launchPairs
  
  let filtered = Array.mapMaybe identity activeLaunches
  pure $ Right $ ActiveLaunchesList filtered

-- | Handle launch status query
handleGetLaunchStatus :: String -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetLaunchStatus poolId state = do
  case Map.lookup poolId state.launches of
    Nothing -> pure $ Right $ LaunchStatusResult Nothing
    Just launchRef -> do
      result <- Launch.getLaunchStatus launchRef
      -- Convert from Launch.LaunchStatus to foreign LaunchResult type
      let convertedResult = unsafeCoerce result :: LaunchResult
      pure $ Right $ LaunchStatusResult (Just convertedResult)
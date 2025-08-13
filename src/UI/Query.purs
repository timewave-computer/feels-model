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
  , handleGetActiveOfferings
  , handleGetOfferingStatus
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (filter, find, nub, length)
import Data.Foldable (sum)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

-- Import protocol modules for data access
import UI.TokenRegistry (getAllTokens)
import UI.PoolRegistry (getUserPositions, getAllPositions)
import Protocol.POL (getTotalPOL, getPOLMetrics, calculateGrowthRate24h)
import UI.Account (getFeelsAccountBalance, getTotalTokenBalance)
import Protocol.Error (ProtocolError(..))
import Protocol.Offering as Offering
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Array as Array
import Data.Function (identity)
import Effect.Ref (read)
import Protocol.Position as P

-- Import app state and result types
import UI.ProtocolState (ProtocolState, IndexerQuery(..))
import Protocol.Common (QueryResult(..), TokenMetadata, Position, OfferingResult)
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
  GetActiveOfferings -> handleGetActiveOfferings state
  GetOfferingStatus poolId -> handleGetOfferingStatus poolId state

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
  -- Convert from P.Position to foreign Position type
  let convertedPositions = unsafeCoerce positions :: Array Position
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
handleGetPOLMetrics :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetPOLMetrics state = do
  balance <- getTotalPOL state.polState
  _metrics <- getPOLMetrics state.polState
  growthRate <- calculateGrowthRate24h state.polState
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

-- | Handle active offerings query
handleGetActiveOfferings :: ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetActiveOfferings state = do
  -- Get all offerings from the map
  let offeringPairs = Map.toUnfoldable state.offerings :: Array (Tuple String Offering.OfferingState)
  activeOfferings <- traverse (\(Tuple poolId offeringRef) -> do
    offering <- read offeringRef
    if offering.isActive
      then pure $ Just { poolId, phase: show offering.currentPhase }
      else pure Nothing
  ) offeringPairs
  
  let filtered = Array.mapMaybe identity activeOfferings
  pure $ Right $ ActiveOfferingsList filtered

-- | Handle offering status query
handleGetOfferingStatus :: String -> ProtocolState -> Effect (Either ProtocolError QueryResult)
handleGetOfferingStatus poolId state = do
  case Map.lookup poolId state.offerings of
    Nothing -> pure $ Right $ OfferingStatusResult Nothing
    Just offeringRef -> do
      result <- Offering.getOfferingStatus offeringRef
      -- Convert from Offering.OfferingResult to foreign OfferingResult type
      let convertedResult = unsafeCoerce result :: OfferingResult
      pure $ Right $ OfferingStatusResult (Just convertedResult)
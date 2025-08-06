-- UI Integration for the Feels Protocol application
-- Handles integration with external systems like API, WebSocket, Chart.js, and simulation processing
module UI.Integration
  ( initializeAPI
  , initializeRemoteActions
  , refreshProtocolData
  , processSimulationResults
  ) where

import Prelude
import Data.Array ((:), length, find, head, null, filter, sortBy, drop, take, groupBy, zip)
import Data.Functor (map)
import Data.Traversable (traverse, traverse_)
import Data.Array.NonEmpty as NEA
import Data.String.Common (trim, joinWith)
import Data.String as String
import Data.Number as Number
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), fromLeft)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (new, read, write)
import Data.Functor (void)

-- Import API and data types
import API as A
import API (APIRuntime, APIResult(..), initAPI, executeCommand, executeQuery)
import Token (TokenType(..), TokenMetadata)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), LendingSide(..), getAvailableAmount)
import FFI (setTimeout, getElementById, getValue, triggerUIAction, registerRemoteAction, checkAndInitializeChart)
import Data.Nullable (toMaybe)
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..), TradingAction(..), initSimulationWithLendingBook, executeSimulation, calculateResults)
import Simulation.Sim as S
import Oracle (PriceObservation)

--------------------------------------------------------------------------------
-- API Initialization
--------------------------------------------------------------------------------

-- Initialize the API runtime
initializeAPI :: Effect APIRuntime
initializeAPI = do
  log "Initializing protocol API..."
  initAPI

--------------------------------------------------------------------------------
-- Remote Action Integration (WebSocket)
--------------------------------------------------------------------------------

-- Initialize remote actions for WebSocket control
initializeRemoteActions :: Effect Unit
initializeRemoteActions = do
  -- Store a reference for triggering Halogen actions from outside
  ref <- new Nothing
  
  registerRemoteAction "runSimulation" \params -> do
    log "Remote action: runSimulation triggered via WebSocket"
    -- Click the simulation button directly
    button <- getElementById "run-simulation-btn"
    case toMaybe button of
      Just btn -> do
        log "Found simulation button, clicking..."
        -- Dispatch click event
        void $ setTimeout (do
          success <- triggerUIAction "runSimulation"
          log $ "Trigger result: " <> show success
          pure unit
        ) 100
        pure { status: "success", message: "Simulation button click scheduled" }
      Nothing -> do
        log "ERROR: Simulation button not found!"
        pure { status: "error", message: "Simulation button not found" }
  
  registerRemoteAction "refreshData" \_ -> do
    log "Remote action: refreshData triggered"
    void $ triggerUIAction "refreshData"
    pure unit
  
  registerRemoteAction "getChartData" \_ -> do
    log "Remote action: getChartData triggered"
    -- Get chart data from DOM
    chartDataEl <- getElementById "chart-data"
    case toMaybe chartDataEl of
      Just el -> do
        chartData <- getValue el
        log $ "Chart data: " <> chartData
      Nothing -> log "Chart data element not found"
    pure unit
  
  registerRemoteAction "testTokenValidation" \_ -> do
    log "Remote action: testTokenValidation triggered"
    void $ triggerUIAction "testTokenValidation"
    pure unit

--------------------------------------------------------------------------------
-- Protocol Data Refresh
--------------------------------------------------------------------------------

-- Refresh all protocol data for a user
refreshProtocolData :: APIRuntime -> String -> Effect Unit
refreshProtocolData protocol currentUser = do
  log "Refreshing protocol data..."
  
  -- Get user positions
  posResult <- executeQuery protocol (A.GetUserPositions currentUser)
  case posResult of
    Right (PositionList positions) -> 
      log $ "Found " <> show (length positions) <> " user positions"
    Left err -> 
      log $ "Failed to get user positions: " <> show err
    _ -> pure unit
  
  -- Get lender offers
  offersResult <- executeQuery protocol A.GetLenderOffers
  case offersResult of
    Right (LenderOfferList offers) -> do
      log $ "Found " <> show (length offers) <> " lender offers"
      -- Log details about each offer for debugging
      traverse_ (\offer -> 
        log $ "  Offer #" <> show offer.id <> ": " <> 
              show offer.lendAmount <> " " <> show offer.lendAsset <> 
              " (status: " <> show offer.status <> ")"
      ) offers
    Left err -> 
      log $ "Failed to get lender offers: " <> show err
    _ -> pure unit
  
  -- Get protocol stats
  statsResult <- executeQuery protocol A.GetSystemStats
  case statsResult of
    Right (SystemStatsResult stats) -> do
      log $ "Updated protocol stats - TVL: " <> show stats.totalValueLocked <> ", Users: " <> show stats.totalUsers
    Left err ->
      log $ "Failed to get protocol stats: " <> show err
    _ -> pure unit

--------------------------------------------------------------------------------
-- Simulation Results Processing
--------------------------------------------------------------------------------

-- Process simulation results and return price history for charts
processSimulationResults :: APIRuntime -> S.SimulationState -> SimulationResults -> Effect (Array { timestamp :: Number, block :: Int, price :: Number, polValue :: Number, tokens :: Array { ticker :: String, price :: Number, polFloor :: Number, live :: Boolean } })
processSimulationResults protocol finalState results = do
  -- Process token creation actions from the simulation
  let tokenCreationActions = filter isCreateTokenAction finalState.actionHistory
  log $ "Processing " <> show (length tokenCreationActions) <> " token creation actions"
  
  _ <- traverse (processTokenCreation protocol) tokenCreationActions
  
  -- Get all tokens from the protocol (including those created during simulation)
  allTokensResult <- executeQuery protocol A.GetAllTokens
  
  let allTokens = case allTokensResult of
        Right (TokenList tokens) -> tokens
        _ -> []
      
      -- Include ALL live tokens (including system tokens) in the price history
      -- But exclude FeelsSOL (can't pair with itself) and duplicates from old casing
      liveTokens = filter (\t -> t.live && 
                                 t.ticker /= "FeelsSOL" &&  -- Exclude FeelsSOL - can't pair with itself
                                 t.ticker /= "jitoSOL" && 
                                 t.ticker /= "feelsSOL") allTokens
  
  log $ "Found " <> show (length allTokens) <> " total tokens, " <> show (length liveTokens) <> " live tokens"
  
  -- Get the oracle's actual price history from protocol state
  protocolState <- read protocol.state
  oracleState <- read protocolState.oracle.state
  let oraclePriceHistory = oracleState.priceHistory
  
  log $ "Oracle has " <> show (length oraclePriceHistory) <> " price observations"
  
  -- Convert oracle price history to chart format
  -- Sort observations by block number (not timestamp) to ensure proper ordering
  let sortedHistory = sortBy (\a b -> compare (fromMaybe 0 a.block) (fromMaybe 0 b.block)) oraclePriceHistory
      
      -- Debug: Log block numbers to see if we have all blocks
      blockNumbers = map (fromMaybe (-1)) $ map _.block sortedHistory
  
  log $ "Sorted history blocks: " <> show (take 20 blockNumbers) <> 
        " ... " <> show (drop (length blockNumbers - 5) blockNumbers)
  
  -- Check for gaps in block numbers
  let findFirstGap nums = case nums of
        [] -> Nothing
        [_] -> Nothing
        _ -> 
          let pairs = zip nums (drop 1 nums)
              gapPairs = filter (\(Tuple a b) -> b - a > 1) pairs
          in case head gapPairs of
               Just (Tuple a b) -> Just ("Gap from block " <> show a <> " to " <> show b)
               Nothing -> Nothing
  
  case findFirstGap blockNumbers of
    Just gap -> log $ "WARNING: Found gap in block sequence: " <> gap
    Nothing -> pure unit
      
  -- Group observations by block number instead of timestamp
  -- This ensures we get one chart point per block
  let groupByBlock obs = fromMaybe 0 obs.block
      grouped = groupBy (\a b -> groupByBlock a == groupByBlock b) sortedHistory
      
      -- Convert each group of observations to a chart point
      -- First filter to ensure we have valid groups, then map
      validGroups = filter (\neGroup -> 
        case head (NEA.toArray neGroup) of
          Just obs -> obs.timestamp > 0.0
          Nothing -> false
      ) grouped
      
      convertedPriceHistory = map (\neGroup ->
        let group = NEA.toArray neGroup
            timestamp = case head group of
              Just obs -> obs.timestamp
              Nothing -> 0.0  -- This shouldn't happen due to filter above
            
            -- Get block number from first observation
            blockNum = case head group of
              Just obs -> fromMaybe 0 obs.block  -- Default to 0 if no block
              Nothing -> 0
            
            -- Find JitoSOL price in this group
            jitoSOLObs = find (\obs -> obs.baseAsset == JitoSOL) group
            jitoPrice = case jitoSOLObs of
              Just obs -> obs.impliedPrice
              Nothing -> 1.22  -- Default JitoSOL price
            
            -- Get prices for all live tokens
            tokenPrices = map (\token ->
              let tokenObs = find (\obs ->
                    case obs.baseAsset of
                      Token t -> t == token.ticker
                      _ -> false
                  ) group
              in { ticker: token.ticker
                 , price: case tokenObs of
                     Just obs -> obs.impliedPrice
                     Nothing -> 0.0
                 , polFloor: case tokenObs of
                     Just obs -> obs.polFloor
                     Nothing -> 0.0
                 , live: token.live
                 }
            ) liveTokens
        in { timestamp: timestamp
           , block: blockNum
           , price: jitoPrice
           , polValue: jitoPrice * 0.98  -- POL tracks slightly below price
           , tokens: tokenPrices
           }
      ) validGroups
  
  log $ "Converted " <> show (length validGroups) <> " valid price groups to chart format (from " <> show (length grouped) <> " total)"
  
  -- Since the simulation used the protocol's actual lending book (shared by reference),
  -- the protocol's lending book is already updated. No need to write back the state.
  log "Simulation used shared lending book - protocol state automatically updated"
  
  pure convertedPriceHistory
  where
    isCreateTokenAction action = case action of
      S.CreateToken _ _ _ -> true
      _ -> false
    
    processTokenCreation protocol action = case action of
      S.CreateToken userId ticker name -> do
        log $ "Creating token: " <> ticker <> " for user: " <> userId
        result <- executeCommand protocol (A.CreateToken userId ticker name)
        case result of
          Right _ -> do
            log $ "Successfully created token: " <> ticker
            -- Immediately stake 100 FeelsSOL to make the token go live
            stakeResult <- executeCommand protocol 
              (A.CreateLendingPosition userId FeelsSOL 100.0 (Token ticker) 100.0 
                (StakingTerms Infinite) (Just ticker))
            case stakeResult of
              Right _ -> do
                log $ "Successfully staked 100 FeelsSOL for " <> ticker <> " to make it live"
                -- Add initial liquidity for token trading
                -- Token -> FeelsSOL: 100 tokens for 100 FeelsSOL (1:1 ratio)
                liquidity1 <- executeCommand protocol 
                  (A.CreateLendingPosition userId (Token ticker) 100.0 FeelsSOL 100.0 SwapTerms Nothing)
                case liquidity1 of
                  Right _ -> log $ "Added initial " <> ticker <> " -> FeelsSOL liquidity"
                  Left err -> log $ "Failed to add token liquidity: " <> show err
                -- FeelsSOL -> Token: 100 FeelsSOL for 100 tokens (1:1 ratio)
                liquidity2 <- executeCommand protocol 
                  (A.CreateLendingPosition userId FeelsSOL 100.0 (Token ticker) 100.0 SwapTerms Nothing)
                case liquidity2 of
                  Right _ -> log $ "Added initial FeelsSOL -> " <> ticker <> " liquidity"
                  Left err -> log $ "Failed to add FeelsSOL liquidity: " <> show err
              Left err -> log $ "Failed to stake for token launch: " <> show err
          Left err -> log $ "Failed to create token: " <> show err
      _ -> pure unit

--------------------------------------------------------------------------------
-- Chart Integration
--------------------------------------------------------------------------------

-- Initialize chart rendering after a delay
initializeChart :: Effect Unit
initializeChart = do
  log "Initializing price chart..."
  void $ setTimeout checkAndInitializeChart 250
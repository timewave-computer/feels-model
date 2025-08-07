-- UI Integration for the Feels Protocol application
-- Handles integration with external systems like API, WebSocket, Chart.js, and simulation processing
module UI.Integration
  ( initializeAPI
  , initializeRemoteActions
  , refreshProtocolData
  , processSimulationResults
  ) where

import Prelude
import Data.String as String
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
    chartDataEl <- getElementById "chart-data-hidden"
    case toMaybe chartDataEl of
      Just el -> do
        chartData <- getValue el
        log $ "Chart data: " <> chartData
      Nothing -> log "Chart data element not found"
    pure unit
  
  registerRemoteAction "debugChartElements" \_ -> do
    log "Remote action: debugChartElements triggered"
    -- Check canvas
    canvas <- getElementById "price-chart"
    case toMaybe canvas of
      Just _ -> log "Canvas element found"
      Nothing -> log "Canvas element NOT found"
    -- Check data element
    dataEl <- getElementById "chart-data-hidden"
    case toMaybe dataEl of
      Just el -> do
        content <- getValue el
        log $ "Data element found, length: " <> show (String.length content)
        log $ "Data preview: " <> String.take 100 content
      Nothing -> log "Data element NOT found"
    -- Also trigger UI action to check state
    void $ triggerUIAction "debugChartData"
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
      -- Log position details (Position type = LendingRecord)
      traverse_ (\offer -> 
        log $ "  Position #" <> show offer.id <> ": " <> 
              show offer.amount <> " " <> show offer.tokenPair.base
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
  oracleState <- read protocolState.oracle
  let oraclePriceHistory = oracleState.priceHistory
  
  log $ "Oracle has " <> show (length oraclePriceHistory) <> " price observations"
  
  -- Convert oracle price history to chart format
  -- Sort observations by block number (not timestamp) to ensure proper ordering
  let sortedHistory = sortBy (\a b -> compare a.timestamp b.timestamp) oraclePriceHistory
      
      -- Debug: Log timestamps instead of blocks
      timestamps = map _.timestamp sortedHistory
  
  log $ "Sorted history timestamps: " <> show (take 20 timestamps) <> 
        " ... " <> show (drop (length timestamps - 5) timestamps)
  
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
  
  case Nothing of  -- No block numbers in oracle observations
    Just gap -> log $ "WARNING: Found gap in block sequence: " <> gap
    Nothing -> pure unit
      
  -- Convert sorted observations directly to chart format
  let convertedPriceHistory = map (\obs ->
        let jitoPrice = if obs.tokenPair.base == JitoSOL 
                        then obs.price
                        else 1.22  -- Default JitoSOL price
            
            -- For now, just include the current observation's token if it's live
            tokenPrices = case obs.tokenPair.base of
              Token ticker -> 
                case find (\t -> t.ticker == ticker) liveTokens of
                  Just token -> 
                    [{ ticker: ticker
                     , price: obs.price
                     , polFloor: obs.price * 0.98  -- POL tracks slightly below price
                     , live: token.live
                     }]
                  Nothing -> []
              _ -> []
            
        in { timestamp: obs.timestamp
           , block: 0  -- Oracle doesn't track blocks
           , price: jitoPrice
           , polValue: jitoPrice * 0.98  -- POL tracks slightly below price
           , tokens: tokenPrices
           }
      ) sortedHistory
  
  log $ "Converted " <> show (length convertedPriceHistory) <> " price observations to chart format"
  
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
                (StakingTerms NoBonding) (Just ticker))
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
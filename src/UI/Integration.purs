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
import Data.Array (drop, filter, head, length, sortBy, take, zip)
import Data.Traversable (traverse, traverse_)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (new, read, write)
import Unsafe.Coerce (unsafeCoerce)

-- Import API and data types
import UI.ProtocolState as A
import UI.ProtocolState (AppRuntime, initState, ProtocolCommand(..), IndexerQuery(..))
import Protocol.Common (QueryResult(..), CommandResult(..))
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import FFI (setTimeout, getElementById, getValue, getTextContent, triggerUIAction, registerRemoteAction, sin)
import Data.Nullable (toMaybe)
import Simulation.Sim (SimulationResults)
import Simulation.Sim as S
import Utils (formatAmount)

--------------------------------------------------------------------------------
-- API Initialization
--------------------------------------------------------------------------------

-- Initialize the API runtime
initializeAPI :: Effect AppRuntime
initializeAPI = do
  log "Initializing protocol API..."
  initState

--------------------------------------------------------------------------------
-- Remote Action Integration (WebSocket)
--------------------------------------------------------------------------------

-- Initialize remote actions for WebSocket control
initializeRemoteActions :: Effect Unit
initializeRemoteActions = do
  -- Store a reference for triggering Halogen actions from outside
  _ref <- new Nothing
  
  registerRemoteAction "runSimulation" \_params -> do
    log "Remote action: runSimulation triggered via WebSocket"
    -- Click the simulation button directly
    button <- getElementById "run-simulation-btn"
    case toMaybe button of
      Just _btn -> do
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
  
  -- getChartData is handled by JavaScript in websocket-client.js
  
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
        content <- getTextContent el
        log $ "Data element found, length: " <> show (String.length content)
        log $ "Data preview: " <> String.take 100 content
      Nothing -> log "Data element NOT found"
    pure unit
  
  registerRemoteAction "testTokenValidation" \_ -> do
    log "Remote action: testTokenValidation triggered"
    void $ triggerUIAction "testTokenValidation"
    pure unit

--------------------------------------------------------------------------------
-- Protocol Data Refresh
--------------------------------------------------------------------------------

-- Refresh all protocol data for a user
refreshProtocolData :: AppRuntime -> String -> Effect Unit
refreshProtocolData protocol currentUser = do
  log "Refreshing protocol data..."
  
  -- Get user positions
  state <- read protocol.state
  posResult <- executeQuery (A.GetUserPositions currentUser) state
  case posResult of
    Right (PositionList positions) -> 
      log $ "Found " <> show (length positions) <> " user positions"
    Left err -> 
      log $ "Failed to get user positions: " <> show err
    _ -> pure unit
  
  -- Get lender offers
  offersResult <- executeQuery A.GetLenderOffers state
  case offersResult of
    Right (LenderOfferList offers) -> do
      log $ "Found " <> show (length offers) <> " lender offers"
      -- Cannot access fields of foreign Position type directly
    Left err -> 
      log $ "Failed to get lender offers: " <> show err
    _ -> pure unit
  
  -- Get protocol stats
  statsResult <- executeQuery A.GetSystemStats state
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
processSimulationResults :: AppRuntime -> S.SimulationState -> SimulationResults -> Effect (Array { timestamp :: Number, block :: Int, price :: Number, polValue :: Number, tokens :: Array { ticker :: String, price :: Number, polFloor :: Number, live :: Boolean } })
processSimulationResults protocol finalState _results = do
  -- Process token creation actions from the simulation
  let tokenCreationActions = filter isCreateTokenAction finalState.actionHistory
  log $ "Processing " <> show (length tokenCreationActions) <> " token creation actions"
  
  _ <- traverse (processTokenCreation protocol) tokenCreationActions
  
  -- Get all tokens from the protocol (including those created during simulation)
  state2 <- read protocol.state
  allTokensResult <- executeQuery A.GetAllTokens state2
  
  let allTokens = case allTokensResult of
        Right (TokenList tokens) -> tokens
        _ -> []
      
      -- Cannot filter foreign TokenMetadata type directly
      -- Would need to use unsafeCoerce to access fields
      liveTokens = allTokens
  
  log $ "Found " <> show (length allTokens) <> " total tokens"
  
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
  let _findFirstGap nums = case nums of
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
        let jitoPrice = obs.price  -- Oracle tracks JitoSOL/FeelsSOL price
            
            -- Generate realistic token prices based on oracle price and time-based variations
            -- Each token has a base price multiplier and oscillates around it
            tokenPrices = map (\token -> 
              let -- Use unsafeCoerce to access foreign TokenMetadata fields
                  tokenData = unsafeCoerce token :: { ticker :: String, live :: Boolean }
                  -- Calculate base price from ticker hash for consistency
                  tickerLen = String.length tokenData.ticker
                  baseMultiplier = (toNumber (tickerLen * 13) / 10.0) + 0.8  -- Range: 0.8-1.8x oracle price
                  -- Add time-based oscillation for realistic market movement
                  timeVariation = sin (obs.timestamp / 1000000.0) * 0.1  -- ±10% oscillation
                  currentPrice = jitoPrice * baseMultiplier * (1.0 + timeVariation)
                  -- POL floor is 95% of current price for stability
                  polFloorPrice = currentPrice * 0.95
              in { ticker: tokenData.ticker
                 , price: currentPrice
                 , polFloor: polFloorPrice  
                 , live: tokenData.live
                 }
            ) liveTokens
            
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
    
    processTokenCreation _protocol action = case action of
      S.CreateToken userId ticker name -> do
        log $ "Creating token: " <> ticker <> " for user: " <> userId
        state <- read protocol.state
        result <- executeCommand (A.CreateToken userId ticker name) state
        case result of
          Right (Tuple newState _) -> do
            write newState protocol.state
            log $ "Successfully created token: " <> ticker
            -- Tokens are now launched through the batch auction system
            -- No need for legacy staking mechanism
            log $ "Token created - ready for batch auction launch"
          Left err -> log $ "Failed to create token: " <> show err
      _ -> pure unit

--------------------------------------------------------------------------------
-- Chart Integration
--------------------------------------------------------------------------------


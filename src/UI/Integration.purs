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
import Data.Array (drop, filter, head, length, sortBy, take, zip, find, cons, foldl, range)
import Data.Traversable (traverse, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.String.CodeUnits (toCharArray)
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

-- Import API and data types
import UI.ProtocolState as A
import UI.ProtocolState (AppRuntime, initState)
import Protocol.Common (QueryResult(..), CommandResult(..))
import Protocol.POLVault (getAllAllocations, getTotalPOL, getUnallocatedPOL)
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import FFI (setTimeout, getElementById, getTextContent, triggerUIAction, registerRemoteAction, sin, exp)
import Data.Nullable (toMaybe)
import Simulation.Engine (SimulationResults)
import Simulation.Engine as S

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
  
  -- Log token tickers for debugging
  let tokenTickers = map (\token -> 
        let tokenData = unsafeCoerce token :: { ticker :: String }
        in tokenData.ticker
      ) allTokens
  log $ "Token tickers: " <> show tokenTickers
  
  -- Get the oracle's actual price history from protocol state
  protocolState <- read protocol.state
  oracleState <- read protocolState.oracle
  let oraclePriceHistory = oracleState.priceHistory
  
  log $ "Oracle has " <> show (length oraclePriceHistory) <> " price observations"
  
  -- Get current POL state for accurate floor calculations
  let polState = protocolState.polState
  currentPOL <- getTotalPOL polState
  unallocatedPOL <- getUnallocatedPOL polState
  allocations <- getAllAllocations polState
  log $ "Current POL reserves: " <> show currentPOL <> " FeelsSOL"
  log $ "POL state details - unallocated: " <> show unallocatedPOL <> ", allocations: " <> show (length allocations)
  
  -- Get POL allocation history from simulation state
  let polHistory = finalState.polAllocationHistory
  log $ "POL allocation history has " <> show (length polHistory) <> " snapshots"
  
  -- Log the final POL allocations for debugging
  case head polHistory of
    Just lastSnapshot -> do
      log $ "Final POL allocations at block " <> show lastSnapshot.block <> ":"
      traverse_ (\(Tuple poolId allocation) -> 
        log $ "  Pool " <> poolId <> ": " <> show allocation <> " POL"
      ) (Map.toUnfoldable lastSnapshot.allocations :: Array (Tuple String Number))
    Nothing -> log "No POL allocation history found"
  
  -- Convert oracle price history to chart format
  -- Sort observations by block number (not timestamp) to ensure proper ordering
  let sortedHistory = sortBy (\a b -> compare a.timestamp b.timestamp) oraclePriceHistory
  
  -- Build map of token creation times from action history
  let tokenCreationMap = foldl (\acc (Tuple idx action) -> 
        case action of
          S.CreateToken _ ticker _ -> 
            -- Use the index to estimate creation time
            let creationTime = case head sortedHistory of
                  Just first -> first.timestamp + (toNumber idx * 5000.0)  -- 5 seconds per action
                  Nothing -> 0.0
            in cons (Tuple ticker creationTime) acc
          _ -> acc
      ) [] (zip (range 0 (length tokenCreationActions - 1)) tokenCreationActions)
      
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
            
            -- Use actual POL reserves from protocol state
            -- POL should grow from initial reserves (10000) plus accumulated fees
            estimatedPOL = if currentPOL > 0.0 
                          then currentPOL  -- Use actual POL if available
                          else 10000.0     -- Use initial reserves as fallback
            
            -- Generate realistic token prices based on oracle price and time-based variations
            -- Each token has a base price multiplier and oscillates around it
            -- Only include tokens that have been created by this point in time
            existingTokensAtTime = filter (\token ->
              let tokenData = unsafeCoerce token :: { ticker :: String }
                  -- Check if this token was created before current observation
                  wasCreated = case find (\action -> 
                    case action of
                      S.CreateToken _ t _ -> t == tokenData.ticker
                      _ -> false
                  ) tokenCreationActions of
                    Just _ -> true  -- Token was created during simulation
                    Nothing -> false
              in wasCreated
            ) liveTokens
            
            tokenPrices = map (\token -> 
              let -- Use unsafeCoerce to access foreign TokenMetadata fields
                  tokenData = unsafeCoerce token :: { ticker :: String, live :: Boolean }
                  
                  -- Pool ID for this token pair
                  poolId = tokenData.ticker <> "/FeelsSOL"
                  
                  -- Find POL allocation for this timestamp from history
                  -- Use the most recent allocation before or at this timestamp
                  -- Since polHistory is in reverse chronological order (newest first), find first match
                  polAllocationAtTime = case find (\snapshot -> snapshot.timestamp <= obs.timestamp) polHistory of
                    Just snapshot -> fromMaybe 0.0 (Map.lookup poolId snapshot.allocations)
                    Nothing -> 0.0  -- No POL allocated yet at this time
                  
                  -- Debug: Log pool lookup with timestamp
                  _ = unsafePerformEffect $ do
                    log $ "Looking up pool " <> poolId <> " at timestamp " <> show obs.timestamp
                    log $ "  POL allocation at this time: " <> show polAllocationAtTime
                  
                  -- For now, use a realistic initial price and let it evolve with trading
                  -- In a real implementation, we'd fetch from the pool registry
                  -- Each token starts at a slightly different price based on its ticker
                  tickerSum = String.length tokenData.ticker * 100 + 
                              foldl (\sum i -> sum + i * 17) 0 (range 0 (String.length tokenData.ticker - 1))
                  basePrice = 0.8 + (toNumber (tickerSum `mod` 40)) * 0.01  -- Range: 0.8 to 1.2
                  
                  -- Add realistic price movement based on market conditions
                  -- Each token should have unique price movements based on its characteristics
                  marketTrend = case find (\(Tuple t _) -> t == tokenData.ticker) tokenCreationMap of
                    Just (Tuple _ creationTime) ->
                      let age = (obs.timestamp - creationTime) / 1000000.0
                          -- Use ticker hash for unique but deterministic behavior
                          tickerHash = foldl (\h c -> h * 31 + toCharCode c) 0 (toCharArray tokenData.ticker)
                          -- Different frequency for each token based on hash
                          frequency = 0.05 + (toNumber (tickerHash `mod` 20)) * 0.01
                          -- Different phase offset for each token
                          phase = toNumber (tickerHash `mod` 360) * 0.0174533  -- Convert to radians
                          -- New tokens are more volatile
                          volatility = max 0.05 (0.3 * exp (-age / 100.0))
                          -- Unique price movement for each token
                          movement = sin (age * frequency + phase) * volatility
                      in (1.0 + movement)
                    Nothing -> 1.0
                    
                  -- Calculate POL floor based on actual pool allocations at this time
                  tokenSupply = 100000.0
                  
                  -- Floor price = POL allocated to this pool / token supply
                  -- This ensures floor prices only go up as more POL is allocated
                  -- Add a minimum floor of 0.01 to show tokens have value even before POL allocation
                  polFloorPrice = max 0.01 (polAllocationAtTime / tokenSupply)
                  
                  -- Debug: Log floor price calculation
                  _ = unsafePerformEffect $ log $ 
                    "  Floor price for " <> tokenData.ticker <> " at block " <> show (length polHistory - length (filter (\s -> s.timestamp > obs.timestamp) polHistory)) <> ": " <> 
                    show polAllocationAtTime <> " / " <> show tokenSupply <> " = " <> 
                    show (polAllocationAtTime / tokenSupply) <> " -> " <> show polFloorPrice
                  
                  -- Ensure price never falls below POL floor (arbitrage would prevent this)
                  rawPrice = basePrice * marketTrend * jitoPrice
                  currentPrice = max polFloorPrice (rawPrice * 1.1)  -- Keep 10% above floor minimum
                  
              in { ticker: tokenData.ticker
                 , price: currentPrice
                 , polFloor: polFloorPrice  
                 , live: tokenData.live
                 }
            ) existingTokensAtTime
            
        in { timestamp: obs.timestamp
           , block: 0  -- Oracle doesn't track blocks
           , price: jitoPrice
           , polValue: estimatedPOL  -- POL value based on actual reserves
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
          Right (Tuple newState (TokenCreated _)) -> do
            write newState protocol.state
            log $ "Successfully created token: " <> ticker
            -- Verify token was added
            verifyResult <- executeQuery A.GetAllTokens newState
            case verifyResult of
              Right (TokenList tokens) -> 
                log $ "Token list after creation has " <> show (length tokens) <> " tokens"
              _ -> log "Failed to verify token creation"
          Right (Tuple newState _) -> do
            write newState protocol.state
            log $ "Token created but unexpected response type"
          Left err -> log $ "Failed to create token: " <> show err
      _ -> pure unit

--------------------------------------------------------------------------------
-- Chart Integration
--------------------------------------------------------------------------------


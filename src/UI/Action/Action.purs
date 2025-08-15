-- UI Action Handling for the Feels Protocol application
-- Contains all action handling logic for user interactions
module UI.Action.Action
  ( handleAction
  , validateTokenInput
  ) where

import Prelude
import Control.Monad (unless, when)
import Data.Array ((:), find, null, length)
import Data.Traversable (traverse_)
import Data.String.Common (trim)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref (read, write)
import Halogen as H

-- Import state and action types
import UI.State (UIState, Action(..))
import UI.Integration (initializeRemoteActions, refreshProtocolData, processSimulationResults)

-- Import API and data types
import UI.ProtocolState as A
import UI.ProtocolState (initState, ProtocolCommand, IndexerQuery(..))
import UI.ProtocolState as PS
import Protocol.Common (QueryResult(..), CommandResult(..), TokenMetadata)
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import Protocol.Token (TokenType(..))
import Protocol.Position (spotDuration, monthlyDuration, Leverage(..))
import FFI (setTimeout, checkAndInitializeChart, setChartData)
import Simulation.Engine (initSimulationWithPoolRegistry, executeSimulation, calculateResults, runProtocolSimulation)
import Protocol.Metric (getPOLMetrics, getProtocolTotalFeesCollected)
import UI.PoolRegistry (getAllPools)

--------------------------------------------------------------------------------
-- Main Action Handler
--------------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM UIState Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize protocol
    protocol <- H.liftEffect initState
    
    -- Subscribe to protocol state changes (disabled - subscribe function not implemented)
    -- _ <- H.liftEffect $ subscribe protocol \_ -> do
    --   log "Protocol state changed, refreshing UI data"
    
    -- Store protocol runtime
    H.modify_ _ { api = Just protocol, loading = false }
    
    -- Initialize remote actions for WebSocket control
    H.liftEffect $ initializeRemoteActions
    
    -- Initial data fetch
    handleAction RefreshData
  
  RefreshData -> do
    state <- H.get
    case state.api of
      Nothing -> pure unit
      Just protocol -> do
        H.liftEffect $ refreshProtocolData protocol state.currentUser
        
        -- Update UI state with refreshed data
        protocolState <- H.liftEffect $ read protocol.state
        posResult <- H.liftEffect $ executeQuery (A.GetUserPositions state.currentUser) protocolState
        case posResult of
          Right (PositionList positions) -> 
            H.modify_ _ { userPositions = positions }
          _ -> pure unit
        
        -- Get lender offers
        offersResult <- H.liftEffect $ executeQuery A.GetLenderOffers protocolState
        case offersResult of
          Right (LenderOfferList offers) -> do
            H.liftEffect $ log $ "RefreshData: Found " <> show (length offers) <> " lender offers"
            H.modify_ _ { lenderOffers = offers }
          Left err -> 
            H.liftEffect $ log $ "RefreshData: Failed to get lender offers: " <> show err
          _ -> pure unit
        
        -- Get protocol stats
        statsResult <- H.liftEffect $ executeQuery A.GetSystemStats protocolState
        case statsResult of
          Right (SystemStatsResult stats) -> do
            H.liftEffect $ log $ "RefreshData: Updated protocol stats - TVL: " <> show stats.totalValueLocked <> ", Users: " <> show stats.totalUsers
            H.modify_ _ { protocolStats = Just stats }
          Left err ->
            H.liftEffect $ log $ "RefreshData: Failed to get protocol stats: " <> show err
          _ -> pure unit
        
        -- Get wallet balances
        jitoBalanceResult <- H.liftEffect $ executeQuery (A.GetUserBalance state.currentUser JitoSOL) protocolState
        case jitoBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { jitoBalance = balance }
          _ -> pure unit
        
        feelsBalanceResult <- H.liftEffect $ executeQuery (A.GetUserBalance state.currentUser FeelsSOL) protocolState
        case feelsBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { feelsBalance = balance }
          _ -> pure unit
        
        -- Clear any errors
        H.modify_ _ { error = Nothing }
  
  RenderChart -> do
    H.liftEffect $ log "Rendering price chart..."
    currentState <- H.get
    H.liftEffect $ log $ "Price history length: " <> show (length currentState.priceHistory)
    -- Add a delay before calling the chart initialization
    H.liftEffect $ log "UI: About to schedule checkAndInitializeChart"
    H.liftEffect $ void $ setTimeout checkAndInitializeChart 250
    H.liftEffect $ log "UI: Scheduled checkAndInitializeChart for 250ms delay"

  -- Position Management Actions
  UpdateInputAmount amount -> do
    H.modify_ _ { inputAmount = amount }
  
  SelectFromAsset asset -> do
    H.modify_ _ { selectedFromAsset = asset }

  SelectToAsset asset -> do
    H.modify_ _ { selectedToAsset = asset }
  
  SelectTargetToken token -> do
    H.modify_ _ { selectedTargetToken = token }
  
  SetTermType termType -> do
    H.modify_ _ { selectedTermType = termType }
  
  SetLeverageType leverageType -> do
    H.modify_ _ { selectedLeverage = leverageType }

  ExecuteExchange -> do
    handleExecuteExchange

  -- Token Creation Actions
  UpdateTokenTicker ticker -> do
    state <- H.get
    let errors = validateTokenInput ticker state.tokenName state.userTokens
    -- Debug logging
    H.liftEffect $ do
      log $ "Ticker validation: " <> ticker <> " -> errors: " <> show (length errors)
      traverse_ (log <<< ("  - " <> _)) errors
    H.modify_ _ { tokenTicker = ticker, tokenValidationErrors = errors }
  
  UpdateTokenName name -> do
    state <- H.get
    let errors = validateTokenInput state.tokenTicker name state.userTokens  
    -- Debug logging
    H.liftEffect $ do
      log $ "Name validation: " <> name <> " -> errors: " <> show (length errors)
      traverse_ (log <<< ("  - " <> _)) errors
    H.modify_ _ { tokenName = name, tokenValidationErrors = errors }

  CreateTokenUI -> do
    handleCreateToken


  -- Simulation Actions
  UpdateSimulationConfig updater -> do
    H.modify_ \s -> s { simulationConfig = updater s.simulationConfig }

  RunSimulation -> do
    handleRunSimulation

  -- Launch System Actions
  SelectLaunch launchId -> do
    H.modify_ $ _ { selectedLaunchId = Just launchId }

  UpdateLaunchBidAmount amount -> do
    H.modify_ $ _ { launchBidAmount = amount }

  UpdateLaunchPriorityFee percent -> do
    H.modify_ $ _ { launchPriorityFeePercent = percent }

  SubmitLaunchBid -> do
    H.liftEffect $ log "Launch bid submission not yet implemented"

  RefreshLaunches -> do
    H.liftEffect $ log "Launch refresh not yet implemented"

  ProcessLaunchBatch launchId -> do
    H.liftEffect $ log $ "Processing batch for launch: " <> launchId

--------------------------------------------------------------------------------
-- Token Creation Handlers
--------------------------------------------------------------------------------

handleCreateToken :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleCreateToken = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      let ticker = trim state.tokenTicker
          name = trim state.tokenName
          -- Run validation
          errors = validateTokenInput ticker name state.userTokens
          
      -- Update validation errors in state
      H.modify_ _ { tokenValidationErrors = errors }
      
      -- Check if we can proceed
      if null errors && ticker /= "" && name /= ""
        then do
          -- Execute create token command
          protocolState <- H.liftEffect $ read protocol.state
          result <- H.liftEffect $ executeCommand  
            (PS.CreateToken state.currentUser ticker name) protocolState
          
          case result of
            Right (Tuple newState (TokenCreated token)) -> do
              H.liftEffect $ write newState protocol.state
              H.liftEffect $ log $ "Token created successfully"
              -- Reset form and clear validation errors
              H.modify_ \s -> s 
                { tokenTicker = ""
                , tokenName = ""
                , tokenValidationErrors = []
                , error = Nothing
                }
              -- Refresh data to get updated token list
              handleAction RefreshData
            Right _ -> 
              H.modify_ _ { error = Just "Unexpected result from token creation" }
            Left err -> 
              H.modify_ _ { error = Just $ show err }
        else do
          -- Add basic validation errors if fields are empty
          let emptyFieldErrors = 
                (if ticker == "" then ["Token ticker is required"] else []) <>
                (if name == "" then ["Token name is required"] else [])
          H.modify_ \s -> s { tokenValidationErrors = s.tokenValidationErrors <> emptyFieldErrors }

--------------------------------------------------------------------------------
-- Universal Exchange Handler
--------------------------------------------------------------------------------

handleExecuteExchange :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleExecuteExchange = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      -- Get protocol state
      protocolState <- H.liftEffect $ read protocol.state
      
      -- Route based on from/to selection
      case { from: state.selectedFromAsset, to: state.selectedToAsset } of
        -- JitoSOL -> Position
        { from: "jitosol", to: "position-spot" } -> do
          H.liftEffect $ log $ "Exchange: JitoSOL -> Spot Position"
          -- TODO: Implement atomic JitoSOL -> Position conversion
          H.modify_ _ { error = Just "JitoSOL to Position exchange coming soon!" }
        
        -- JitoSOL -> Term Position
        { from: "jitosol", to: "position-term" } -> do
          H.liftEffect $ log $ "Exchange: JitoSOL -> Term Position"
          -- TODO: Implement atomic JitoSOL -> Term Position conversion
          H.modify_ _ { error = Just "JitoSOL to Term Position exchange coming soon!" }
        
        -- TODO: Implement other routes
        _ -> do
          H.modify_ _ { error = Just "This exchange route is not yet implemented" }

--------------------------------------------------------------------------------
-- Simulation Handlers
--------------------------------------------------------------------------------

handleRunSimulation :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleRunSimulation = do
  -- Check if simulation is already running
  currentState <- H.get
  when currentState.simulationRunning do
    H.liftEffect $ log "Simulation already running, skipping..."
    pure unit
  
  unless currentState.simulationRunning do
    H.liftEffect $ log "Starting simulation..."
    H.modify_ _ { simulationRunning = true, error = Nothing }
    
    state <- H.get
    case state.api of
      Nothing -> H.modify_ _ { error = Just "Protocol not initialized" }
      Just protocol -> do
        -- Extract the protocol's lending book instead of creating a new one
        protocolState <- H.liftEffect $ read protocol.state
      
        -- Initialize simulation with the protocol's actual pool registry and oracle
        simState <- H.liftEffect $ initSimulationWithPoolRegistry 
          state.simulationConfig 
          protocolState.poolRegistry
          protocolState.oracle
      
        -- Seed initial JitoSOL/FeelsSOL liquidity at correct price
        H.liftEffect $ log "Seeding initial JitoSOL/FeelsSOL liquidity..."
        _ <- H.liftEffect $ do
          log "Creating first liquidity position..."
          -- Create initial liquidity offers at 1.22 FeelsSOL per JitoSOL
          -- For JitoSOL -> FeelsSOL: If lending 500 JitoSOL, want 610 FeelsSOL as collateral (500 * 1.22)
          state1 <- read protocol.state
          result1 <- executeCommand (PS.CreatePosition "liquidity-bot" JitoSOL 500.0 FeelsSOL 610.0 spotDuration Senior false Nothing) state1
          _ <- case result1 of
            Right (Tuple newState1 _) -> do
              write newState1 protocol.state
              log "First liquidity position created"
            Left err -> log $ "Failed to create first position: " <> show err
          -- For FeelsSOL -> JitoSOL: If lending 500 FeelsSOL, want 410 JitoSOL as collateral (500 / 1.22)
          log "Creating second liquidity position..."
          state2 <- read protocol.state
          result2 <- executeCommand (PS.CreatePosition "liquidity-bot" FeelsSOL 500.0 JitoSOL 410.0 spotDuration Senior false Nothing) state2
          _ <- case result2 of
            Right (Tuple newState2 _) -> do
              write newState2 protocol.state
              log "Second liquidity position created"
            Left err -> log $ "Failed to create second position: " <> show err
          log "Finished seeding liquidity"
          pure unit
      
        -- Execute simulation through the actual protocol with proper event loop
        H.liftEffect $ log "Starting simulation..."
        executionResult <- H.liftEffect $ runProtocolSimulation 
          protocol.state 
          state.simulationConfig 
          simState
      
        let finalState = executionResult.finalSimState
            finalProtocolState = executionResult.finalProtocolState
      
        -- Calculate results using the final simulation state
        results <- H.liftEffect $ calculateResults state.simulationConfig finalState
      
        -- Get actual protocol metrics including POL reserves
        polMetrics <- H.liftEffect $ getPOLMetrics finalProtocolState.polState
        pools <- H.liftEffect $ getAllPools finalProtocolState.poolRegistry
        let poolStates = map snd pools
        totalFees <- H.liftEffect $ getProtocolTotalFeesCollected poolStates
      
        H.liftEffect $ log $ "Simulation completed with " <> show results.totalUsers <> " users"
        H.liftEffect $ log $ "POL reserves: " <> show polMetrics.totalValue
        H.liftEffect $ log $ "Total fees collected: " <> show totalFees
      
        -- Process simulation results with proper POL floor data
        priceHistory <- H.liftEffect $ processSimulationResults protocol finalState results
      
        -- Refresh data to show updated offers
        handleAction RefreshData
      
        -- Update state
        H.modify_ _ 
          { simulationRunning = false
          , simulationResults = Just results
          , priceHistory = priceHistory
          }
      
        -- Set chart data in the DOM
        H.liftEffect $ do
          log $ "Setting chart data with " <> show (length priceHistory) <> " points"
          setChartData priceHistory
      
        -- Force a render cycle before initializing chart
        H.liftEffect $ log "State updated, scheduling chart render..."
      
        -- Use a small delay to ensure Halogen completes the DOM update
        void $ H.fork do
          H.liftEffect $ void $ setTimeout (pure unit) 200
          handleAction RenderChart

--------------------------------------------------------------------------------
-- Token Validation
--------------------------------------------------------------------------------

-- Token validation function
validateTokenInput :: String -> String -> Array TokenMetadata -> Array String
validateTokenInput ticker name existingTokens =
  let trimmedTicker = trim ticker
      trimmedName = trim name
      
      -- Check ticker length
      tickerLengthErrors = 
        if trimmedTicker == "" then []
        else if String.length trimmedTicker < 3 then ["Ticker must be at least 3 characters"]
        else if String.length trimmedTicker > 10 then ["Ticker must be at most 10 characters"]
        else []
      
      -- Cannot check for duplicates on foreign TokenMetadata type
      duplicateTickerErrors = []
      
      -- Cannot check for duplicates on foreign TokenMetadata type
      duplicateNameErrors = []
          
      -- Reserved ticker checks
      reservedTickerErrors =
        if trimmedTicker == "" then []
        else if String.toUpper trimmedTicker == "SOL" || 
                String.toUpper trimmedTicker == "JITO" ||
                String.toUpper trimmedTicker == "FEELSSOL" ||
                String.toUpper trimmedTicker == "JITOSOL"
             then ["Ticker '" <> trimmedTicker <> "' is reserved"]
             else []
             
  in tickerLengthErrors <> duplicateTickerErrors <> duplicateNameErrors <> reservedTickerErrors
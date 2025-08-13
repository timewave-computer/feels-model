-- UI Action Handling for the Feels Protocol application
-- Contains all action handling logic for user interactions
module UI.Action
  ( handleAction
  , validateTokenInput
  ) where

import Prelude
import Data.Array ((:), find, null, length)
import Data.Traversable (traverse_)
import Data.String.Common (trim)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
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
import Protocol.Position (spotTerm, monthlyTerm)
import FFI (setTimeout, checkAndInitializeChart, setChartData)
import Simulation.Sim (initSimulationWithPoolRegistry, executeSimulation, calculateResults)

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
  
  SelectAsset asset -> do
    H.modify_ _ { selectedAsset = asset }

  SelectCollateralAsset asset -> do
    H.modify_ _ { collateralAsset = asset }
  
  SetTermType termType -> do
    H.modify_ _ { selectedTermType = termType }


  CreatePosition -> do
    handleCreatePosition

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

  -- FeelsSOL Actions
  ToggleFeelsSOL -> do
    H.modify_ \s -> s { showFeelsSOL = not s.showFeelsSOL }

  UpdateJitoSOLAmount amount -> do
    H.modify_ _ { jitoSOLAmount = amount }

  UpdateFeelsSOLAmount amount -> do
    H.modify_ _ { feelsSOLAmount = amount }

  EnterFeelsSOL -> do
    handleEnterFeelsSOL

  ExitFeelsSOL -> do
    handleExitFeelsSOL

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
-- Position Management Handlers
--------------------------------------------------------------------------------

handleCreatePosition :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleCreatePosition = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      -- Get current block from protocol state for term creation
      protocolState <- H.liftEffect $ read protocol.state
      let currentBlock = protocolState.currentBlock
          term = case state.selectedTermType of
            "monthly" -> monthlyTerm currentBlock
            _ -> spotTerm
          
          collateralAmount = state.inputAmount * 1.5  -- Simplified
      
      -- Execute create position command
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeCommand
        (PS.CreatePosition 
          state.currentUser
          state.selectedAsset
          state.inputAmount
          state.collateralAsset
          collateralAmount
          term
          false  -- rollover: default to false for now
          Nothing) protocolState
      
      case result of
        Right (Tuple newState (PositionCreated pos)) -> do
          H.liftEffect $ write newState protocol.state
          H.liftEffect $ log $ "Position created successfully"
          -- Refresh all data
          handleAction RefreshData
        Right _ ->
          H.modify_ _ { error = Just "Unexpected result from position creation" }
        Left err ->
          H.modify_ _ { error = Just $ show err }

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
          
      if null state.tokenValidationErrors && ticker /= "" && name /= ""
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
        else 
          H.modify_ _ { error = Just "Please fix validation errors before creating token" }

--------------------------------------------------------------------------------
-- FeelsSOL Handlers
--------------------------------------------------------------------------------

handleEnterFeelsSOL :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleEnterFeelsSOL = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeCommand
        (PS.EnterFeelsSOL state.currentUser state.jitoSOLAmount) protocolState
      
      case result of
        Right (Tuple newState (FeelsSOLMinted info)) -> do
          H.liftEffect $ write newState protocol.state
          H.liftEffect $ log $ "Entered FeelsSOL: " <> show info.feelsSOLMinted <> " FeelsSOL minted"
          handleAction RefreshData
        Right _ ->
          H.modify_ _ { error = Just "Unexpected result from FeelsSOL entry" }
        Left err ->
          H.modify_ _ { error = Just $ show err }

handleExitFeelsSOL :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleExitFeelsSOL = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      -- For exit, we use the input amount as FeelsSOL amount to convert
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeCommand
        (PS.ExitFeelsSOL state.currentUser state.feelsSOLAmount) protocolState
      
      case result of
        Right (Tuple newState (FeelsSOLBurned info)) -> do
          H.liftEffect $ write newState protocol.state
          H.liftEffect $ log $ "Exited FeelsSOL: " <> show info.jitoSOLReceived <> " JitoSOL received"
          handleAction RefreshData
        Right _ ->
          H.modify_ _ { error = Just "Unexpected result from FeelsSOL exit" }
        Left err ->
          H.modify_ _ { error = Just $ show err }

--------------------------------------------------------------------------------
-- Simulation Handlers
--------------------------------------------------------------------------------

handleRunSimulation :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleRunSimulation = do
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
        -- Create initial liquidity offers at 1.22 FeelsSOL per JitoSOL
        -- For JitoSOL -> FeelsSOL: If lending 500 JitoSOL, want 610 FeelsSOL as collateral (500 * 1.22)
        state1 <- read protocol.state
        result1 <- executeCommand (PS.CreatePosition "liquidity-bot" JitoSOL 500.0 FeelsSOL 610.0 spotTerm false Nothing) state1
        _ <- case result1 of
          Right (Tuple newState1 _) -> write newState1 protocol.state
          _ -> pure unit
        -- For FeelsSOL -> JitoSOL: If lending 500 FeelsSOL, want 410 JitoSOL as collateral (500 / 1.22)
        state2 <- read protocol.state
        result2 <- executeCommand (PS.CreatePosition "liquidity-bot" FeelsSOL 500.0 JitoSOL 410.0 spotTerm false Nothing) state2
        _ <- case result2 of
          Right (Tuple newState2 _) -> write newState2 protocol.state
          _ -> pure unit
        pure unit
      
      -- Run simulation
      finalState <- H.liftEffect $ executeSimulation state.simulationConfig simState
      
      -- Calculate results
      results <- H.liftEffect $ calculateResults state.simulationConfig finalState
      
      H.liftEffect $ log $ "Simulation completed with " <> show results.totalUsers <> " users"
      
      -- Process simulation results including token creation
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
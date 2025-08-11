-- UI Action Handling for the Feels Protocol application
-- Contains all action handling logic for user interactions
module UI.Actions
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
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref (read)
import Halogen as H

-- Import state and action types
import UI.State (UIState, Action(..))
import UI.Integration (initializeRemoteActions, refreshProtocolData, processSimulationResults)

-- Import API and data types
import State.State as A
import State.State (AppResult(..), initState, executeCommand, executeQuery)
import Token (TokenType(..), TokenMetadata)
import Position (spotTerm, hourlyTerm, dailyTerm, weeklyTerm)
import FFI (setTimeout, checkAndInitializeChart)
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
        posResult <- H.liftEffect $ executeQuery protocol (A.GetUserPositions state.currentUser)
        case posResult of
          Right (PositionList positions) -> 
            H.modify_ _ { userPositions = positions }
          _ -> pure unit
        
        -- Get lender offers
        offersResult <- H.liftEffect $ executeQuery protocol A.GetLenderOffers
        case offersResult of
          Right (LenderOfferList offers) -> do
            H.liftEffect $ log $ "RefreshData: Found " <> show (length offers) <> " lender offers"
            H.modify_ _ { lenderOffers = offers }
          Left err -> 
            H.liftEffect $ log $ "RefreshData: Failed to get lender offers: " <> show err
          _ -> pure unit
        
        -- Get protocol stats
        statsResult <- H.liftEffect $ executeQuery protocol A.GetSystemStats
        case statsResult of
          Right (SystemStatsResult stats) -> do
            H.liftEffect $ log $ "RefreshData: Updated protocol stats - TVL: " <> show stats.totalValueLocked <> ", Users: " <> show stats.totalUsers
            H.modify_ _ { protocolStats = Just stats }
          Left err ->
            H.liftEffect $ log $ "RefreshData: Failed to get protocol stats: " <> show err
          _ -> pure unit
        
        -- Get wallet balances
        jitoBalanceResult <- H.liftEffect $ executeQuery protocol (A.GetUserBalance state.currentUser JitoSOL)
        case jitoBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { jitoSOLBalance = balance }
          _ -> pure unit
        
        feelsBalanceResult <- H.liftEffect $ executeQuery protocol (A.GetUserBalance state.currentUser FeelsSOL)
        case feelsBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { feelsSOLBalance = balance }
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

  -- Gateway Actions
  ToggleGateway -> do
    H.modify_ \s -> s { showGateway = not s.showGateway }

  UpdateJitoSOLAmount amount -> do
    H.modify_ _ { jitoSOLAmount = amount }

  UpdateFeelsSOLAmount amount -> do
    H.modify_ _ { feelsSOLAmount = amount }

  EnterGateway -> do
    handleEnterGateway

  ExitGateway -> do
    handleExitGateway

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
            "hourly" -> hourlyTerm currentBlock
            "daily" -> dailyTerm currentBlock
            "weekly" -> weeklyTerm currentBlock
            _ -> spotTerm
          
          collateralAmount = state.inputAmount * 1.5  -- Simplified
      
      -- Execute create position command
      result <- H.liftEffect $ executeCommand protocol
        (A.CreatePosition 
          state.currentUser
          state.selectedAsset
          state.inputAmount
          state.collateralAsset
          collateralAmount
          term
          Nothing)
      
      case result of
        Right (PositionCreated pos) -> do
          H.liftEffect $ log $ "Position created: #" <> show pos.id
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
          result <- H.liftEffect $ executeCommand protocol 
            (A.CreateToken state.currentUser ticker name)
          
          case result of
            Right (TokenCreated token) -> do
              H.liftEffect $ log $ "Token created: " <> token.ticker
              -- Reset form and clear validation errors
              H.modify_ \s -> s 
                { userTokens = token : s.userTokens
                , tokenTicker = ""
                , tokenName = ""
                , tokenValidationErrors = []
                , error = Nothing
                }
              -- Refresh all data
              handleAction RefreshData
            Right _ -> 
              H.modify_ _ { error = Just "Unexpected result from token creation" }
            Left err -> 
              H.modify_ _ { error = Just $ show err }
        else 
          H.modify_ _ { error = Just "Please fix validation errors before creating token" }

--------------------------------------------------------------------------------
-- Gateway Handlers
--------------------------------------------------------------------------------

handleEnterGateway :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleEnterGateway = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      result <- H.liftEffect $ executeCommand protocol
        (A.EnterGateway state.currentUser state.jitoSOLAmount)
      
      case result of
        Right (GatewayEntered info) -> do
          H.liftEffect $ log $ "Entered gateway: " <> show info.feelsSOLMinted <> " FeelsSOL minted"
          handleAction RefreshData
        Right _ ->
          H.modify_ _ { error = Just "Unexpected result from gateway entry" }
        Left err ->
          H.modify_ _ { error = Just $ show err }

handleExitGateway :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleExitGateway = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      -- For exit, we use the input amount as FeelsSOL amount to convert
      result <- H.liftEffect $ executeCommand protocol
        (A.ExitGateway state.currentUser state.feelsSOLAmount)
      
      case result of
        Right (GatewayExited info) -> do
          H.liftEffect $ log $ "Exited gateway: " <> show info.jitoSOLReceived <> " JitoSOL received"
          handleAction RefreshData
        Right _ ->
          H.modify_ _ { error = Just "Unexpected result from gateway exit" }
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
        _ <- executeCommand protocol (A.CreatePosition "liquidity-bot" JitoSOL 500.0 FeelsSOL 610.0 spotTerm Nothing)
        -- For FeelsSOL -> JitoSOL: If lending 500 FeelsSOL, want 410 JitoSOL as collateral (500 / 1.22)
        _ <- executeCommand protocol (A.CreatePosition "liquidity-bot" FeelsSOL 500.0 JitoSOL 410.0 spotTerm Nothing)
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
      
      -- Check for duplicate ticker
      duplicateTickerErrors = 
        if trimmedTicker == "" then []
        else case find (\t -> t.ticker == trimmedTicker) existingTokens of
          Just _ -> ["Token with ticker '" <> trimmedTicker <> "' already exists"]
          Nothing -> []
      
      -- Check for duplicate name  
      duplicateNameErrors =
        if trimmedName == "" then []
        else case find (\t -> t.name == trimmedName) existingTokens of
          Just _ -> ["Token with name '" <> trimmedName <> "' already exists"]
          Nothing -> []
          
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
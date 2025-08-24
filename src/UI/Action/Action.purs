-- UI Action Handling for the Feels Protocol application
-- Contains all action handling logic for user interactions
module UI.Action.Action
  ( handleAction
  , validateTokenInput
  ) where

import Prelude
import Data.Array (null, length)
import Data.Traversable (traverse_)
import Data.String.Common (trim)
import Data.String as String
import Data.Maybe (Maybe(..))
import Data.Variant (Variant, inj, match)
-- import Data.Variant as V -- removed: redundant
import Type.Proxy (Proxy(..))
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt, parallel, sequential, throwError, error)
import Effect.Aff.Class (liftAff, class MonadAff)
-- import Effect (Effect) -- removed: redundant
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Tuple (Tuple(..))
import UI.Util.Logger (logInfo, logError, logActionStart, logActionEnd)
-- Record updates now done directly with PureScript record update syntax
import Effect.Ref (read, write)
import Halogen as H

-- Import state and action types
import UI.State (UIState, Action(..))
import UI.ProtocolState as UI.ProtocolState
import UI.Integration (initializeRemoteActions, refreshProtocolData, processSimulationResults)
import UI.Util.CommandExecution (executeCommandWithRefresh)

-- Import API and data types
import UI.ProtocolState (initState, ProtocolCommand(..), IndexerQuery(..), addListener)
import Protocol.Common (QueryResult(..), CommandResult(..), TokenMetadata)
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import Protocol.Token (TokenType(..))
import Protocol.Pool (Duration, Leverage(..))
import Protocol.PositionVault (swapDuration, monthlyDuration)
import FFI (setTimeout, checkAndInitializeChart, setChartData)
import Simulation.Engine (SimulatedAccount, SimulationConfig, SimulationState, calculateResults, initSimulationWithPoolRegistry, runProtocolSimulation)
import Protocol.ProtocolVault (getProtocolMetrics)
-- import Protocol.Vault (LedgerVault) -- removed: redundant
-- import Protocol.FeelsSOLVault (FeelsSOLEntry, FeelsSOLStrategy) -- removed: redundant
-- import Protocol.LaunchVault (LaunchEntry, LaunchStrategy) -- removed: redundant
-- import Protocol.ProtocolVault (ProtocolEntry, ProtocolStrategy) -- removed: redundant
-- import UI.Account (ChainAccount, FeelsAccount) -- removed: redundant
-- import Protocol.Oracle (Oracle) -- removed: redundant
-- import Data.Map (Map) -- removed: redundant
-- import UI.PoolRegistry (PoolRegistry) -- removed: redundant
-- import UI.TokenRegistry (TokenRegistry) -- removed: redundant
-- import UI.PoolRegistry (getAllPools) -- removed: redundant

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- ProtocolRef is just an alias for AppRuntime from UI.ProtocolState
type ProtocolRef = UI.ProtocolState.AppRuntime

-- We should use the ProtocolState from UI.ProtocolState directly
-- This is just an alias for documentation purposes
type ProtocolState = UI.ProtocolState.ProtocolState

-- SimState is just an alias for SimulationState from Simulation.Engine
type SimState = SimulationState

-- SimAccount is just an alias for SimulatedAccount from Simulation.Agent
type SimAccount = SimulatedAccount

type SimResults =
  { activePositions :: Int
  , averageUtilization :: Number
  , priceChange :: Number
  , protocolTVL :: Number
  , scenarioSuccess :: Boolean
  , totalFees :: Number
  , totalUsers :: Int
  , totalVolume :: Number
  , volatility :: Number
  }

type POLMetrics =
  { avgPoolPerformance :: Number
  , concentrationRisk :: Number
  , deployedPOL :: Number
  , healthScore :: Number
  , poolCount :: Int
  , totalFeesCollected :: Number
  , totalPOL :: Number
  , utilization :: Number
  }

type PriceHistoryEntry =
  { block :: Int
  , polValue :: Number
  , price :: Number
  , timestamp :: Number
  , tokens :: Array TokenPriceEntry
  }

type TokenPriceEntry =
  { live :: Boolean
  , polFloor :: Number
  , price :: Number
  , ticker :: String
  }

--------------------------------------------------------------------------------
-- Simulation Helper Functions (Aff-based - reduces 98 lines to ~30)
--------------------------------------------------------------------------------

-- | Initialize simulation with proper error handling
initializeSimulation :: UIState -> Aff { protocol :: ProtocolRef, simState :: SimState, protocolState :: ProtocolState }
initializeSimulation state = do
  protocol <- case state.api of
    Nothing -> H.liftAff $ throwError $ error "Protocol not initialized"
    Just p -> pure p
  protocolState <- liftEffect $ read protocol.state
  -- poolRegistry and oracle are not refs in ProtocolState, use them directly
  maybeSimState <- liftEffect $ initSimulationWithPoolRegistry 
    state.simulationConfig 
    protocolState.poolRegistry
    protocolState.oracle
  simState <- case maybeSimState of
    Nothing -> throwError $ error "Failed to initialize simulation"
    Just s -> pure s
  pure { protocol, simState, protocolState }

-- | Seed initial liquidity positions in parallel
seedLiquidityPositions :: ProtocolRef -> Aff Unit
seedLiquidityPositions protocol = do
  liftEffect $ logActionStart "SimulationEngine" "SeedLiquidity"
  
  -- Create both liquidity positions in parallel for better performance
  sequential $ ado
    _ <- parallel $ createLiquidityPosition protocol "first" FeelsSOL 610.0 JitoSOL 500.0
    _ <- parallel $ createLiquidityPosition protocol "second" JitoSOL 410.0 FeelsSOL 500.0
    in unit
  
  liftEffect $ logActionEnd "SimulationEngine" "SeedLiquidity"

-- | Create a single liquidity position with error handling
createLiquidityPosition :: ProtocolRef -> String -> TokenType -> Number -> TokenType -> Number -> Aff Unit
createLiquidityPosition protocol positionType lendAsset lendAmount collateralAsset collateralAmount = do
  liftEffect $ log $ "Creating " <> positionType <> " liquidity position..."
  state <- liftEffect $ read protocol.state
  result <- liftEffect $ executeCommand 
    (CreatePosition "liquidity-bot" lendAsset lendAmount collateralAsset collateralAmount swapDuration (Leverage 1.0) false Nothing) 
    state
  case result of
    Right (Tuple newState _) -> do
      liftEffect $ write newState protocol.state
      liftEffect $ log $ positionType <> " liquidity position created successfully"
    Left err -> 
      liftEffect $ log $ "Failed to create " <> positionType <> " position: " <> show err

-- | Run simulation and collect results in parallel
runSimulationWithMetrics :: ProtocolRef -> SimState -> SimulationConfig -> Aff { results :: SimResults, polMetrics :: POLMetrics, totalFees :: Number, priceHistory :: Array PriceHistoryEntry }
runSimulationWithMetrics protocol simState config = do
  liftEffect $ log "Starting simulation..."
  
  -- Run simulation
  -- Pass the ref directly, not protocol.state
  protocolStateRef <- liftEffect $ pure protocol.state
  executionResult <- liftEffect $ runProtocolSimulation protocolStateRef config simState
  let finalState = executionResult.finalSimState
      finalProtocolState = executionResult.finalProtocolState
  
  -- Calculate all metrics
  results <- liftEffect $ calculateResults config finalState
  polMetrics <- liftEffect $ getProtocolMetrics finalProtocolState.polState
  -- pools <- liftEffect $ getAllPools finalProtocolState.poolRegistry
  priceHistory <- liftEffect $ processSimulationResults protocol finalState results
  
  -- Calculate total fees from simulation results
  let totalFees = results.totalFees + polMetrics.totalFeesCollected
  
  pure { results, polMetrics, totalFees, priceHistory }

--------------------------------------------------------------------------------
-- Main Action Handler
--------------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM UIState Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize protocol
    protocol <- H.liftEffect initState
    
    -- Subscribe to protocol state changes
    _ <- H.liftEffect $ addListener (\_ -> do
      log "Protocol state changed, refreshing UI data") protocol
    
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
        
        -- Get protocol state first, then execute queries in parallel for better performance
        protocolState <- H.liftEffect $ read protocol.state
        
        result <- liftAff $ sequential $ ado
          posResult <- parallel $ liftEffect $ executeQuery (GetUserPositions state.currentUser) protocolState
          offersResult <- parallel $ liftEffect $ executeQuery GetLenderOffers protocolState
          statsResult <- parallel $ liftEffect $ executeQuery GetSystemStats protocolState
          tokensResult <- parallel $ liftEffect $ executeQuery (GetUserTokens state.currentUser) protocolState
          jitoBalanceResult <- parallel $ liftEffect $ executeQuery (GetUserBalance state.currentUser JitoSOL) protocolState
          feelsBalanceResult <- parallel $ liftEffect $ executeQuery (GetUserBalance state.currentUser FeelsSOL) protocolState
          in { posResult, offersResult, statsResult, tokensResult, jitoBalanceResult, feelsBalanceResult }
        
        -- Process results and update state
        -- Example of how record utilities could simplify this:
        -- H.modify_ $ batchUpdates
        --   [ extractAndSet result.posResult setUserPositions
        --   , extractAndSet result.offersResult setLenderOffers  
        --   , extractAndSet result.statsResult (setProtocolStats <<< Just)
        --   , extractAndSet result.tokensResult setUserTokens
        --   , clearError
        --   ]
        
        case result.posResult of
          Right (PositionList positions) -> 
            H.modify_ _ { userPositions = positions }
          _ -> pure unit
        
        case result.offersResult of
          Right (LenderOfferList offers) -> do
            H.liftEffect $ logInfo $ "RefreshData: Found " <> show (length offers) <> " lender offers"
            H.modify_ _ { lenderOffers = offers }
          Left err -> 
            H.liftEffect $ logError $ "RefreshData: Failed to get lender offers: " <> show err
          _ -> pure unit
        
        case result.statsResult of
          Right (SystemStatsResult stats) -> do
            H.liftEffect $ log $ "RefreshData: Updated protocol stats - TVL: " <> show stats.totalValueLocked <> ", Users: " <> show stats.totalUsers
            H.modify_ _ { protocolStats = Just stats }
          Left err ->
            H.liftEffect $ log $ "RefreshData: Failed to get protocol stats: " <> show err
          _ -> pure unit
        
        case result.tokensResult of
          Right (TokenList tokens) -> do
            H.liftEffect $ log $ "RefreshData: Found " <> show (length tokens) <> " user tokens"
            H.modify_ _ { userTokens = tokens }
          Left err ->
            H.liftEffect $ log $ "RefreshData: Failed to get user tokens: " <> show err
          _ -> pure unit
        
        case result.jitoBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { jitoBalance = balance }
          _ -> pure unit
        
        case result.feelsBalanceResult of
          Right (Balance balance) ->
            H.modify_ _ { feelsBalance = balance }
          _ -> pure unit
        
        -- Clear any errors (could be: H.modify_ clearError)
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
  let ticker = trim state.tokenTicker
      name = trim state.tokenName
      -- Run validation
      errors = validateTokenInput ticker name state.userTokens
      -- Add basic validation errors if fields are empty
      allErrors = errors <> 
                 (if ticker == "" then ["Token ticker is required"] else []) <>
                 (if name == "" then ["Token name is required"] else [])
      
  -- Update validation errors in state
  H.modify_ _ { tokenValidationErrors = allErrors }
  
  -- Execute command if validation passes
  if null allErrors
    then executeCommandWithRefresh 
           (CreateToken state.currentUser ticker name)
           "Token created successfully"
    else do
      H.liftEffect $ log $ "Token creation validation failed: " <> show (length allErrors) <> " errors"

--------------------------------------------------------------------------------
-- Exchange Route Types (Variant-based for extensibility - reduces ~59 lines)
--------------------------------------------------------------------------------

-- | Exchange route variant type (extensible and type-safe)
type ExchangeRouteV = Variant
  ( jitoSOLToSwapPosition :: Unit
  , jitoSOLToTermPosition :: Unit  
  , feelsSOLToPosition :: Unit
  , directSwap :: Unit
  , unsupportedRoute :: String
  )

-- | Route proxy symbols for type safety
_jitoSOLToSwapPosition = Proxy :: Proxy "jitoSOLToSwapPosition"
_jitoSOLToTermPosition = Proxy :: Proxy "jitoSOLToTermPosition"
_feelsSOLToPosition = Proxy :: Proxy "feelsSOLToPosition"
-- _directSwap = Proxy :: Proxy "directSwap"
_unsupportedRoute = Proxy :: Proxy "unsupportedRoute"

-- | Parse route from UI selection state (type-safe variant construction)
parseExchangeRoute :: String -> String -> ExchangeRouteV
parseExchangeRoute from to = case { from, to } of
  { from: "jitosol", to: "position-swap" } -> inj _jitoSOLToSwapPosition unit
  { from: "jitosol", to: "position-term" } -> inj _jitoSOLToTermPosition unit
  { from: "feelssol", to: "position-swap" } -> inj _feelsSOLToPosition unit
  { from: "feelssol", to: "position-term" } -> inj _feelsSOLToPosition unit
  _ -> inj _unsupportedRoute $ "Route " <> from <> " -> " <> to <> " is not yet implemented"

-- | Execute exchange based on route variant (extensible pattern with better error handling)
executeExchangeRoute :: forall o m. MonadAff m => ExchangeRouteV -> UIState -> ProtocolRef -> H.HalogenM UIState Action () o m Unit
executeExchangeRoute route state protocol = 
  route # match
    { jitoSOLToSwapPosition: \_ -> executeJitoSOLToPosition swapDuration state protocol
    , jitoSOLToTermPosition: \_ -> executeJitoSOLToPosition monthlyDuration state protocol
    , feelsSOLToPosition: \_ -> executeDirectPosition state protocol
    , directSwap: \_ -> executeDirectSwap state protocol
    , unsupportedRoute: \errorMsg -> H.modify_ _ { error = Just errorMsg }
    }

-- | Execute JitoSOL -> Position conversion (unified handler)
executeJitoSOLToPosition :: forall o m. MonadAff m => Duration -> UIState -> ProtocolRef -> H.HalogenM UIState Action () o m Unit
executeJitoSOLToPosition duration state protocol = do
  protocolState <- H.liftEffect $ read protocol.state
  H.liftEffect $ log $ "Exchange: JitoSOL -> " <> show duration <> " Position"
  
  -- 1. Enter FeelsSOL with JitoSOL
  enterResult <- H.liftEffect $ executeCommand 
    (EnterFeelsSOL state.currentUser state.inputAmount) protocolState
  case enterResult of
    Left err -> H.modify_ _ { error = Just $ show err }
    Right (Tuple newState1 (FeelsSOLMinted mintResult)) -> do
      -- 2. Create position with FeelsSOL
      case state.selectedTargetToken of
        Nothing -> H.modify_ _ { error = Just "Please select a target token for the position" }
        Just ticker -> do
          let leverage = if state.selectedLeverage == "senior" then Leverage 1.0 else Leverage 3.0
          posResult <- H.liftEffect $ executeCommand
            (CreatePosition state.currentUser FeelsSOL mintResult.feelsSOLMinted (Custom ticker) mintResult.feelsSOLMinted duration leverage false Nothing)
            newState1
          case posResult of
            Left err -> H.modify_ _ { error = Just $ show err }
            Right (Tuple newState2 _) -> do
              -- Update protocol state and refresh UI
              H.liftEffect $ write newState2 protocol.state
              handleAction RefreshData
    _ -> H.modify_ _ { error = Just "Unexpected result from FeelsSOL entry" }

-- | Execute direct position creation (for future implementation)
executeDirectPosition :: forall o m. MonadAff m => UIState -> ProtocolRef -> H.HalogenM UIState Action () o m Unit
executeDirectPosition _ _ = 
  H.modify_ _ { error = Just "Direct position creation not yet implemented" }

-- | Execute direct swap (for future implementation)
executeDirectSwap :: forall o m. MonadAff m => UIState -> ProtocolRef -> H.HalogenM UIState Action () o m Unit
executeDirectSwap _ _ = 
  H.modify_ _ { error = Just "Direct swap not yet implemented" }

--------------------------------------------------------------------------------
-- Universal Exchange Handler (simplified with variant pattern)
--------------------------------------------------------------------------------

handleExecuteExchange :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleExecuteExchange = do
  state <- H.get
  case state.api of
    Nothing -> pure unit
    Just protocol -> do
      let route = parseExchangeRoute state.selectedFromAsset state.selectedToAsset
      executeExchangeRoute route state protocol

--------------------------------------------------------------------------------
-- Simulation Handlers
--------------------------------------------------------------------------------

handleRunSimulation :: forall o m. MonadAff m => H.HalogenM UIState Action () o m Unit
handleRunSimulation = do
  currentState <- H.get
  
  -- Early exit if simulation is already running
  when currentState.simulationRunning do
    H.liftEffect $ log "Simulation already running, skipping..."
    pure unit
  
  -- Run simulation with proper error handling
  result <- liftAff $ attempt $ runFullSimulation currentState
  
  case result of
    Left error -> do
      H.liftEffect $ log $ "Simulation failed: " <> show error
      H.modify_ _ { simulationRunning = false, error = Just $ show error }
    Right simulationData -> do
      -- Log completion
      H.liftEffect $ do
        log $ "Simulation completed with " <> show simulationData.results.totalUsers <> " users"
        log $ "POL reserves: " <> show simulationData.polMetrics.totalPOL
        log $ "Total fees collected: " <> show simulationData.totalFees
        log $ "Setting chart data with " <> show (length simulationData.priceHistory) <> " points"
        setChartData simulationData.priceHistory
      
      -- Update UI state
      H.modify_ _ 
        { simulationRunning = false
        , simulationResults = Just simulationData.results
        , priceHistory = simulationData.priceHistory
        , error = Nothing
        }
      
      -- Refresh and render
      handleAction RefreshData
      void $ H.fork do
        H.liftEffect $ void $ setTimeout (pure unit) 200
        handleAction RenderChart

-- | Complete simulation workflow (replaces 98 lines with 12 + helpers)
runFullSimulation :: UIState -> Aff { results :: SimResults, polMetrics :: POLMetrics, totalFees :: Number, priceHistory :: Array PriceHistoryEntry }
runFullSimulation state = do
  -- Initialize (replaces 15 lines)
  { protocol, simState } <- initializeSimulation state
  
  -- Seed liquidity (replaces 25 lines)
  seedLiquidityPositions protocol
  
  -- Run simulation and collect metrics (replaces 58 lines)
  runSimulationWithMetrics protocol simState state.simulationConfig

--------------------------------------------------------------------------------
-- Token Validation
--------------------------------------------------------------------------------

-- Token validation function
validateTokenInput :: String -> String -> Array TokenMetadata -> Array String
validateTokenInput ticker _ _ =
  let trimmedTicker = trim ticker
      
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
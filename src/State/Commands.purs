-- | Command processing for the Feels Protocol.
-- | This module handles all state modifications through commands.
module State.Commands
  ( commandHandlers
  , executeCommand
  , captureRebaseDifferential
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref (read, write)
-- Import state types
import State.Types (AppState, AppRuntime, AppCommand(..), AppResult(..))

-- Import domain modules
import Token (TokenType)
import Position (TermCommitment)
import Gateway (getTotalSupply)
import POL (contribute)
import Oracle (takeMarketSnapshot)
import FFI (currentTime)
import Errors (ProtocolError(..))
import Launch.Orchestrator as LO
import Launch.Integration as LI
import Launch.Launch (LaunchPhase(..))

-- Import action modules
import Actions.TokenActions as TokenActions
import Actions.PositionActions as PositionActions
import Actions.GatewayActions as GatewayActions

--------------------------------------------------------------------------------
-- Rebase Capture
--------------------------------------------------------------------------------

-- | Capture JitoSOL/FeelsSOL rebase differential for POL
captureRebaseDifferential :: AppRuntime -> Effect Unit
captureRebaseDifferential runtime = do
  state <- read runtime.state
  
  -- Get current JitoSOL price from oracle
  marketSnapshot <- takeMarketSnapshot state.oracle
  let currentJitoPrice = marketSnapshot.spot
      previousPrice = state.lastJitoSOLPrice
      
  -- Only capture if price has increased (staking rewards)
  when (currentJitoPrice > previousPrice) $ do
        -- Get total FeelsSOL supply from gateway
        totalSupply <- getTotalSupply state.gateway.syntheticSOL
        
        -- Calculate differential value
        let priceAppreciation = currentJitoPrice - previousPrice
            differentialValue = totalSupply * priceAppreciation
        
        -- Capture to POL
        when (differentialValue > 0.0) $ do
          contribute state.polState differentialValue
          
          -- Update state with new price
          let newState = state { lastJitoSOLPrice = currentJitoPrice }
          write newState runtime.state
          
          -- Notify listeners
          listeners <- read runtime.listeners
          _ <- traverse (\l -> l.callback newState) listeners
          pure unit

--------------------------------------------------------------------------------
-- Command Handlers
--------------------------------------------------------------------------------

-- | Handle token creation command
handleCreateToken :: String -> String -> String -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleCreateToken creator ticker name state = do
  result <- TokenActions.createToken creator ticker name state
  case result of
    Left err -> pure $ Left err
    Right newToken -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (TokenCreated newToken)

-- | Handle position creation command
handleCreatePosition :: String -> TokenType -> Number -> TokenType -> Number -> TermCommitment -> Maybe String -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleCreatePosition user lendAsset amount collateralAsset collateralAmount term targetToken state = do
  result <- PositionActions.createPosition user lendAsset amount collateralAsset collateralAmount term targetToken state
  case result of
    Left err -> pure $ Left err
    Right { position, positionTokenMap } -> do
      -- No legacy token staking needed - tokens are launched through batch auctions
      
      timestamp <- currentTime
      let newState = state 
            { positionTokenMap = positionTokenMap
            , timestamp = timestamp 
            }
      
      pure $ Right $ Tuple newState (PositionCreated position)

-- | Handle gateway entry command
handleEnterGateway :: String -> Number -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleEnterGateway user jitoAmount state = do
  result <- GatewayActions.enterGateway user jitoAmount state
  case result of
    Left err -> pure $ Left err
    Right gatewayResult -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (GatewayEntered gatewayResult)

-- | Handle gateway exit command
handleExitGateway :: String -> Number -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleExitGateway user feelsAmount state = do
  result <- GatewayActions.exitGateway user feelsAmount state
  case result of
    Left err -> pure $ Left err
    Right gatewayResult -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (GatewayExited gatewayResult)

-- | Handle launch start command
handleStartLaunch :: String -> String -> Number -> Number -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleStartLaunch _ ticker initialPrice totalTokens state = do
  -- Create pool for the token
  let poolId = ticker <> "/FeelsSOL"
  poolResult <- LI.createLaunchPool ticker poolId initialPrice state.poolRegistry
  
  case poolResult of
    Left err -> pure $ Left err
    Right _ -> do
      -- Create the launch
      launchResult <- LO.createLaunch ticker poolId initialPrice totalTokens state.timestamp state.launchOrchestrator
      case launchResult of
        Left err -> pure $ Left err
        Right launchId -> do
          timestamp <- currentTime
          let newState = state { timestamp = timestamp }
          pure $ Right $ Tuple newState (LaunchCreated launchId)

-- | Handle launch bid submission
handleSubmitLaunchBid :: String -> String -> Number -> Number -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleSubmitLaunchBid launchId bidder baseAmount priorityFee state = do
  result <- LO.submitBidToLaunch launchId bidder baseAmount priorityFee state.launchOrchestrator
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState LaunchBidSubmitted

-- | Handle launch batch processing
handleProcessLaunchBatch :: String -> AppState -> Effect (Either ProtocolError (Tuple AppState AppResult))
handleProcessLaunchBatch launchId state = do
  result <- LO.processNextBatch launchId state.launchOrchestrator
  case result of
    Left err -> pure $ Left err
    Right batchResult -> do
      -- Process winners and create positions
      _ <- LI.processBatchWinners 
        batchResult.batchResult 
        (launchId <> "/FeelsSOL")  -- Pool ID
        (case batchResult.newPhase of
          Just phase -> phase
          Nothing -> WeeklyPhase)  -- Default to weekly if no phase info
        state.currentBlock
        state.poolRegistry
      
      -- Route fees to POL
      LI.routeFeesToPOL batchResult.batchResult state.polState
      
      timestamp <- currentTime
      let newState = state 
            { timestamp = timestamp
            , currentBlock = state.currentBlock + 1  -- Advance block
            }
      
      pure $ Right $ Tuple newState $ LaunchBatchProcessed
        { tokensDistributed: batchResult.tokensDistributed
        , newPhase: case batchResult.newPhase of
            Nothing -> Nothing
            Just phase -> Just (show phase)
        , isComplete: batchResult.isComplete
        }

--------------------------------------------------------------------------------
-- Command Processing
--------------------------------------------------------------------------------

-- | Define command handlers for all state modifications
commandHandlers :: AppState -> AppCommand -> Effect (Either ProtocolError (Tuple AppState AppResult))
commandHandlers state = case _ of
  CreateToken creator ticker name -> 
    handleCreateToken creator ticker name state
  
  CreatePosition user lendAsset amount collateralAsset collateralAmount term targetToken -> 
    handleCreatePosition user lendAsset amount collateralAsset collateralAmount term targetToken state
  
  EnterGateway user jitoAmount -> 
    handleEnterGateway user jitoAmount state
  
  ExitGateway user feelsAmount -> 
    handleExitGateway user feelsAmount state
  
  StartLaunch creator ticker initialPrice totalTokens ->
    handleStartLaunch creator ticker initialPrice totalTokens state
  
  SubmitLaunchBid launchId bidder baseAmount priorityFee ->
    handleSubmitLaunchBid launchId bidder baseAmount priorityFee state
  
  ProcessLaunchBatch launchId ->
    handleProcessLaunchBatch launchId state
  
  _ -> pure $ Left $ InvalidCommandError "Command not implemented"

-- | Execute a command that modifies state
executeCommand :: AppRuntime -> AppCommand -> Effect (Either ProtocolError AppResult)
executeCommand runtime cmd = do
  state <- read runtime.state
  result <- commandHandlers state cmd
  
  case result of
    Right (Tuple newState cmdResult) -> do
      -- Update state
      write newState runtime.state
      
      -- Notify listeners
      listeners <- read runtime.listeners
      _ <- traverse (\l -> l.callback newState) listeners
      
      pure $ Right cmdResult
    Left err -> pure $ Left err
-- | Command processing for the Feels Protocol UI.
-- | This module handles all state modifications through commands.
module UI.Commands
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
import Data.Map as Map
import Math (log, floor)

-- Import app state types
import UI.ProtocolState (ProtocolState, AppRuntime, ProtocolCommand(..))
import Protocol.Common (CommandResult(..))

-- Import domain modules
import Protocol.Token (TokenType)
import Protocol.Position (TermCommitment)
import Protocol.FeelsSOL (getTotalSupply)
import Protocol.POL (contribute)
import Protocol.Oracle (takeMarketSnapshot)
import FFI (currentTime)
import Protocol.Errors (ProtocolError(..))
import Protocol.Offering as Offering
import Protocol.Offering (OfferingPhase(..), OfferingConfig, PhaseConfig)

-- Import action modules
import UI.Actions.TokenActions as TokenActions
import UI.Actions.PositionActions as PositionActions
import UI.Actions.FeelsSOLActions as FeelsSOLActions
import UI.Actions.AccountActions as AccountActions
import UI.PoolRegistry (PoolRegistry, getPool)
import Protocol.Pool (PoolState)

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
        -- Get total FeelsSOL supply from FeelsSOL state
        totalSupply <- getTotalSupply state.feelsSOL
        
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
handleCreateToken :: String -> String -> String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCreateToken creator ticker name state = do
  result <- TokenActions.createToken creator ticker name state
  case result of
    Left err -> pure $ Left err
    Right newToken -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (TokenCreated newToken)

-- | Handle position creation command
handleCreatePosition :: String -> TokenType -> Number -> TokenType -> Number -> TermCommitment -> Maybe String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
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

-- | Handle token transfer command
handleTransferTokens :: String -> String -> TokenType -> Number -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleTransferTokens from to token amount state = do
  result <- AccountActions.transferTokens from to token amount state
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (TokensTransferred { from, to, token, amount })

-- | Handle FeelsSOL entry command
handleEnterFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleEnterFeelsSOL user jitoAmount state = do
  result <- FeelsSOLActions.enterFeelsSOL user jitoAmount state
  case result of
    Left err -> pure $ Left err
    Right feelsSOLMinted -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (FeelsSOLMinted { user, feelsSOLMinted })

-- | Handle FeelsSOL exit command
handleExitFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleExitFeelsSOL user feelsAmount state = do
  result <- FeelsSOLActions.exitFeelsSOL user feelsAmount state
  case result of
    Left err -> pure $ Left err
    Right jitoSOLReceived -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (FeelsSOLBurned { user, jitoSOLReceived })

-- | Handle unbonding initiation
handleInitiateUnbonding :: String -> Int -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleInitiateUnbonding user positionId state = do
  result <- PositionActions.initiateUnbonding user positionId state
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (UnbondingInitiated positionId)

-- | Handle position withdrawal
handleWithdrawPosition :: String -> Int -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleWithdrawPosition user positionId state = do
  result <- PositionActions.withdrawPosition user positionId state
  case result of
    Left err -> pure $ Left err
    Right _ -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      pure $ Right $ Tuple newState (PositionWithdrawn positionId)

-- | Handle offering creation
handleCreateOffering :: String -> Number -> Array { phase :: String, tokens :: Number, priceLower :: Number, priceUpper :: Number } -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCreateOffering ticker totalTokens phases state = do
  -- Convert phase configs
  let phaseConfigs = map convertPhase phases
      config = { tokenTicker: ticker
               , totalTokens: totalTokens
               , phases: phaseConfigs
               , treasuryAddress: "treasury"
               }
  
  offeringState <- Offering.initOffering config
  let poolId = "FeelsSOL/" <> ticker
      newState = state { offerings = Map.insert poolId offeringState state.offerings
                       , timestamp = state.timestamp 
                       }
  pure $ Right $ Tuple newState (OfferingCreated poolId ticker)
  where
    convertPhase p = 
      { phase: parsePhase p.phase
      , tokenAmount: p.tokens
      , priceRangeLower: p.priceLower
      , priceRangeUpper: p.priceUpper
      , tickLower: priceToTick p.priceLower
      , tickUpper: priceToTick p.priceUpper
      }
    parsePhase "Weekly" = WeeklyPhase
    parsePhase "Daily" = DailyPhase
    parsePhase _ = SpotPhase
    priceToTick price = floor (log price / log 1.0001)

-- | Handle offering phase start
handleStartOfferingPhase :: String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleStartOfferingPhase poolId state = do
  case Map.lookup poolId state.offerings of
    Nothing -> pure $ Left $ InvalidCommandError "Offering not found"
    Just offeringRef -> do
      -- Get pool state
      pool <- lookupPool poolId state.poolRegistry
      case pool of
        Nothing -> pure $ Left $ InvalidCommandError "Pool not found"
        Just poolState -> do
          result <- Offering.startPhase offeringRef poolState state.currentBlock
          case result of
            Left err -> pure $ Left err
            Right _ -> do
              offering <- read offeringRef
              pure $ Right $ Tuple state (OfferingPhaseStarted poolId (show offering.currentPhase))

-- | Handle offering phase completion
handleCompleteOfferingPhase :: String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCompleteOfferingPhase poolId state = do
  case Map.lookup poolId state.offerings of
    Nothing -> pure $ Left $ InvalidCommandError "Offering not found"
    Just offeringRef -> do
      pool <- lookupPool poolId state.poolRegistry
      case pool of
        Nothing -> pure $ Left $ InvalidCommandError "Pool not found"
        Just poolState -> do
          -- Check if phase is complete
          isComplete <- Offering.checkPhaseComplete offeringRef poolState
          if not isComplete
            then pure $ Left $ InvalidCommandError "Phase not complete"
            else do
              result <- Offering.completeOffering offeringRef
              case result of
                Left err -> pure $ Left err
                Right nextPhase -> pure $ Right $ Tuple state (OfferingPhaseCompleted poolId (show nextPhase))

--------------------------------------------------------------------------------
-- Command Mapping
--------------------------------------------------------------------------------

-- | Map of command handlers
commandHandlers :: ProtocolCommand -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
commandHandlers = executeCommand

-- | Execute a command
executeCommand :: ProtocolCommand -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
executeCommand cmd state = case cmd of
  CreateToken creator ticker name -> 
    handleCreateToken creator ticker name state
  CreatePosition user lendAsset amount collateralAsset collateralAmount term targetToken ->
    handleCreatePosition user lendAsset amount collateralAsset collateralAmount term targetToken state
  TransferTokens from to token amount ->
    handleTransferTokens from to token amount state
  EnterFeelsSOL user amount ->
    handleEnterFeelsSOL user amount state
  ExitFeelsSOL user amount ->
    handleExitFeelsSOL user amount state
  InitiateUnbonding user positionId ->
    handleInitiateUnbonding user positionId state
  WithdrawPosition user positionId ->
    handleWithdrawPosition user positionId state
  CreateOffering ticker totalTokens phases ->
    handleCreateOffering ticker totalTokens phases state
  StartOfferingPhase poolId ->
    handleStartOfferingPhase poolId state
  CompleteOfferingPhase poolId ->
    handleCompleteOfferingPhase poolId state

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Lookup pool from registry
lookupPool :: String -> PoolRegistry -> Effect (Maybe PoolState)
lookupPool poolId registry = getPool poolId registry


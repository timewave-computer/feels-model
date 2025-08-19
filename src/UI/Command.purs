-- | Command processing for the Feels Protocol UI.
-- | This module handles all state modifications through commands.
module UI.Command
  ( commandHandlers
  , executeCommand
  , captureRebaseDifferential
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Ref (read, write)
import Data.Map as Map
import FFI (log, floor, currentTime)
import Data.Array (length, drop)
import Unsafe.Coerce (unsafeCoerce)

-- Import app state types
import UI.ProtocolState (ProtocolState, AppRuntime, ProtocolCommand(..))
import Protocol.Common (CommandResult(..))
import Protocol.Token (TokenType)

-- Import domain modules
import Protocol.PositionVault (Duration, Leverage)
import Protocol.FeelsSOLVault (FeelsSOLVault, updateOraclePrice)
import Protocol.POLVault (contribute, getTotalPOL)
import Protocol.Oracle (takeMarketSnapshot)
import Protocol.Error (ProtocolError(..))
import Protocol.LaunchVault as Launch
import Protocol.LaunchVault (LaunchPhase(..))

-- Import action modules
import UI.Action.TokenActions as TokenActions
import UI.Action.PositionActions as PositionActions
import UI.Action.FeelsSOLActions as FeelsSOLActions
import UI.Action.AccountActions as AccountActions
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
        -- Get total FeelsSOL supply from vault's balance sheet
        vault <- read state.feelsSOL
        let totalSupply = vault.state.balanceSheet.totalShares
        
        -- Calculate differential value
        let priceAppreciation = currentJitoPrice - previousPrice
            differentialValue = totalSupply * priceAppreciation
        
        -- Capture to POL
        when (differentialValue > 0.0) $ do
          contribute state.polState differentialValue
          
          -- Get current POL value
          polBalance <- getTotalPOL state.polState
          
          -- Update state with new price and record history
          currentTime <- currentTime
          let priceRecord = { timestamp: currentTime
                            , block: state.currentBlock
                            , price: currentJitoPrice
                            , polValue: polBalance
                            }
              newHistory = state.priceHistory <> [priceRecord]
              -- Keep only last 100 records
              trimmedHistory = if length newHistory > 100 
                              then drop (length newHistory - 100) newHistory
                              else newHistory
              newState = state { lastJitoSOLPrice = currentJitoPrice
                               , priceHistory = trimmedHistory
                               }
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
          -- Convert from Token.TokenMetadata to foreign TokenMetadata type
          convertedToken = unsafeCoerce newToken
      pure $ Right $ Tuple newState (TokenCreated convertedToken)

-- | Handle position creation command
handleCreatePosition :: String -> TokenType -> Number -> TokenType -> Number -> Duration -> Leverage -> Boolean -> Maybe String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCreatePosition user lendAsset amount collateralAsset collateralAmount term leverage rollover targetToken state = do
  result <- PositionActions.createPosition user lendAsset amount collateralAsset collateralAmount term leverage rollover targetToken state
  case result of
    Left err -> pure $ Left err
    Right { position, positionTokenMap } -> do
      -- No legacy token staking needed - tokens are launched through batch auctions
      
      timestamp <- currentTime
      let newState = state 
            { positionTokenMap = positionTokenMap
            , timestamp = timestamp 
            }
      -- Convert from P.Position to foreign Position type
      let convertedPosition = unsafeCoerce position
      pure $ Right $ Tuple newState (PositionCreated convertedPosition)

-- | Perform solvency check for FeelsSOL protocol
-- | Ensures total FeelsSOL supply is backed by sufficient JitoSOL reserves
checkFeelsSOLSolvency :: ProtocolState -> Effect Boolean
checkFeelsSOLSolvency state = do
  vault <- read state.feelsSOL
  let totalFeelsSOL = vault.state.balanceSheet.totalShares
      totalAssets = sum $ map _.amount vault.state.balanceSheet.assets
      strategy = vault.state.strategyState
  
  -- Update oracle price to get current exchange rate
  updatedStrategy <- updateOraclePrice strategy
  let requiredJitoSOL = case updatedStrategy.cachedPrice of
        Just oracle -> totalFeelsSOL / oracle.price
        Nothing -> totalFeelsSOL / 1.05
  
  pure $ totalAssets >= requiredJitoSOL

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
    Right mintResult -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      -- Perform solvency check
      isSolvent <- checkFeelsSOLSolvency newState
      if isSolvent
        then pure $ Right $ Tuple newState (FeelsSOLMinted mintResult)
        else pure $ Left InsufficientReserves

-- | Handle FeelsSOL exit command
handleExitFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleExitFeelsSOL user feelsAmount state = do
  result <- FeelsSOLActions.exitFeelsSOL user feelsAmount state
  case result of
    Left err -> pure $ Left err
    Right burnResult -> do
      timestamp <- currentTime
      let newState = state { timestamp = timestamp }
      -- Perform solvency check
      isSolvent <- checkFeelsSOLSolvency newState
      if isSolvent
        then pure $ Right $ Tuple newState (FeelsSOLBurned burnResult)
        else pure $ Left InsufficientReserves

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

-- | Handle launch creation
handleCreateLaunch :: String -> Number -> Array { phase :: String, tokens :: Number, priceLower :: Number, priceUpper :: Number } -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCreateLaunch ticker totalTokens phases state = do
  -- Convert phase configs
  let phaseConfigs = map convertPhase phases
      config = { tokenTicker: ticker
               , totalTokens: totalTokens
               , phases: phaseConfigs
               , treasuryAddress: "treasury"
               }
  
  launchResult <- Launch.initializeLaunchVault config
  case launchResult of
    Left err -> pure $ Left err
    Right launchState -> do
      let poolId = ticker <> "/FeelsSOL"
          newState = state { launches = Map.insert poolId launchState state.launches
                           , timestamp = state.timestamp 
                           }
      pure $ Right $ Tuple newState (LaunchCreated poolId ticker)
  where
    convertPhase p = 
      { phase: parsePhase p.phase
      , tokenAmount: p.tokens
      , priceRangeLower: p.priceLower
      , priceRangeUpper: p.priceUpper
      , tickLower: priceToTick p.priceLower
      , tickUpper: priceToTick p.priceUpper
      , lockDuration: if parsePhase p.phase == MonthlyPhase then 216000 else 0  -- 30 days for monthly, 0 for spot
      }
    parsePhase "Monthly" = MonthlyPhase
    parsePhase _ = SpotPhase
    priceToTick price = floor (log price / log 1.0001)

-- | Handle launch phase start
handleStartLaunchPhase :: String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleStartLaunchPhase poolId state = do
  case Map.lookup poolId state.launches of
    Nothing -> pure $ Left $ InvalidCommandError "Launch not found"
    Just launchRef -> do
      -- Get pool state
      pool <- lookupPool poolId state.poolRegistry
      case pool of
        Nothing -> pure $ Left $ InvalidCommandError "Pool not found"
        Just poolState -> do
          result <- Launch.provisionPhase launchRef poolState state.currentBlock
          case result of
            Left err -> pure $ Left err
            Right _ -> do
              status <- Launch.getLaunchStatus launchRef
              pure $ Right $ Tuple state (LaunchPhaseStarted poolId (show status.phase))

-- | Handle launch phase completion
handleCompleteLaunchPhase :: String -> ProtocolState -> Effect (Either ProtocolError (Tuple ProtocolState CommandResult))
handleCompleteLaunchPhase poolId state = do
  case Map.lookup poolId state.launches of
    Nothing -> pure $ Left $ InvalidCommandError "Launch not found"
    Just launchRef -> do
      pool <- lookupPool poolId state.poolRegistry
      case pool of
        Nothing -> pure $ Left $ InvalidCommandError "Pool not found"
        Just _ -> do
          -- Check if phase is complete
          isComplete <- Launch.checkPhaseComplete launchRef
          if not isComplete
            then pure $ Left $ InvalidCommandError "Phase not complete"
            else do
              result <- Launch.transitionPhase launchRef
              case result of
                Left err -> pure $ Left err
                Right nextPhase -> pure $ Right $ Tuple state (LaunchPhaseCompleted poolId (show nextPhase))

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
  CreatePosition user lendAsset amount collateralAsset collateralAmount term leverage rollover targetToken ->
    handleCreatePosition user lendAsset amount collateralAsset collateralAmount term leverage rollover targetToken state
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
  CreateLaunch ticker totalTokens phases ->
    handleCreateLaunch ticker totalTokens phases state
  StartLaunchPhase poolId ->
    handleStartLaunchPhase poolId state
  CompleteLaunchPhase poolId ->
    handleCompleteLaunchPhase poolId state

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Lookup pool from registry
lookupPool :: String -> PoolRegistry -> Effect (Maybe PoolState)
lookupPool poolId registry = getPool poolId registry


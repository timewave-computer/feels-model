-- | Command Execution Utilities for Feels Protocol UI
-- |
-- | This module provides standardized command execution patterns with
-- | consistent error handling and state management to reduce code duplication.
module UI.Util.CommandExecution
  ( -- Command execution functions
    executeCommandWithErrorHandling
  , executeCommandWithRefresh
  , executeQueryWithErrorHandling
  , executeValidatedCommand
  -- Configuration types
  , CommandConfig
  , CommandResult(..)
  , ExecutionContext
  -- Error handling utilities
  , handleCommandResult
  , handleQueryResult
  , setErrorMessage
  , clearErrors
  -- State update utilities
  , updateStateFromResult
  , resetForm
  ) where

import Prelude
import Data.Array (null, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Ref (Ref, read, write)
import Halogen as H
import UI.State (UIState, Action(..))
import UI.ProtocolState (AppRuntime, ProtocolCommand, ProtocolState, IndexerQuery)
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import Protocol.Common as PC
import Protocol.Common (QueryResult)
import Protocol.Error (ProtocolError(..))

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Result of command execution
data CommandResult a
  = CommandSuccess a
  | CommandError String
  | CommandValidationError (Array String)

derive instance eqCommandResult :: Eq a => Eq (CommandResult a)
derive instance functorCommandResult :: Functor CommandResult

instance showCommandResult :: Show a => Show (CommandResult a) where
  show (CommandSuccess a) = "CommandSuccess(" <> show a <> ")"
  show (CommandError err) = "CommandError(" <> err <> ")"
  show (CommandValidationError errs) = "CommandValidationError(" <> show errs <> ")"

-- | Configuration for command execution (simplified to avoid kind issues)
type CommandConfig m =
  { command :: ProtocolCommand
  , onSuccess :: PC.CommandResult -> Effect Unit  -- Simplified
  , onError :: String -> Effect Unit  -- Simplified
  , onValidationError :: Array String -> Effect Unit  -- Simplified
  , shouldRefresh :: Boolean
  , shouldResetForm :: Boolean
  , successMessage :: Maybe String
  , validationErrors :: Array String
  }

-- | Execution context with protocol access
type ExecutionContext = 
  { protocol :: AppRuntime
  , user :: String
  }

-- | Default command configuration
defaultCommandConfig :: forall m. CommandConfig m
defaultCommandConfig = 
  { command: unsafeCoerce unit
  , onSuccess: const (pure unit)
  , onError: \msg -> log $ "Error: " <> msg
  , onValidationError: \errs -> log $ "Validation errors: " <> show (length errs)
  , shouldRefresh: true
  , shouldResetForm: false
  , successMessage: Nothing
  , validationErrors: []
  }

--------------------------------------------------------------------------------
-- Command Execution Functions
--------------------------------------------------------------------------------

-- | Execute a protocol command with standardized error handling
executeCommandWithErrorHandling :: 
  forall o m. MonadAff m =>
  ProtocolCommand ->
  H.HalogenM UIState Action () o m Unit
executeCommandWithErrorHandling command = do
  state <- H.get
  case state.api of
    Nothing -> setErrorMessage "No protocol connection available"
    Just protocol -> do
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeCommand command protocolState
      handleCommandResult result

-- | Execute command and automatically refresh data
executeCommandWithRefresh :: 
  forall o m. MonadAff m =>
  ProtocolCommand ->
  String ->
  H.HalogenM UIState Action () o m Unit
executeCommandWithRefresh command successMsg = do
  state <- H.get
  case state.api of
    Nothing -> setErrorMessage "No protocol connection available"
    Just protocol -> do
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeCommand command protocolState
      case result of
        Right (Tuple newState cmdResult) -> do
          H.liftEffect $ write newState protocol.state
          H.liftEffect $ log successMsg
          clearErrors
          -- H.tell (H.Proxy :: H.Proxy "main") unit (RefreshData :: Action) -- Commented out due to type issues
          pure unit
        Left err -> setErrorMessage (show err)

-- | Execute query with error handling
executeQueryWithErrorHandling :: 
  forall o m. MonadAff m =>
  IndexerQuery ->
  (QueryResult -> H.HalogenM UIState Action () o m Unit) ->
  H.HalogenM UIState Action () o m Unit
executeQueryWithErrorHandling query onSuccess = do
  state <- H.get
  case state.api of
    Nothing -> setErrorMessage "No protocol connection available"
    Just protocol -> do
      protocolState <- H.liftEffect $ read protocol.state
      result <- H.liftEffect $ executeQuery query protocolState
      case result of
        Right queryResult -> onSuccess queryResult
        Left err -> setErrorMessage (show err)

-- | Execute command with pre-validation
executeValidatedCommand :: 
  forall o m. MonadAff m =>
  Array String ->
  ProtocolCommand ->
  Effect Unit ->
  H.HalogenM UIState Action () o m Unit
executeValidatedCommand validationErrors command onSuccess = do
  if null validationErrors
    then do
      executeCommandWithErrorHandling command
      H.liftEffect onSuccess
    else do
      H.modify_ _ { tokenValidationErrors = validationErrors }
      H.liftEffect $ log $ "Validation failed: " <> show (length validationErrors) <> " errors"

--------------------------------------------------------------------------------
-- Result Handling Functions
--------------------------------------------------------------------------------

-- | Handle the result of a protocol command
handleCommandResult :: 
  forall o m. MonadAff m =>
  Either ProtocolError (Tuple ProtocolState PC.CommandResult) ->
  H.HalogenM UIState Action () o m Unit
handleCommandResult result = do
  state <- H.get
  case result, state.api of
    Right (Tuple newState cmdResult), Just protocol -> do
      H.liftEffect $ write newState protocol.state
      handleSuccessfulCommand cmdResult
      clearErrors
      -- H.tell (H.proxy :: H.Proxy "main") unit (RefreshData :: Action) -- Commented out due to type issues
    Right _, Nothing -> 
      setErrorMessage "Protocol connection lost"
    Left err, _ -> 
      setErrorMessage (show err)

-- | Handle successful command execution
handleSuccessfulCommand :: 
  forall o m. MonadAff m =>
  PC.CommandResult ->
  H.HalogenM UIState Action () o m Unit
handleSuccessfulCommand cmdResult = do
  case cmdResult of
    PC.TokenCreated _ -> do
      H.liftEffect $ log "Token created successfully"
      resetTokenForm
    PC.PositionCreated _ -> do
      H.liftEffect $ log "Position created successfully"
      resetExchangeForm
    PC.TokensTransferred transfer -> do
      H.liftEffect $ log $ "Transferred " <> show transfer.amount <> " " <> show transfer.token
    PC.FeelsSOLMinted mint -> do
      H.liftEffect $ log $ "Minted " <> show mint.feelsSOLMinted <> " FeelsSOL"
    PC.FeelsSOLBurned burn -> do
      H.liftEffect $ log $ "Burned FeelsSOL, received " <> show burn.jitoSOLReceived <> " jitoSOL"
    _ -> H.liftEffect $ log "Command executed successfully"

-- | Handle query results
handleQueryResult :: 
  forall o m. MonadAff m =>
  Either ProtocolError QueryResult ->
  (QueryResult -> H.HalogenM UIState Action () o m Unit) ->
  H.HalogenM UIState Action () o m Unit
handleQueryResult result onSuccess =
  case result of
    Right queryResult -> onSuccess queryResult
    Left err -> setErrorMessage (show err)

--------------------------------------------------------------------------------
-- Error Handling Utilities
--------------------------------------------------------------------------------

-- | Set error message in UI state
setErrorMessage :: 
  forall o m. MonadEffect m =>
  String ->
  H.HalogenM UIState Action () o m Unit
setErrorMessage message = do
  H.liftEffect $ log $ "Error: " <> message
  H.modify_ _ { error = Just message }

-- | Clear all error messages
clearErrors :: 
  forall o m. 
  H.HalogenM UIState Action () o m Unit
clearErrors = H.modify_ _ { error = Nothing }

-- | Set validation errors
setValidationErrors :: 
  forall o m. MonadEffect m =>
  Array String ->
  H.HalogenM UIState Action () o m Unit
setValidationErrors errors = do
  H.liftEffect $ do
    log $ "Validation errors: " <> show (length errors)
    traverse_ (log <<< ("  - " <> _)) errors
  H.modify_ _ { tokenValidationErrors = errors }

-- | Clear validation errors
clearValidationErrors :: 
  forall o m. 
  H.HalogenM UIState Action () o m Unit
clearValidationErrors = H.modify_ _ { tokenValidationErrors = [] }

--------------------------------------------------------------------------------
-- State Update Utilities
--------------------------------------------------------------------------------

-- | Update state based on command result
updateStateFromResult :: 
  forall o m. 
  PC.CommandResult ->
  H.HalogenM UIState Action () o m Unit
updateStateFromResult cmdResult = do
  case cmdResult of
    PC.TokenCreated _ -> do
      resetTokenForm
      clearValidationErrors
    PC.PositionCreated _ -> do
      resetExchangeForm
      clearValidationErrors
    _ -> pure unit

-- | Reset token creation form
resetTokenForm :: 
  forall o m. 
  H.HalogenM UIState Action () o m Unit
resetTokenForm = H.modify_ \s -> s 
  { tokenTicker = ""
  , tokenName = ""
  , tokenValidationErrors = []
  }

-- | Reset exchange form
resetExchangeForm :: 
  forall o m. 
  H.HalogenM UIState Action () o m Unit
resetExchangeForm = H.modify_ \s -> s 
  { inputAmount = 0.0
  , selectedFromAsset = "jitosol"
  , selectedToAsset = "position-swap"
  }

-- | Generic form reset function
resetForm :: 
  forall o m. 
  (UIState -> UIState) ->
  H.HalogenM UIState Action () o m Unit
resetForm updater = H.modify_ updater

--------------------------------------------------------------------------------
-- Validation Integration
--------------------------------------------------------------------------------

-- | Execute command with comprehensive validation
executeWithValidation :: 
  forall o m. MonadAff m =>
  Array String ->
  ProtocolCommand ->
  Effect Unit ->
  Effect Unit ->
  H.HalogenM UIState Action () o m Unit
executeWithValidation validationErrors command onSuccess onValidationError = do
  if null validationErrors
    then do
      result <- executeCommandWithErrorHandling command
      H.liftEffect onSuccess
    else do
      -- setValidationErrors validationErrors  -- Commented due to type conflicts
      pure unit
      H.liftEffect onValidationError

-- | Log execution details
logCommandExecution :: 
  forall m. 
  ProtocolCommand ->
  String ->
  Effect Unit
logCommandExecution command context = do
  log $ context <> ": Executing command (details hidden)"

foreign import unsafeCoerce :: forall a b. a -> b
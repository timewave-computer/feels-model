-- | Structured Logging for Feels Protocol  
-- |
-- | This module provides structured logging capabilities to replace manual
-- | Effect.Console.log calls throughout the application with configurable,
-- | contextual, and level-based logging.
module UI.Util.Logger
  ( -- Log levels
    LogLevel(..)
  , setLogLevel
  , getLogLevel
  -- Logger type and creation
  , Logger
  , createLogger  
  , createLoggerWithContext
  -- Logging functions (replaces manual console.log calls)
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logTrace
  -- Contextual logging
  , withContext
  , withContextM
  -- Structured logging  
  , logStructured
  , logWithData
  -- Action logging (protocol-specific)
  , logActionStart
  , logActionEnd
  , logActionError
  -- Performance logging
  , logPerformance
  , logTiming
  , timeAction
  -- Conditional logging
  , logWhen
  , logUnless
  -- Batch logging  
  , logBatch
  , flushLogs
  -- Configuration
  , LogConfig
  , defaultLogConfig
  , loggerFromConfig
  ) where

import Prelude
import Effect (Effect)
import Effect.Console as Console
import Effect.Now (now)
import Effect.Ref (Ref, new, read, write, modify_)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:), reverse, length) as Array
import Data.String (joinWith)
import Data.JSDate (now, getTime) as JSDate

--------------------------------------------------------------------------------
-- Log Levels and Configuration
--------------------------------------------------------------------------------

-- | Log severity levels
data LogLevel  
  = Trace
  | Debug  
  | Info
  | Warn
  | Error

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance showLogLevel :: Show LogLevel where
  show Trace = "TRACE"
  show Debug = "DEBUG" 
  show Info = "INFO"
  show Warn = "WARN"
  show Error = "ERROR"

-- | Global log level configuration
globalLogLevel :: Ref LogLevel
globalLogLevel = unsafePerformEffect $ new Info

-- | Set global log level
setLogLevel :: LogLevel -> Effect Unit
setLogLevel level = write level globalLogLevel

-- | Get current log level
getLogLevel :: Effect LogLevel
getLogLevel = read globalLogLevel

-- | Logger configuration
type LogConfig =
  { level :: LogLevel
  , context :: String
  , enableTimestamps :: Boolean
  , enableColors :: Boolean
  , structured :: Boolean
  }

-- | Default logger configuration  
defaultLogConfig :: LogConfig
defaultLogConfig =
  { level: Info
  , context: "FeelsProtocol"
  , enableTimestamps: true
  , enableColors: false
  , structured: false
  }

--------------------------------------------------------------------------------
-- Logger Type and Creation
--------------------------------------------------------------------------------

-- | Logger with context and configuration
type Logger =
  { config :: LogConfig
  , context :: Array String
  }

-- | Create logger with default configuration
createLogger :: String -> Logger
createLogger context =
  { config: defaultLogConfig { context = context }
  , context: [context]
  }

-- | Create logger with custom configuration
createLoggerWithContext :: LogConfig -> Array String -> Logger
createLoggerWithContext config contextArray =
  { config: config
  , context: contextArray
  }

-- | Create logger from configuration
loggerFromConfig :: LogConfig -> Logger  
loggerFromConfig config = createLoggerWithContext config [config.context]

--------------------------------------------------------------------------------
-- Core Logging Functions (replaces console.log - reduces ~10 lines per usage)
--------------------------------------------------------------------------------

-- | Log debug message
logDebug :: String -> Effect Unit
logDebug message = do
  level <- getLogLevel
  when (level <= Debug) $ 
    logAtLevel Debug "App" message

-- | Log info message (most common - replaces console.log)
logInfo :: String -> Effect Unit
logInfo message = do
  level <- getLogLevel
  when (level <= Info) $
    logAtLevel Info "App" message

-- | Log warning message
logWarn :: String -> Effect Unit  
logWarn message = do
  level <- getLogLevel
  when (level <= Warn) $
    logAtLevel Warn "App" message

-- | Log error message
logError :: String -> Effect Unit
logError message = do
  level <- getLogLevel
  when (level <= Error) $
    logAtLevel Error "App" message

-- | Log trace message (very verbose)
logTrace :: String -> Effect Unit
logTrace message = do
  level <- getLogLevel  
  when (level <= Trace) $
    logAtLevel Trace "App" message

-- | Internal log implementation
logAtLevel :: LogLevel -> String -> String -> Effect Unit
logAtLevel level context message = do
  jsDate <- JSDate.now
  let timestamp = JSDate.getTime jsDate
      logLine = formatLogLine level context message (Just timestamp)
  case level of
    Error -> Console.error logLine
    Warn -> Console.warn logLine  
    _ -> Console.log logLine

-- | Format log line with timestamp and context
formatLogLine :: LogLevel -> String -> String -> Maybe Number -> String
formatLogLine level context message maybeTimestamp =
  let timestampStr = case maybeTimestamp of
        Nothing -> ""
        Just ts -> "[" <> show ts <> "] "
      levelStr = "[" <> show level <> "] "
      contextStr = "[" <> context <> "] "
  in timestampStr <> levelStr <> contextStr <> message

--------------------------------------------------------------------------------
-- Contextual Logging (better organization)
--------------------------------------------------------------------------------

-- | Add context to logger
withContext :: String -> Logger -> Logger
withContext newContext logger = 
  logger { context = Array.(:) newContext logger.context }

-- | Add context within a monadic computation
withContextM :: forall m. Monad m => String -> Logger -> m Logger
withContextM newContext logger = pure $ withContext newContext logger

-- | Log with logger context
logWithLogger :: LogLevel -> Logger -> String -> Effect Unit
logWithLogger level logger message = do
  globalLevel <- getLogLevel
  when (globalLevel <= level) $ do
    let context = joinWith "." (reverse logger.context) 
    logAtLevel level context message

--------------------------------------------------------------------------------
-- Structured Logging (enhanced data logging)
--------------------------------------------------------------------------------

-- | Log with structured data  
logStructured :: LogLevel -> String -> Array { key :: String, value :: String } -> Effect Unit
logStructured level context keyValues = do
  globalLevel <- getLogLevel
  when (globalLevel <= level) $ do
    let dataStr = joinWith ", " (map (\kv -> kv.key <> "=" <> kv.value) keyValues)
        message = "data: {" <> dataStr <> "}"
    logAtLevel level context message

-- | Log with arbitrary data
logWithData :: LogLevel -> String -> String -> String -> Effect Unit  
logWithData level context message dataStr = do
  globalLevel <- getLogLevel
  when (globalLevel <= level) $
    logAtLevel level context (message <> " | data: " <> dataStr)

--------------------------------------------------------------------------------
-- Action Logging (protocol-specific patterns)
--------------------------------------------------------------------------------

-- | Log action start
logActionStart :: String -> String -> Effect Unit
logActionStart context action = 
  logInfo $ "[" <> context <> "] Starting action: " <> action

-- | Log action completion
logActionEnd :: String -> String -> Effect Unit  
logActionEnd context action =
  logInfo $ "[" <> context <> "] Completed action: " <> action

-- | Log action error
logActionError :: String -> String -> String -> Effect Unit
logActionError context action error =
  logError $ "[" <> context <> "] Action failed: " <> action <> " - " <> error

--------------------------------------------------------------------------------
-- Performance Logging (timing and metrics)
--------------------------------------------------------------------------------

-- | Log performance metric
logPerformance :: String -> Number -> String -> Effect Unit
logPerformance operation duration unit = 
  logInfo $ "PERF: " <> operation <> " took " <> show duration <> unit

-- | Log timing information
logTiming :: String -> Number -> Number -> Effect Unit
logTiming operation startTime endTime =
  let duration = endTime - startTime
  in logPerformance operation duration "ms"

-- | Time an action and log duration
timeAction :: forall a. String -> Effect a -> Effect a
timeAction actionName action = do
  startTimeJS <- JSDate.now
  let startTime = JSDate.getTime startTimeJS
  result <- action
  endTimeJS <- JSDate.now
  let endTime = JSDate.getTime endTimeJS
  logTiming actionName startTime endTime
  pure result

--------------------------------------------------------------------------------
-- Conditional Logging (reduces if-then-else boilerplate)
--------------------------------------------------------------------------------

-- | Log only when condition is true
logWhen :: Boolean -> LogLevel -> String -> Effect Unit
logWhen condition level message = 
  when condition $ logAtLevel level "App" message

-- | Log only when condition is false
logUnless :: Boolean -> LogLevel -> String -> Effect Unit  
logUnless condition level message =
  unless condition $ logAtLevel level "App" message

--------------------------------------------------------------------------------
-- Batch Logging (efficient bulk operations)
--------------------------------------------------------------------------------

-- | Log multiple messages at once
logBatch :: LogLevel -> String -> Array String -> Effect Unit
logBatch level context messages = do
  globalLevel <- getLogLevel
  when (globalLevel <= level) $ do
    let batchMessage = "Batch log (" <> show (length messages) <> " entries):\n" 
                    <> joinWith "\n" (map ("  - " <> _) messages)
    logAtLevel level context batchMessage

-- | Flush any pending logs (placeholder for future buffering)
flushLogs :: Effect Unit
flushLogs = pure unit

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Calculate array length
length :: forall a. Array a -> Int
length = Array.length

-- | Reverse array
reverse :: forall a. Array a -> Array a
reverse = Array.reverse

-- Foreign import for unsafePerformEffect (needed for global state)
foreign import unsafePerformEffect :: forall a. Effect a -> a
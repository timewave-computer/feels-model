module Clock
  ( Clock
  , ClockState
  , EventType(..)
  , EventResult
  , BlockEvent
  , ProtocolSequence
  , EventHandlers
  , initClock
  , tick
  , getCurrentBlock
  , getEventHistory
  , setOracleUpdateFn
  , setGatewayRebaseFn
  , setRiskCalculationFn
  , setParameterUpdateFn
  , setFeeUpdateFn
  , setPOLUpdateFn
  , setPositionMaturityCheckFn
  , setLendingBookMatchingFn
  , setPOLMetricsUpdateFn
  , setTokenCreationProcessingFn
  , setSystemHealthCheckFn
  , setTermExpiryCheckFn
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), take, length)
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Effect.Console (log)
import FFI (currentTime)
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- Clock Types
--------------------------------------------------------------------------------

-- Types of events in the protocol sequence
data EventType
  = JitoSOLOracleUpdate
  | GatewayRebase
  | FeelsOracleUpdate
  | RiskCalculation
  | ParameterUpdate
  | FeeUpdate
  | POLPlacement
  | TermExpiryCheck         -- Check and process term expiries
  | PositionMaturityCheck
  | LendingBookMatching
  | POLMetricsUpdate
  | TokenCreationProcessing
  | SystemHealthCheck
  | UserTransactions

derive instance eqEventType :: Eq EventType

instance showEventType :: Show EventType where
  show JitoSOLOracleUpdate = "JitoSOL Oracle Update"
  show GatewayRebase = "JitoSOL/FeelsSOL Rebase"
  show FeelsOracleUpdate = "Feels Oracle Update"
  show RiskCalculation = "Risk Calculation"
  show ParameterUpdate = "Parameter Update"
  show FeeUpdate = "Fee Update"
  show POLPlacement = "POL Placement Update"
  show TermExpiryCheck = "Term Expiry Check"
  show PositionMaturityCheck = "Position Maturity Check"
  show LendingBookMatching = "Lending Book Matching"
  show POLMetricsUpdate = "POL Metrics Update"
  show TokenCreationProcessing = "Token Creation Processing"
  show SystemHealthCheck = "System Health Check"
  show UserTransactions = "User Transactions"

-- Result of executing an event
type EventResult =
  { eventType :: EventType
  , success :: Boolean
  , message :: String
  , timestamp :: Number
  , gasUsed :: Maybe Number
  }

-- Record of a block's events
type BlockEvent =
  { blockHeight :: Int
  , timestamp :: Number
  , events :: Array EventResult
  , userTransactionCount :: Int
  , totalGasUsed :: Number
  }

-- Protocol sequence configuration
type ProtocolSequence =
  { events :: Array EventType              -- Ordered list of events to execute
  , allowUserTransactions :: Boolean       -- Whether to allow user txs after sequence
  , maxEventsPerBlock :: Int              -- Maximum protocol events per block
  }

-- Clock state
type ClockState =
  { currentBlock :: Int                    -- Current block height
  , eventHistory :: Array BlockEvent       -- History of block events
  , sequence :: ProtocolSequence           -- Current protocol sequence
  , lastTickTime :: Number                 -- Timestamp of last tick
  , isPaused :: Boolean                    -- Whether the clock is paused
  , eventHandlers :: EventHandlers         -- Registered event handlers
  }

-- Event handler functions
type EventHandlers =
  { onJitoSOLOracleUpdate :: Maybe (Int -> Effect EventResult)
  , onGatewayRebase :: Maybe (Int -> Effect EventResult)
  , onFeelsOracleUpdate :: Maybe (Int -> Effect EventResult)
  , onRiskCalculation :: Maybe (Int -> Effect EventResult)
  , onParameterUpdate :: Maybe (Int -> Effect EventResult)
  , onFeeUpdate :: Maybe (Int -> Effect EventResult)
  , onPOLPlacement :: Maybe (Int -> Effect EventResult)
  , onTermExpiryCheck :: Maybe (Int -> Effect EventResult)
  , onPositionMaturityCheck :: Maybe (Int -> Effect EventResult)
  , onLendingBookMatching :: Maybe (Int -> Effect EventResult)
  , onPOLMetricsUpdate :: Maybe (Int -> Effect EventResult)
  , onTokenCreationProcessing :: Maybe (Int -> Effect EventResult)
  , onSystemHealthCheck :: Maybe (Int -> Effect EventResult)
  , onUserTransactions :: Maybe (Int -> Effect EventResult)
  }

-- Main clock type
type Clock = Ref ClockState

--------------------------------------------------------------------------------
-- Default Configuration
--------------------------------------------------------------------------------

-- Default protocol sequence
defaultSequence :: ProtocolSequence
defaultSequence =
  { events: 
      [ JitoSOLOracleUpdate      -- 1. Update JitoSOL/SOL price
      , GatewayRebase           -- 2. Rebase JitoSOL/FeelsSOL
      , FeelsOracleUpdate       -- 3. Update Feels oracle
      , RiskCalculation         -- 4. Calculate system risk
      , ParameterUpdate         -- 5. Update system parameters
      , FeeUpdate              -- 6. Update fees based on risk
      , POLMetricsUpdate        -- 7. Update POL metrics
      , POLPlacement           -- 8. Update POL positions
      , TermExpiryCheck         -- 9. Check and roll expired term positions
      , PositionMaturityCheck   -- 10. Check and process matured positions
      , LendingBookMatching     -- 11. Execute lending book matches
      , TokenCreationProcessing -- 12. Process pending token creations
      , SystemHealthCheck       -- 13. Check system invariants
      , UserTransactions        -- 14. Process user transactions
      ]
  , allowUserTransactions: true
  , maxEventsPerBlock: 15
  }

-- Empty event handlers
emptyHandlers :: EventHandlers
emptyHandlers =
  { onJitoSOLOracleUpdate: Nothing
  , onGatewayRebase: Nothing
  , onFeelsOracleUpdate: Nothing
  , onRiskCalculation: Nothing
  , onParameterUpdate: Nothing
  , onFeeUpdate: Nothing
  , onPOLPlacement: Nothing
  , onTermExpiryCheck: Nothing
  , onPositionMaturityCheck: Nothing
  , onLendingBookMatching: Nothing
  , onPOLMetricsUpdate: Nothing
  , onTokenCreationProcessing: Nothing
  , onSystemHealthCheck: Nothing
  , onUserTransactions: Nothing
  }

--------------------------------------------------------------------------------
-- Clock Initialization
--------------------------------------------------------------------------------

-- Initialize a new clock
initClock :: Effect Clock
initClock = do
  now <- currentTime
  let initialState =
        { currentBlock: 0
        , eventHistory: []
        , sequence: defaultSequence
        , lastTickTime: now
        , isPaused: false
        , eventHandlers: emptyHandlers
        }
  new initialState

--------------------------------------------------------------------------------
-- Clock Operations
--------------------------------------------------------------------------------

-- Execute one tick of the clock (advance one block)
tick :: Clock -> Effect (Either ProtocolError BlockEvent)
tick clockRef = do
  state <- read clockRef
  
  if state.isPaused
    then pure $ Left $ SystemError "Clock is paused"
    else do
      now <- currentTime
      let newBlockHeight = state.currentBlock + 1
      
      -- Log block start
      log $ "\n=== Block " <> show newBlockHeight <> " ===" 
      
      -- Execute protocol sequence
      eventResults <- executeProtocolSequence state.eventHandlers state.sequence newBlockHeight
      
      -- Count user transactions (would come from actual tx processing)
      let userTxCount = 0  -- Placeholder - would be set by user tx handler
          totalGas = calculateTotalGas eventResults
      
      -- Create block event record
      let blockEvent =
            { blockHeight: newBlockHeight
            , timestamp: now
            , events: eventResults
            , userTransactionCount: userTxCount
            , totalGasUsed: totalGas
            }
      
      -- Update state
      _ <- modify_ (\s -> s 
        { currentBlock = newBlockHeight
        , eventHistory = take 1000 (blockEvent : s.eventHistory)  -- Keep last 1000 blocks
        , lastTickTime = now
        }) clockRef
      
      -- Log block completion
      log $ "Block " <> show newBlockHeight <> " completed with " <> 
            show (length eventResults) <> " events"
      
      pure $ Right blockEvent

-- Execute the protocol sequence for a block
executeProtocolSequence :: EventHandlers -> ProtocolSequence -> Int -> Effect (Array EventResult)
executeProtocolSequence handlers sequence blockHeight = do
  let executeEvent eventType = case eventType of
        JitoSOLOracleUpdate -> executeHandler handlers.onJitoSOLOracleUpdate eventType blockHeight
        GatewayRebase -> executeHandler handlers.onGatewayRebase eventType blockHeight
        FeelsOracleUpdate -> executeHandler handlers.onFeelsOracleUpdate eventType blockHeight
        RiskCalculation -> executeHandler handlers.onRiskCalculation eventType blockHeight
        ParameterUpdate -> executeHandler handlers.onParameterUpdate eventType blockHeight
        FeeUpdate -> executeHandler handlers.onFeeUpdate eventType blockHeight
        POLPlacement -> executeHandler handlers.onPOLPlacement eventType blockHeight
        TermExpiryCheck -> executeHandler handlers.onTermExpiryCheck eventType blockHeight
        PositionMaturityCheck -> executeHandler handlers.onPositionMaturityCheck eventType blockHeight
        LendingBookMatching -> executeHandler handlers.onLendingBookMatching eventType blockHeight
        POLMetricsUpdate -> executeHandler handlers.onPOLMetricsUpdate eventType blockHeight
        TokenCreationProcessing -> executeHandler handlers.onTokenCreationProcessing eventType blockHeight
        SystemHealthCheck -> executeHandler handlers.onSystemHealthCheck eventType blockHeight
        UserTransactions -> executeHandler handlers.onUserTransactions eventType blockHeight
  
  -- Execute each event in sequence
  traverse executeEvent sequence.events

-- Execute a single event handler
executeHandler :: Maybe (Int -> Effect EventResult) -> EventType -> Int -> Effect EventResult
executeHandler maybeHandler eventType blockHeight = do
  now <- currentTime
  case maybeHandler of
    Nothing -> 
      -- No handler registered, return skipped result
      pure { eventType
           , success: true
           , message: "No handler registered (skipped)"
           , timestamp: now
           , gasUsed: Nothing
           }
    Just handler -> do
      -- Execute the handler
      result <- handler blockHeight
      -- Log the event execution
      log $ "  " <> show eventType <> ": " <> result.message
      pure result

-- Calculate total gas used in events
calculateTotalGas :: Array EventResult -> Number
calculateTotalGas events =
  let gasValues = map (\e -> case e.gasUsed of
                              Just gas -> gas
                              Nothing -> 0.0) events
  in sum gasValues
  where
    sum = foldl (+) 0.0

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get current block height
getCurrentBlock :: Clock -> Effect Int
getCurrentBlock clockRef = do
  state <- read clockRef
  pure state.currentBlock

-- Get event history (most recent first)
getEventHistory :: Clock -> Int -> Effect (Array BlockEvent)
getEventHistory clockRef limit = do
  state <- read clockRef
  pure $ take limit state.eventHistory

--------------------------------------------------------------------------------
-- Event Handler Registration
--------------------------------------------------------------------------------

-- Set JitoSOL oracle update handler
setOracleUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setOracleUpdateFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onJitoSOLOracleUpdate = Just handler } }) clockRef

-- Set gateway rebase handler
setGatewayRebaseFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setGatewayRebaseFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onGatewayRebase = Just handler } }) clockRef

-- Set Feels oracle update handler
-- setFeelsOracleUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
-- setFeelsOracleUpdateFn clockRef handler = 
--   modify_ (\s -> s { eventHandlers = s.eventHandlers { onFeelsOracleUpdate = Just handler } }) clockRef

-- Set risk calculation handler
setRiskCalculationFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setRiskCalculationFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onRiskCalculation = Just handler } }) clockRef

-- Set parameter update handler
setParameterUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setParameterUpdateFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onParameterUpdate = Just handler } }) clockRef

-- Set fee update handler
setFeeUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setFeeUpdateFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onFeeUpdate = Just handler } }) clockRef

-- Set POL placement update handler
setPOLUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setPOLUpdateFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onPOLPlacement = Just handler } }) clockRef

-- Set term expiry check handler
setTermExpiryCheckFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setTermExpiryCheckFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onTermExpiryCheck = Just handler } }) clockRef

-- Set position maturity check handler
setPositionMaturityCheckFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setPositionMaturityCheckFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onPositionMaturityCheck = Just handler } }) clockRef

-- Set lending book matching handler
setLendingBookMatchingFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setLendingBookMatchingFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onLendingBookMatching = Just handler } }) clockRef

-- Set POL metrics update handler
setPOLMetricsUpdateFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setPOLMetricsUpdateFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onPOLMetricsUpdate = Just handler } }) clockRef

-- Set token creation processing handler
setTokenCreationProcessingFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setTokenCreationProcessingFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onTokenCreationProcessing = Just handler } }) clockRef

-- Set system health check handler
setSystemHealthCheckFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
setSystemHealthCheckFn clockRef handler = 
  modify_ (\s -> s { eventHandlers = s.eventHandlers { onSystemHealthCheck = Just handler } }) clockRef

-- Set user transactions handler
-- setUserTransactionsFn :: Clock -> (Int -> Effect EventResult) -> Effect Unit
-- setUserTransactionsFn clockRef handler = 
--   modify_ (\s -> s { eventHandlers = s.eventHandlers { onUserTransactions = Just handler } }) clockRef
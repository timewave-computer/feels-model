-- Core Simulation State and Execution Engine
-- Handles simulation initialization, state management, and main execution loops
module Simulation.Engine
  ( SimulationState
  , initSimulation
  , initSimulationWithLendingBook
  , executeSimulation
  , runSimulation
  , runSimulationWithLendingBook
  , getSimulationStats
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array ((:), length, filter, take, drop, range, head, tail, find, snoc)
import Data.Foldable (sum, foldl)
import Data.Functor (map)
import Data.Traversable (traverse, sequence)
import Data.Int as Int
import Data.Number ((%))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify)
import Effect.Random (random, randomInt)
import Effect.Console (log)
import Data.Ord (max, min)

-- Core system imports
import Token (TokenType(..), TokenAmount)
import LendingRecord (LendingRecord, LendingSide(..), LendingTerms(..), UnbondingPeriod(..), LendingStatus(..), createLenderRecord, createBorrowerRecord)
import LendingBook (LendingBook, initLendingBook, createLendOffer, takeLoan, matchAndExecute, getTotalLiquidity)
import Gateway (GatewayState, initGateway, enterSystem, exitSystem)
import SyntheticSOL (SyntheticSOLState, initSyntheticSOL)
import Utils (formatAmount, formatPercentage)
import FFI (currentTime, sqrt, log, cos) as FFI
import POL (POLState, initPOL)
import Oracle (Oracle, PriceObservation, observeMarket, observeMarketWithBlock, initOracle, takeMarketSnapshot)

-- Import from our new modules
import Simulation.Agents (SimulatedAccount, generateAccounts)
import Simulation.Market (SimulationConfig, generateMarketScenario)
import Simulation.Actions (TradingAction(..), generateTradingSequence, getRecentlyCreatedTokens)
import Simulation.Analysis (SimulationResults, calculateResults)

--------------------------------------------------------------------------------
-- Simulation State Types
--------------------------------------------------------------------------------

-- Simulation state
type SimulationState =
  { accounts :: Array SimulatedAccount
  , lendingBook :: LendingBook
  , gateway :: GatewayState
  , oracle :: Oracle
  , currentBlock :: Int
  , currentPrice :: Number
  , priceHistory :: Array PriceObservation
  , actionHistory :: Array TradingAction
  , nextPositionId :: Int
  }

--------------------------------------------------------------------------------
-- Simulation Setup
--------------------------------------------------------------------------------

-- Initialize simulation with config (creates new lending book)
initSimulation :: SimulationConfig -> Effect SimulationState
initSimulation config = do
  lendingBook <- initLendingBook
  pol <- initPOL
  oracle <- initOracle lendingBook pol
  initSimulationWithLendingBook config lendingBook oracle

-- Initialize simulation with existing lending book for UI integration
initSimulationWithLendingBook :: SimulationConfig -> LendingBook -> Oracle -> Effect SimulationState
initSimulationWithLendingBook config existingLendingBook oracle = do
  accounts <- generateAccounts { numAccounts: config.numAccounts, accountProfiles: config.accountProfiles }
  pol <- initPOL
  -- Create a simple oracle function for simulation
  let priceOracle = pure config.initialJitoSOLPrice
  syntheticSOL <- initSyntheticSOL priceOracle
  -- Create empty user accounts for simulation
  userAccounts <- new []
  -- Initialize gateway with proper parameters (simplified for simulation)
  let gateway = { syntheticSOL: syntheticSOL
                , entryFee: 0.001
                , exitFee: 0.002  
                , polAllocationRate: 0.1
                , userAccounts: userAccounts
                , polState: pol
                }
  
  pure { accounts: accounts
       , lendingBook: existingLendingBook
       , gateway: gateway
       , oracle: oracle
       , currentBlock: 0
       , currentPrice: config.initialJitoSOLPrice
       , priceHistory: []
       , actionHistory: []
       , nextPositionId: 1
       }

--------------------------------------------------------------------------------
-- Simulation Execution
--------------------------------------------------------------------------------

-- Run the complete simulation
runSimulation :: SimulationConfig -> Effect SimulationResults
runSimulation config = do
  log $ "Starting simulation: " <> show config.scenario
  initialState <- initSimulation config
  finalState <- executeSimulation config initialState
  results <- calculateResults config finalState
  log $ "Simulation completed. Total volume: " <> formatAmount results.totalVolume
  pure results

-- Run simulation with existing lending book (for UI integration)
runSimulationWithLendingBook :: SimulationConfig -> LendingBook -> Oracle -> Effect SimulationResults
runSimulationWithLendingBook config existingLendingBook oracle = do
  log $ "Starting simulation with existing lending book"
  log $ "Market scenario: " <> show config.scenario
  log $ "Price volatility: " <> show config.priceVolatility
  initialState <- initSimulationWithLendingBook config existingLendingBook oracle
  finalState <- executeSimulation config initialState
  results <- calculateResults config finalState
  log $ "Simulation completed. Total volume: " <> formatAmount results.totalVolume
  pure results

-- Execute simulation step by step
executeSimulation :: SimulationConfig -> SimulationState -> Effect SimulationState
executeSimulation config initialState = do
  -- First, ensure block 0 is observed in the oracle
  _ <- observeMarketWithBlock initialState.oracle 0
  log "Simulation: Observed block 0"
  
  -- Then execute blocks 1 through simulationBlocks
  finalState <- foldl executeBlock (pure initialState) (range 1 config.simulationBlocks)
  
  -- After simulation, ensure all blocks were observed
  -- This is important because some blocks might not have trading activity
  log "Simulation: Ensuring all blocks have observations..."
  _ <- observeMarketWithBlock finalState.oracle config.simulationBlocks
  
  pure finalState
  where
    executeBlock stateEffect blockNum = do
      state <- stateEffect
      executeSimulationBlock config state blockNum

-- Execute a single simulation block
executeSimulationBlock :: SimulationConfig -> SimulationState -> Int -> Effect SimulationState
executeSimulationBlock config state blockNum = do
  -- Apply market dynamics to update base prices
  marketMovement <- generateMarketScenario config blockNum
  -- when (blockNum `mod` 20 == 0) $ do
  --   log $ "Block " <> show blockNum <> ": Market movement " <> show (marketMovement * 100.0) <> "%"
  -- Log block execution
  -- when (blockNum <= 5 || Int.toNumber blockNum `mod` 10.0 == 0.0) $ do
  --   log $ "Executing simulation block " <> show blockNum
  
  -- Generate market-influenced price movement for this block
  priceMovement <- generateMarketScenario config blockNum
  
  -- Update state with market-influenced price
  let marketInfluencedState = state { currentPrice = state.currentPrice * (1.0 + priceMovement) }
  
  -- Generate trading actions for this block
  actions <- generateTradingSequence config { accounts: marketInfluencedState.accounts
                                           , currentBlock: blockNum
                                           , actionHistory: marketInfluencedState.actionHistory
                                           , oracle: marketInfluencedState.oracle
                                           }
  
  -- Execute each action
  newState <- foldl executeAction (pure marketInfluencedState) actions
  
  -- Get price observations from actual market activity with block number
  _ <- observeMarketWithBlock newState.oracle blockNum  -- Update oracle with block number
  snapshot <- takeMarketSnapshot newState.oracle
  let priceObservations = snapshot.priceObservations
  
  -- Calculate current JitoSOL/FeelsSOL price from observations (if available)
  let jitoSOLObs = find (\obs -> obs.baseAsset == JitoSOL) priceObservations
      currentPriceFromMarket = case jitoSOLObs of
        Just obs -> obs.impliedPrice
        Nothing -> newState.currentPrice
  
  pure $ newState { currentBlock = blockNum
                  , currentPrice = currentPriceFromMarket
                  , priceHistory = priceObservations <> state.priceHistory
                  , actionHistory = state.actionHistory <> actions
                  }

-- Execute a single trading action
executeAction :: Effect SimulationState -> TradingAction -> Effect SimulationState
executeAction stateEffect action = do
  state <- stateEffect
  case action of
    EnterProtocol userId amount asset -> do
      -- log $ "Executing: " <> show action
      -- Update account balances to simulate gateway entry
      -- Deduct JitoSOL and add FeelsSOL (1:1 exchange rate for simplicity)
      let updatedAccounts1 = updateAccountBalances state.accounts userId (-amount) JitoSOL
      let updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount FeelsSOL
      -- Fees are automatically collected by the protocol
      pure state { accounts = updatedAccounts2 }
    
    ExitProtocol userId amount asset -> do
      -- log $ "Executing: " <> show action
      -- Update account balances to simulate gateway exit
      -- Deduct FeelsSOL and add JitoSOL (1:1 exchange rate for simplicity, minus fees)
      let feelsAmount = amount
          jitoAmount = amount * 0.998  -- 0.2% exit fee
          updatedAccounts1 = updateAccountBalances state.accounts userId (-feelsAmount) FeelsSOL
          updatedAccounts2 = updateAccountBalances updatedAccounts1 userId jitoAmount JitoSOL
      pure state { accounts = updatedAccounts2 }
    
    CreateToken userId ticker name -> do
      -- log $ "Executing: " <> show action
      -- Token creation is handled by the protocol, no balance changes needed
      pure state
      
    CreateLendOffer userId lendAsset lendAmount collateralAsset collateralAmount terms targetToken -> do
      -- log $ "Executing: " <> show action
      -- Create lending offer in the book
      result <- createLendOffer state.lendingBook userId lendAsset lendAmount collateralAsset collateralAmount terms
      case result of
        Right _ -> 
          pure state { nextPositionId = state.nextPositionId + 1 }
        Left err -> do
          -- log $ "Failed to create lend offer: " <> err
          pure state
    
    TakeLoan userId borrowAsset borrowAmount collateralAsset collateralAmount terms -> do
      -- log $ "Executing: " <> show action
      -- Take loan from the book
      result <- takeLoan state.lendingBook userId borrowAsset borrowAmount collateralAsset collateralAmount terms
      case result of
        Right match -> do
          -- Update balances for successful loan
          let updatedAccounts = updateAccountBalances state.accounts userId borrowAmount borrowAsset
          pure state { accounts = updatedAccounts }
        Left err -> do
          -- log $ "Failed to take loan: " <> err
          pure state
    
    ClosePosition userId positionId -> do
      -- Position closing is more complex and would require tracking position state
      pure state
    
    WaitBlocks blocks -> 
      pure state

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- Update account balances for a specific user and token
updateAccountBalances :: Array SimulatedAccount -> String -> Number -> TokenType -> Array SimulatedAccount
updateAccountBalances accounts userId amount token =
  map updateAccount accounts
  where
    updateAccount acc
      | acc.id == userId = case token of
          JitoSOL -> acc { jitoSOLBalance = max 0.0 (acc.jitoSOLBalance + amount) }
          FeelsSOL -> acc { feelsSOLBalance = max 0.0 (acc.feelsSOLBalance + amount) }
          Token _ -> acc  -- Token balances not tracked in SimulatedAccount for simplicity
      | otherwise = acc

-- Get simulation statistics
getSimulationStats :: SimulationState -> Effect String  
getSimulationStats state = do
  let totalLiquidity = 0.0  -- TODO: Fix when getTotalLiquidity signature is clarified
  
  let stats = "=== Simulation Statistics ===\n" <>
              "Current Block: " <> show state.currentBlock <> "\n" <>
              "Current Price: " <> formatAmount state.currentPrice <> "\n" <>
              "Total Accounts: " <> show (length state.accounts) <> "\n" <>
              "Total Actions: " <> show (length state.actionHistory) <> "\n" <>
              "Protocol TVL: " <> formatAmount totalLiquidity
  
  pure stats
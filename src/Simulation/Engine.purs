-- Core Simulation State and Execution Engine
-- Handles simulation initialization, state management, and main execution loops
module Simulation.Engine
  ( SimulationState
  , initSimulation
  , initSimulationWithPoolRegistry
  , executeSimulation
  , runSimulation
  , runSimulationWithPoolRegistry
  , getSimulationStats
  ) where

import Prelude
import Data.Array (length, range, (:))
import Data.Foldable (sum, foldl)
import Effect (Effect)
import Effect.Console (log)

-- Core system imports
import Token (TokenType(..))
import PoolRegistry (PoolRegistry, initPoolRegistry)
import Gateway (GatewayState, initGateway)
import Accounts (initAccountRegistry)
import Utils (formatAmount)
import POL (initPOL)
import Oracle (Oracle, initOracle, takeMarketSnapshot)

-- Import from our new modules
import Simulation.Agents (SimulatedAccount, generateAccounts)
import Simulation.Market (SimulationConfig, generateMarketScenario)
import Simulation.Actions (TradingAction(..), generateTradingSequence)
import Simulation.Analysis (SimulationResults, calculateResults)

--------------------------------------------------------------------------------
-- Simulation State Types
--------------------------------------------------------------------------------

-- Simulation state
type SimulationState =
  { accounts :: Array SimulatedAccount
  , poolRegistry :: PoolRegistry
  , gateway :: GatewayState
  , oracle :: Oracle
  , currentBlock :: Int
  , currentPrice :: Number
  , priceHistory :: Array { price :: Number, timestamp :: Number }
  , actionHistory :: Array TradingAction
  , nextPositionId :: Int
  }

--------------------------------------------------------------------------------
-- Simulation Setup
--------------------------------------------------------------------------------

-- Initialize simulation with config (creates new lending book)
initSimulation :: SimulationConfig -> Effect SimulationState
initSimulation config = do
  poolRegistry <- initPoolRegistry
  _ <- initPOL
  oracle <- initOracle config.initialJitoSOLPrice
  initSimulationWithPoolRegistry config poolRegistry oracle

-- Initialize simulation with existing lending book for UI integration
initSimulationWithPoolRegistry :: SimulationConfig -> PoolRegistry -> Oracle -> Effect SimulationState
initSimulationWithPoolRegistry config existingPoolRegistry oracle = do
  accounts <- generateAccounts { numAccounts: config.numAccounts, accountProfiles: config.accountProfiles }
  pol <- initPOL
  -- Create a simple oracle function for simulation
  let priceOracle = pure config.initialJitoSOLPrice
  -- Create account registry for simulation
  accountRegistry <- initAccountRegistry
  -- Initialize gateway with proper parameters (simplified for simulation)
  gateway <- initGateway priceOracle 0.001 0.002 accountRegistry pol
  
  pure { accounts: accounts
       , poolRegistry: existingPoolRegistry
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
runSimulationWithPoolRegistry :: SimulationConfig -> PoolRegistry -> Oracle -> Effect SimulationResults
runSimulationWithPoolRegistry config existingPoolRegistry oracle = do
  log $ "Starting simulation with existing lending book"
  log $ "Market scenario: " <> show config.scenario
  log $ "Price volatility: " <> show config.priceVolatility
  initialState <- initSimulationWithPoolRegistry config existingPoolRegistry oracle
  finalState <- executeSimulation config initialState
  results <- calculateResults config finalState
  log $ "Simulation completed. Total volume: " <> formatAmount results.totalVolume
  pure results

-- Execute simulation step by step
executeSimulation :: SimulationConfig -> SimulationState -> Effect SimulationState
executeSimulation config initialState = do
  -- First, observe initial market state
  _ <- takeMarketSnapshot initialState.oracle
  log "Simulation: Observed initial market state"
  
  -- Then execute blocks 1 through simulationBlocks
  finalState <- foldl executeBlock (pure initialState) (range 1 config.simulationBlocks)
  
  -- After simulation, observe final market state
  log "Simulation: Observing final market state..."
  _ <- takeMarketSnapshot finalState.oracle
  
  pure finalState
  where
    executeBlock stateEffect blockNum = do
      state <- stateEffect
      executeSimulationBlock config state blockNum

-- Execute a single simulation block
executeSimulationBlock :: SimulationConfig -> SimulationState -> Int -> Effect SimulationState
executeSimulationBlock config state blockNum = do
  -- Apply market dynamics to update base prices
  _ <- generateMarketScenario config blockNum
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
  
  -- Get price observations from actual market activity
  _ <- takeMarketSnapshot newState.oracle  -- Update oracle
  marketSnapshot <- takeMarketSnapshot newState.oracle
  
  -- Use the current spot price from the market snapshot
  let currentPriceFromMarket = marketSnapshot.spot
  
  pure $ newState { currentBlock = blockNum
                  , currentPrice = currentPriceFromMarket
                  , priceHistory = { price: marketSnapshot.spot, timestamp: marketSnapshot.timestamp } : state.priceHistory
                  , actionHistory = state.actionHistory <> actions
                  }

-- Execute a single trading action
executeAction :: Effect SimulationState -> TradingAction -> Effect SimulationState
executeAction stateEffect action = do
  state <- stateEffect
  case action of
    EnterProtocol userId amount _ -> do
      -- log $ "Executing: " <> show action
      -- Update account balances to simulate gateway entry
      -- Deduct JitoSOL and add FeelsSOL (1:1 exchange rate for simplicity)
      let updatedAccounts1 = updateAccountBalances state.accounts userId (-amount) JitoSOL
      let updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount FeelsSOL
      -- Fees are automatically collected by the protocol
      pure state { accounts = updatedAccounts2 }
    
    ExitProtocol userId amount _ -> do
      -- log $ "Executing: " <> show action
      -- Update account balances to simulate gateway exit
      -- Deduct FeelsSOL and add JitoSOL (1:1 exchange rate for simplicity, minus fees)
      let feelsAmount = amount
          jitoAmount = amount * 0.998  -- 0.2% exit fee
          updatedAccounts1 = updateAccountBalances state.accounts userId (-feelsAmount) FeelsSOL
          updatedAccounts2 = updateAccountBalances updatedAccounts1 userId jitoAmount JitoSOL
      pure state { accounts = updatedAccounts2 }
    
    CreateToken _ _ _ -> do
      -- log $ "Executing: " <> show action
      -- Token creation is handled by the protocol, no balance changes needed
      pure state
      
    CreateLendOffer _ _ _ _ _ _ _ -> do
      -- For MVP, just track the offer without creating a position
      -- In the new system, positions are created through pools
      pure state
    
    TakeLoan _ _ _ _ _ _ -> do
      -- For MVP, just track the loan without creating a position
      -- In the new system, positions are created through pools
      pure state
    
    ClosePosition _ _ -> do
      -- Position closing is more complex and would require tracking position state
      pure state
    
    WaitBlocks _ -> 
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
  -- Get liquidity from all accounts
  let totalLiquidity = sum $ map (\acc -> acc.jitoSOLBalance + acc.feelsSOLBalance) state.accounts
  
  let stats = "=== Simulation Statistics ===\n" <>
              "Current Block: " <> show state.currentBlock <> "\n" <>
              "Current Price: " <> formatAmount state.currentPrice <> "\n" <>
              "Total Accounts: " <> show (length state.accounts) <> "\n" <>
              "Total Actions: " <> show (length state.actionHistory) <> "\n" <>
              "Protocol TVL: " <> formatAmount totalLiquidity
  
  pure stats
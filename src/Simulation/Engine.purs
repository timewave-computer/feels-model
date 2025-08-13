-- | Core Simulation State and Execution Engine
-- |
-- | This module implements the central simulation engine responsible for
-- | orchestrating comprehensive DeFi protocol simulations. It manages state
-- | transitions, executes trading actions, and coordinates market dynamics.
-- |
-- | Key Components:
-- | - Simulation state management with comprehensive protocol state tracking
-- | - Block-by-block execution engine with market scenario integration
-- | - Action execution system handling all protocol interactions
-- | - Oracle integration for realistic price discovery and market snapshots
-- | - Performance monitoring and statistics generation
-- |
-- | Execution Flow:
-- | 1. Initialize simulation with agents, pools, and protocol components
-- | 2. Execute simulation blocks with market scenario price movements
-- | 3. Generate and execute trading actions based on agent behavior
-- | 4. Update protocol state and capture market snapshots
-- | 5. Calculate comprehensive results and performance metrics
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

-- Core protocol and UI system imports
import Protocol.Token (TokenType(..))
import UI.PoolRegistry (PoolRegistry, initPoolRegistry)
import Protocol.FeelsSOL (FeelsSOLState, initFeelsSOL)
import UI.Account (initAccountRegistry)
import Utils (formatAmount)
import Protocol.POL (initPOL)
import Protocol.Oracle (Oracle, initOracle, takeMarketSnapshot, updatePrice)

-- Import simulation subsystem modules
import Simulation.Agent (SimulatedAccount, generateAccounts)
import Simulation.Market (SimulationConfig, generateMarketScenario)
import Simulation.Action (TradingAction(..), generateTradingSequence)
import Simulation.Analysis (SimulationResults, calculateResults)

--------------------------------------------------------------------------------
-- SIMULATION STATE DATA STRUCTURES
--------------------------------------------------------------------------------
-- Comprehensive state management for complex DeFi protocol simulations

-- | Complete simulation state tracking all protocol components and history
-- | Maintains comprehensive view of simulation progress and market conditions
type SimulationState =
  { accounts :: Array SimulatedAccount          -- All simulated user accounts with balances
  , poolRegistry :: PoolRegistry                -- Pool registry with all active pools
  , feelsSOL :: FeelsSOLState                  -- FeelsSOL protocol state and parameters
  , oracle :: Oracle                           -- Price oracle for market data feeds
  , currentBlock :: Int                        -- Current simulation block number
  , currentPrice :: Number                     -- Current market price from oracle
  , priceHistory :: Array { price :: Number, timestamp :: Number }  -- Historical price data
  , actionHistory :: Array TradingAction       -- Complete history of all executed actions
  , nextPositionId :: Int                      -- Counter for generating unique position IDs
  }

--------------------------------------------------------------------------------
-- SIMULATION INITIALIZATION SYSTEM
--------------------------------------------------------------------------------
-- Comprehensive setup functions for creating realistic simulation environments

-- | Initialize complete simulation environment with fresh state
-- | Creates new pool registry, oracle, and all protocol components from scratch
initSimulation :: SimulationConfig -> Effect SimulationState
initSimulation config = do
  -- Initialize core protocol components
  poolRegistry <- initPoolRegistry
  _ <- initPOL  -- Initialize Protocol-Owned Liquidity system
  oracle <- initOracle config.initialJitoSOLPrice
  
  -- Use the comprehensive initialization with fresh components
  initSimulationWithPoolRegistry config poolRegistry oracle

-- | Initialize simulation with existing pool registry for UI integration
-- | Enables seamless integration with existing protocol state from UI interactions
initSimulationWithPoolRegistry :: SimulationConfig -> PoolRegistry -> Oracle -> Effect SimulationState
initSimulationWithPoolRegistry config existingPoolRegistry oracle = do
  -- Generate diverse population of simulated accounts
  accounts <- generateAccounts { numAccounts: config.numAccounts, accountProfiles: config.accountProfiles }
  
  -- Initialize Protocol-Owned Liquidity system
  pol <- initPOL
  
  -- Create price oracle function for FeelsSOL integration
  let priceOracle = pure config.initialJitoSOLPrice
  
  -- Initialize account registry for user management
  accountRegistry <- initAccountRegistry
  
  -- Initialize FeelsSOL protocol with realistic parameters
  -- Entry fee: 0.1%, Exit fee: 0.2% (typical DeFi protocol rates)
  feelsSOL <- initFeelsSOL priceOracle 0.001 0.002
  
  -- Assemble complete initial simulation state
  pure { accounts: accounts
       , poolRegistry: existingPoolRegistry
       , feelsSOL: feelsSOL
       , oracle: oracle
       , currentBlock: 0                    -- Start at genesis block
       , currentPrice: config.initialJitoSOLPrice
       , priceHistory: []                   -- Empty initial price history
       , actionHistory: []                  -- Empty initial action history
       , nextPositionId: 1                  -- Start position ID counter
       }

--------------------------------------------------------------------------------
-- SIMULATION EXECUTION ORCHESTRATION
--------------------------------------------------------------------------------
-- High-level functions for running complete simulation scenarios

-- | Execute complete simulation scenario from initialization to results analysis
-- | Creates fresh environment and runs full simulation with comprehensive logging
runSimulation :: SimulationConfig -> Effect SimulationResults
runSimulation config = do
  log $ "Starting simulation: " <> show config.scenario
  log $ "Simulating " <> show config.numAccounts <> " accounts over " <> show config.simulationBlocks <> " blocks"
  
  -- Initialize simulation environment
  initialState <- initSimulation config
  
  -- Execute complete simulation
  finalState <- executeSimulation config initialState
  
  -- Calculate comprehensive results and performance metrics
  results <- calculateResults config finalState
  
  log $ "Simulation completed successfully"
  log $ "Total volume: " <> formatAmount results.totalVolume
  log $ "Price change: " <> show (results.priceChange * 100.0) <> "%"
  log $ "Volatility: " <> show (results.volatility * 100.0) <> "%"
  
  pure results

-- | Run simulation with existing pool registry for UI integration
-- | Enables running simulations against existing protocol state from UI interactions
runSimulationWithPoolRegistry :: SimulationConfig -> PoolRegistry -> Oracle -> Effect SimulationResults
runSimulationWithPoolRegistry config existingPoolRegistry oracle = do
  log $ "Starting simulation with existing protocol state"
  log $ "Market scenario: " <> show config.scenario
  log $ "Price volatility: " <> show config.priceVolatility
  log $ "Action frequency: " <> show config.actionFrequency
  
  -- Initialize simulation with existing state
  initialState <- initSimulationWithPoolRegistry config existingPoolRegistry oracle
  
  -- Execute simulation maintaining existing protocol state
  finalState <- executeSimulation config initialState
  
  -- Generate comprehensive analysis results
  results <- calculateResults config finalState
  
  log $ "Simulation completed with existing state integration"
  log $ "Total volume: " <> formatAmount results.totalVolume
  log $ "Active positions: " <> show results.activePositions
  log $ "Protocol TVL: " <> formatAmount results.protocolTVL
  
  pure results

--------------------------------------------------------------------------------
-- CORE SIMULATION EXECUTION ENGINE
--------------------------------------------------------------------------------
-- Block-by-block simulation execution with market dynamics and state transitions

-- | Execute complete simulation from initial to final state
-- | Processes all simulation blocks with market dynamics and agent interactions
executeSimulation :: SimulationConfig -> SimulationState -> Effect SimulationState
executeSimulation config initialState = do
  -- Establish baseline market conditions
  _ <- takeMarketSnapshot initialState.oracle
  log "Simulation: Established initial market conditions"
  
  -- Execute simulation blocks sequentially with state accumulation
  finalState <- foldl executeBlock (pure initialState) (range 1 config.simulationBlocks)
  
  -- Capture final market state for analysis
  log "Simulation: Capturing final market state for analysis"
  _ <- takeMarketSnapshot finalState.oracle
  
  log $ "Simulation execution completed: " <> show config.simulationBlocks <> " blocks processed"
  pure finalState
  where
    -- | Execute individual simulation block with state transformation
    executeBlock stateEffect blockNum = do
      state <- stateEffect
      executeSimulationBlock config state blockNum

-- | Execute single simulation block with comprehensive market dynamics
-- | Processes market scenario, generates actions, executes trades, and updates state
executeSimulationBlock :: SimulationConfig -> SimulationState -> Int -> Effect SimulationState
executeSimulationBlock config state blockNum = do
  -- Generate scenario-specific market price movement for this block
  priceMovement <- generateMarketScenario config blockNum
  
  -- Apply market scenario to current price for realistic market dynamics
  let marketInfluencedState = state { currentPrice = state.currentPrice * (1.0 + priceMovement) }
  
  -- Generate agent trading actions based on current market conditions
  -- Actions are influenced by market state, agent profiles, and historical activity
  actions <- generateTradingSequence config { accounts: marketInfluencedState.accounts
                                           , currentBlock: blockNum
                                           , actionHistory: marketInfluencedState.actionHistory
                                           , oracle: marketInfluencedState.oracle
                                           }
  
  -- Execute all generated trading actions sequentially
  newState <- foldl executeAction (pure marketInfluencedState) actions
  
  -- Update oracle with current market activity and price discovery
  -- First update the oracle's price to record it in history
  _ <- updatePrice marketInfluencedState.currentPrice newState.oracle
  
  -- Then take the market snapshot
  marketSnapshot <- takeMarketSnapshot newState.oracle
  
  -- Extract official price from market snapshot for state consistency
  let currentPriceFromMarket = marketSnapshot.spot
  
  -- Update simulation state with block results and historical tracking
  pure $ newState { currentBlock = blockNum
                  , currentPrice = currentPriceFromMarket
                  , priceHistory = { price: marketSnapshot.spot, timestamp: marketSnapshot.timestamp } : state.priceHistory
                  , actionHistory = state.actionHistory <> actions  -- Accumulate all actions
                  }

--------------------------------------------------------------------------------
-- INDIVIDUAL ACTION EXECUTION ENGINE
--------------------------------------------------------------------------------
-- Comprehensive execution system for all trading action types

-- | Execute individual trading action with appropriate state updates
-- | Handles all action types with realistic balance updates and fee calculations
executeAction :: Effect SimulationState -> TradingAction -> Effect SimulationState
executeAction stateEffect action = do
  state <- stateEffect
  case action of
    -- Protocol entry: JitoSOL → FeelsSOL conversion with entry fees
    EnterProtocol userId amount _ -> do
      -- Update account balances for protocol entry
      -- Deduct JitoSOL and add equivalent FeelsSOL (1:1 base rate)
      let updatedAccounts1 = updateAccountBalances state.accounts userId (-amount) JitoSOL
      let updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount FeelsSOL
      -- Entry fees are collected automatically by the FeelsSOL protocol
      pure state { accounts = updatedAccounts2 }
    
    -- Protocol exit: FeelsSOL → JitoSOL conversion with exit fees
    ExitProtocol userId amount _ -> do
      -- Update account balances for protocol exit with fee deduction
      let feelsAmount = amount
          jitoAmount = amount * 0.998  -- 0.2% exit fee (realistic DeFi rate)
          updatedAccounts1 = updateAccountBalances state.accounts userId (-feelsAmount) FeelsSOL
          updatedAccounts2 = updateAccountBalances updatedAccounts1 userId jitoAmount JitoSOL
      pure state { accounts = updatedAccounts2 }
    
    -- Token creation: Administrative action with no direct balance impact
    CreateToken _ _ _ -> do
      -- Token creation handled by protocol governance, no user balance changes
      pure state
      
    -- Lending offer creation: Track action without immediate balance changes
    CreateLendOffer _ _ _ _ _ _ _ -> do
      -- Position creation handled through pool system integration
      -- Balance changes occur when positions are actually filled
      pure state
    
    -- Loan taking: Track action without immediate balance changes
    TakeLoan _ _ _ _ _ _ -> do
      -- Loan execution handled through pool system integration
      -- Balance changes occur when loans are actually executed
      pure state
    
    -- Position closing: Complex state management for position lifecycle
    ClosePosition _ _ -> do
      -- Position closing requires comprehensive state tracking
      -- Implementation depends on position management system integration
      pure state
    
    -- Wait action: No-op action for timing and sequencing
    WaitBlocks _ -> 
      pure state  -- No state changes for wait actions

--------------------------------------------------------------------------------
-- SIMULATION UTILITY FUNCTIONS
--------------------------------------------------------------------------------
-- Supporting functions for state management and performance monitoring

-- | Update account balances for specific user and token type with safety checks
-- | Ensures balances never go negative and handles all supported token types
updateAccountBalances :: Array SimulatedAccount -> String -> Number -> TokenType -> Array SimulatedAccount
updateAccountBalances accounts userId amount token =
  map updateAccount accounts
  where
    updateAccount acc
      | acc.id == userId = case token of
          -- Update JitoSOL balance with non-negative constraint
          JitoSOL -> acc { jitoSOLBalance = max 0.0 (acc.jitoSOLBalance + amount) }
          -- Update FeelsSOL balance with non-negative constraint
          FeelsSOL -> acc { feelsSOLBalance = max 0.0 (acc.feelsSOLBalance + amount) }
          -- Custom token balances not tracked in SimulatedAccount for simulation simplicity
          Token _ -> acc
      | otherwise = acc  -- No changes for other accounts

-- | Generate comprehensive simulation statistics and performance summary
-- | Provides real-time monitoring of simulation progress and key metrics
getSimulationStats :: SimulationState -> Effect String  
getSimulationStats state = do
  -- Calculate total liquidity across all simulated accounts
  let totalLiquidity = sum $ map (\acc -> acc.jitoSOLBalance + acc.feelsSOLBalance) state.accounts
  
  -- Generate comprehensive statistics report
  let stats = "=== Comprehensive Simulation Statistics ===\n" <>
              "Current Block: " <> show state.currentBlock <> "\n" <>
              "Current Market Price: " <> formatAmount state.currentPrice <> "\n" <>
              "Total Simulated Accounts: " <> show (length state.accounts) <> "\n" <>
              "Total Executed Actions: " <> show (length state.actionHistory) <> "\n" <>
              "Protocol Total Value Locked: " <> formatAmount totalLiquidity <> "\n" <>
              "Price History Points: " <> show (length state.priceHistory) <> "\n" <>
              "Next Position ID: " <> show state.nextPositionId
  
  pure stats
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
  ( -- Re-exports from Agent module
    module Simulation.Agent
  -- Re-exports from Scenario module  
  , module Simulation.Scenario
  -- Re-exports from Action module
  , module Simulation.Action
  -- Re-exports from Analysis module
  , module Simulation.Analysis
  -- Core simulation types and functions
  , SimulationState
  , initSimulation
  , initSimulationWithPoolRegistry
  , executeSimulation
  , runSimulation
  , runSimulationWithPoolRegistry
  , getSimulationStats
  -- Protocol engine functions
  , runProtocolSimulation
  , executeAgentOrders
  ) where

import Prelude
import Data.Array (length, range, (:))
import Data.Foldable (sum, foldl, foldM)
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, read, write)

-- Core protocol and UI system imports
import Protocol.Token (TokenType(..))
import UI.PoolRegistry (PoolRegistry, initPoolRegistry, getPool, updatePool)
import Protocol.FeelsSOLVault (FeelsSOLState, createFeelsSOLVault, FeelsSOLStrategy)
import UI.Account (AccountRegistry, initAccountRegistry, updateChainAccountBalance, getChainAccountBalance)
import Utils (formatAmount)
import Protocol.POLVault (initPOL)
import Protocol.Oracle (Oracle, initOracle, takeMarketSnapshot, updatePrice)

-- Import simulation subsystem modules
import Simulation.Agent (AccountProfile(..), SimulatedAccount, generateAccounts)
import Simulation.Scenario (MarketScenario(..), SimulationConfig, generateMarketScenario)
import Simulation.Action (TradingAction(..), generateTradingSequence, getRecentlyCreatedTokens)
import Simulation.Analysis (SimulationResults, calculateResults)

-- Additional imports for protocol engine functionality
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)

-- Import protocol types and functions for protocol engine
import Protocol.Common (CommandResult(..))
import Protocol.PositionVault (Leverage(..))
import UI.ProtocolState (ProtocolState)
import UI.ProtocolState (ProtocolCommand(..)) as PS
import UI.Command (executeCommand)
import Protocol.Pool (swap)
import UI.Clock (runProtocolBlock)

--------------------------------------------------------------------------------
-- SIMULATION STATE DATA STRUCTURES
--------------------------------------------------------------------------------
-- Comprehensive state management for complex DeFi protocol simulations

-- | Complete simulation state tracking agents, pools, protocol state, and trading history
-- | Maintains real-time view of simulation progress and market dynamics
type SimulationState =
  { accounts :: Array SimulatedAccount          -- All simulated user accounts with balances
  , poolRegistry :: PoolRegistry                -- Pool registry with all active pools
  , feelsSOL :: FeelsSOLState                  -- FeelsSOL protocol state and parameters
  , oracle :: Oracle                           -- Price oracle for market data feeds
  , currentBlock :: Int                        -- Current simulation block number
  , currentPrice :: Number                     -- Current market price from oracle
  , priceHistory :: Array { price :: Number, timestamp :: Number }  -- Historical price data
  , actionHistory :: Array TradingAction       -- Complete history of all executed actions
  , nextPositionId :: Int                      -- Auto-incrementing counter for position creation
  , polAllocationHistory :: Array { block :: Int, timestamp :: Number, allocations :: Map.Map String Number }  -- POL allocation history by pool over time
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
  _ <- initPOL
  
  -- Initialize account registry for user management
  _ <- initAccountRegistry
  
  -- Initialize FeelsSOL vault with realistic parameters
  -- Entry fee: 0.1%, Exit fee: 0.2% (typical DeFi protocol rates)
  let initialStrategy =
        { priceOracle: pure config.initialJitoSOLPrice
        , lastOracleUpdate: 0.0
        , cachedPrice: Nothing
        , entryFee: 0.001
        , exitFee: 0.002
        , polAllocationRate: 0.25  -- Default 25% to POL
        , bufferTargetRatio: 0.01  -- Default 1% buffer target
        , jitoSOLBuffer: 0.0
        }
  feelsSOL <- createFeelsSOLVault "FeelsSOL-System" initialStrategy
  
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
       , polAllocationHistory: []           -- Empty initial POL allocation history
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
  finalState <- foldM (\state blockNum -> executeSimulationBlock config state blockNum) initialState (range 1 config.simulationBlocks)
  
  -- Capture final market state for analysis
  log "Simulation: Capturing final market state for analysis"
  _ <- takeMarketSnapshot finalState.oracle
  
  log $ "Simulation execution completed: " <> show config.simulationBlocks <> " blocks processed"
  pure finalState

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
    CreateLendOffer _ _ _ _ _ _ _ _ -> do
      -- Position creation handled through pool system integration
      -- Balance changes occur when positions are actually filled
      pure state
    
    -- Loan taking: Track action without immediate balance changes
    TakeLoan _ _ _ _ _ _ _ -> do
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

--------------------------------------------------------------------------------
-- PROTOCOL ENGINE INTEGRATION
--------------------------------------------------------------------------------
-- Agent behavior simulation engine that runs alongside the protocol

-- | Run the complete protocol simulation with agent behaviors
-- | This coordinates the simulation and protocol event loops
runProtocolSimulation :: 
  Ref ProtocolState -> 
  SimulationConfig -> 
  SimulationState -> 
  Effect { finalSimState :: SimulationState, finalProtocolState :: ProtocolState }
runProtocolSimulation protocolRef config initialSimState = do
  log "\n=== Starting Protocol Simulation ==="
  log $ "Simulating " <> show config.simulationBlocks <> " blocks with " <> show (length initialSimState.accounts) <> " agents"
  
  -- Initialize agent accounts in the protocol
  protocolState <- read protocolRef
  traverse_ (initializeAgentAccount protocolState.accounts) initialSimState.accounts
  
  -- Run simulation loop
  finalState <- foldl (runProtocolSimulationBlock protocolRef config) 
    (pure initialSimState) 
    (range 1 config.simulationBlocks)
    
  -- Get final states
  finalProtocolState <- read protocolRef
  
  log "\n=== Protocol Simulation Complete ==="
  pure { finalSimState: finalState, finalProtocolState: finalProtocolState }

-- | Initialize a single agent's chain account
initializeAgentAccount :: AccountRegistry -> SimulatedAccount -> Effect Unit
initializeAgentAccount accounts account = do
  log $ "Initializing account " <> account.id <> " with " <> show account.jitoSOLBalance <> " JitoSOL"
  result <- updateChainAccountBalance accounts account.id account.jitoSOLBalance
  case result of
    Right _ -> do
      balance <- getChainAccountBalance accounts account.id
      log $ "Account " <> account.id <> " initialized with " <> show balance <> " JitoSOL"
    Left err -> log $ "Failed to initialize account " <> account.id <> ": " <> err

-- | Execute a single protocol simulation block
-- | This runs one iteration of the simulation loop:
-- | 1. Generate market conditions for this block
-- | 2. Run the protocol block (advances blockchain state)
-- | 3. Read updated protocol state
-- | 4. Generate agent decisions based on new state
-- | 5. Submit agent orders to the protocol
runProtocolSimulationBlock :: 
  Ref ProtocolState -> 
  SimulationConfig -> 
  Effect SimulationState -> 
  Int -> 
  Effect SimulationState
runProtocolSimulationBlock protocolRef config simStateEffect blockNum = do
  simState <- simStateEffect
  log $ "\n--- Protocol Simulation Block " <> show blockNum <> " ---"
  
  -- Step 1: Generate market price for this block
  priceMovement <- generateMarketScenario config blockNum
  let newPrice = simState.currentPrice * (1.0 + priceMovement)
  log $ "Market conditions: price " <> show simState.currentPrice <> " -> " <> show newPrice
  
  -- Step 2: Run protocol block (this advances the blockchain)
  polSnapshot <- runProtocolBlock protocolRef newPrice blockNum
  
  -- Step 3: Read updated protocol state
  protocolState <- read protocolRef
  
  -- Step 4: Generate agent orders based on current state
  log "\nAgent Decision Phase:"
  tradingActions <- generateTradingSequence config 
    { accounts: simState.accounts
    , currentBlock: blockNum
    , actionHistory: simState.actionHistory
    , oracle: protocolState.oracle  -- Use protocol's oracle
    }
  
  log $ "Agents generated " <> show (length tradingActions) <> " orders"
  
  -- Step 5: Execute agent orders (submit to protocol)
  log "\nOrder Execution Phase:"
  updatedSimState <- executeAgentOrders protocolRef simState tradingActions
  
  -- Step 6: Take market snapshot from protocol
  marketSnapshot <- takeMarketSnapshot protocolState.oracle
  
  -- Update simulation state
  pure $ updatedSimState 
    { currentBlock = blockNum
    , currentPrice = newPrice
    , priceHistory = { price: marketSnapshot.spot, timestamp: polSnapshot.timestamp } : updatedSimState.priceHistory
    , actionHistory = updatedSimState.actionHistory <> tradingActions
    , polAllocationHistory = { block: blockNum, timestamp: polSnapshot.timestamp, allocations: polSnapshot.allocations } : updatedSimState.polAllocationHistory
    }

-- | Execute a batch of agent orders
executeAgentOrders :: 
  Ref ProtocolState -> 
  SimulationState -> 
  Array TradingAction -> 
  Effect SimulationState
executeAgentOrders protocolRef simState actions = 
  foldl (executeAgentOrder protocolRef) (pure simState) actions

-- | Execute a single agent order through the protocol
executeAgentOrder :: 
  Ref ProtocolState -> 
  Effect SimulationState -> 
  TradingAction -> 
  Effect SimulationState
executeAgentOrder protocolRef simStateEffect action = do
  simState <- simStateEffect
  protocolState <- read protocolRef
  
  case action of
    -- Agent enters protocol: Convert JitoSOL to FeelsSOL
    EnterProtocol userId amount _ -> do
      result <- executeCommand (PS.EnterFeelsSOL userId amount) protocolState
      case result of
        Right (Tuple newProtocolState (FeelsSOLMinted _)) -> do
          write newProtocolState protocolRef
          -- Update agent balances in simulation
          let updatedAccounts1 = updateProtocolAccountBalances simState.accounts userId (-amount) JitoSOL
              updatedAccounts2 = updateProtocolAccountBalances updatedAccounts1 userId amount FeelsSOL
          pure simState { accounts = updatedAccounts2 }
        _ -> do
          log $ "Agent " <> userId <> " failed to enter protocol"
          pure simState
              
    -- Agent exits protocol: Convert FeelsSOL to JitoSOL
    ExitProtocol userId amount _ -> do
      result <- executeCommand (PS.ExitFeelsSOL userId amount) protocolState
      case result of
        Right (Tuple newProtocolState (FeelsSOLBurned _)) -> do
          write newProtocolState protocolRef
          -- Calculate actual JitoSOL received after fees
          let jitoAmount = amount * 0.998  -- 0.2% exit fee
              updatedAccounts1 = updateProtocolAccountBalances simState.accounts userId (-amount) FeelsSOL
              updatedAccounts2 = updateProtocolAccountBalances updatedAccounts1 userId jitoAmount JitoSOL
          pure simState { accounts = updatedAccounts2 }
        _ -> do
          log $ "Agent " <> userId <> " failed to exit protocol"
          pure simState
              
    -- Agent creates lending position
    CreateLendOffer userId lendAsset amount collateralAsset collateralAmount duration _leverage _targetToken -> do
      let protocolLeverage = Senior  -- Default leverage for simulation
      
      result <- executeCommand 
        (PS.CreatePosition userId lendAsset amount collateralAsset collateralAmount 
          duration protocolLeverage false Nothing) 
        protocolState
        
      case result of
        Right (Tuple newProtocolState (PositionCreated _)) -> do
          write newProtocolState protocolRef
          log $ "Agent " <> userId <> " created lending position"
          -- Update agent balances
          let updatedAccounts = updateProtocolAccountBalances simState.accounts userId (-amount) lendAsset
          pure simState { accounts = updatedAccounts }
        _ -> do
          log $ "Agent " <> userId <> " failed to create position"
          pure simState
              
    -- Agent takes loan (executes swap through pool)
    TakeLoan userId lendAsset amount collateralAsset _collateralAmount _duration _leverage -> do
      -- Determine pool for this token pair
      let poolId = case Tuple lendAsset collateralAsset of
            Tuple (Token ticker) FeelsSOL -> ticker <> "/FeelsSOL"
            Tuple FeelsSOL (Token ticker) -> ticker <> "/FeelsSOL"
            _ -> "Unknown/FeelsSOL"
      
      -- Execute swap through the pool
      maybePool <- getPool poolId protocolState.poolRegistry
      case maybePool of
        Just pool -> do
          -- Determine swap direction
          let zeroForOne = case lendAsset of
                FeelsSOL -> false  -- Selling token for FeelsSOL
                _ -> true          -- Selling FeelsSOL for token
              
              swapParams = 
                { zeroForOne: zeroForOne
                , amountSpecified: amount  -- Positive = exact input
                , sqrtPriceLimitX96: 0.0   -- No price limit
                }
          
          -- Execute swap
          protocolState' <- read protocolRef
          let currentBlock = protocolState'.currentBlock + 1
          let swapResult = swap pool swapParams currentBlock
          
          -- Update pool state
          updatePool poolId swapResult.updatedPool protocolState.poolRegistry
          
          log $ "Agent " <> userId <> " swapped in pool " <> poolId
          
          -- Update agent balances based on swap result
          let amountOut = if zeroForOne 
                then abs swapResult.result.amount1
                else abs swapResult.result.amount0
              
              updatedAccounts1 = updateProtocolAccountBalances simState.accounts userId (-amount) collateralAsset
              updatedAccounts2 = updateProtocolAccountBalances updatedAccounts1 userId amountOut lendAsset
              
          pure simState { accounts = updatedAccounts2 }
          
        Nothing -> do
          log $ "Pool " <> poolId <> " not found for agent " <> userId
          pure simState
      
    -- Agent creates new token
    CreateToken userId ticker name -> do
      result <- executeCommand (PS.CreateToken userId ticker name) protocolState
      case result of
        Right (Tuple newProtocolState (TokenCreated _)) -> do
          write newProtocolState protocolRef
          log $ "Agent " <> userId <> " created token " <> ticker
          pure simState
        _ -> do
          log $ "Agent " <> userId <> " failed to create token " <> ticker
          pure simState
          
    -- Other agent actions
    _ -> pure simState

-- | Update agent account balances in simulation state (for protocol integration)
updateProtocolAccountBalances :: Array SimulatedAccount -> String -> Number -> TokenType -> Array SimulatedAccount
updateProtocolAccountBalances accounts userId amount token =
  map updateAccount accounts
  where
    updateAccount acc
      | acc.id == userId = case token of
          JitoSOL -> acc { jitoSOLBalance = max 0.0 (acc.jitoSOLBalance + amount) }
          FeelsSOL -> acc { feelsSOLBalance = max 0.0 (acc.feelsSOLBalance + amount) }
          Token _ -> acc
      | otherwise = acc
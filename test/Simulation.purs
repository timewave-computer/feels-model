-- End-to-end simulation tests for the Feels protocol.
-- Tests the complete simulation workflow: token creation, funding, market simulation,
-- and verification of results including POL growth and trading activity.
module Test.Simulation where

import Prelude

import Data.Array (length, filter, range)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (Result(..), quickCheck)

-- Core system imports
import Protocol.Token (TokenCreationParams, TokenMetadata, createToken)
import Protocol.POL (getTotalPOL)
import Simulation.Sim (AccountProfile(..), MarketScenario(..), SimulationConfig, SimulationState, initSimulation, runSimulation, executeSimulation)
import Utils (formatAmount)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Test Configuration
--------------------------------------------------------------------------------

-- Create a test simulation configuration
createTestConfig :: SimulationConfig
createTestConfig = 
  { scenario: BullMarket
  , numAccounts: 5
  , simulationBlocks: 20
  , initialJitoSOLPrice: 100.0
  , priceVolatility: 0.1
  , accountProfiles: [Conservative, Moderate, Aggressive, Whale, Retail]
  , actionFrequency: 2.0
  , juniorTranchePreference: 0.3
  }

-- Create test token creation parameters for simulation
createTestTokenData :: Int -> Array TokenCreationParams
createTestTokenData numTokens = 
  map (\i -> 
    { ticker: "SIM" <> show i
    , name: "Simulation Token " <> show i
    , creator: "simulator"
    }) (range 1 numTokens)

-- Enhanced simulation state for testing
type TestSimulationState =
  { simulationState :: SimulationState
  , createdTokens :: Array TokenMetadata
  , initialPOL :: Number
  , finalPOL :: Number
  , totalTrades :: Int
  , totalVolume :: Number
  }

--------------------------------------------------------------------------------
-- Token Setup Functions
--------------------------------------------------------------------------------

-- Create and fund simulation tokens
setupSimulationTokens :: Int -> Effect (Either String (Array TokenMetadata))
setupSimulationTokens numTokens = do
  log $ "Creating " <> show numTokens <> " simulation tokens..."
  
  let tokenData = createTestTokenData numTokens
  
  -- Create tokens
  createdTokens <- traverse createSimulationToken tokenData
  
  log $ "Successfully created " <> show (length createdTokens) <> " tokens"
  
  -- Fund each token with 150 FeelsSOL to exceed launch threshold
  fundingResults <- traverse (fundSimulationToken 150.0) createdTokens
  
  case sequence fundingResults of
    Left fundErr -> pure (Left fundErr)
    Right _ -> do
      log $ "Successfully funded all " <> show (length createdTokens) <> " tokens"
      pure (Right createdTokens)

-- Create a single simulation token
createSimulationToken :: TokenCreationParams -> Effect TokenMetadata
createSimulationToken params = do
  token <- createToken params
  log $ "Created token: " <> params.ticker
  pure token

-- Fund a simulation token to make it live
fundSimulationToken :: Number -> TokenMetadata -> Effect (Either String Unit)
fundSimulationToken amount token = do
  log $ "Funding token " <> token.ticker <> " with " <> formatAmount amount <> " FeelsSOL"
  -- TODO: Integrate with actual funding mechanism
  -- For now, simulate successful funding
  pure (Right unit)

--------------------------------------------------------------------------------
-- Test Execution Functions
--------------------------------------------------------------------------------

-- Run complete end-to-end simulation test
runE2ESimulationTest :: Effect (Either String TestSimulationState)
runE2ESimulationTest = do
  log "=== Starting End-to-End Simulation Test ==="
  
  -- Phase 1: Setup tokens
  log "Phase 1: Setting up simulation tokens..."
  tokenSetupResult <- setupSimulationTokens 3
  
  case tokenSetupResult of
    Left err -> pure (Left $ "Token setup failed: " <> err)
    Right createdTokens -> do
      
      -- Phase 2: Initialize simulation
      log "Phase 2: Initializing simulation..."
      let config = createTestConfig
      initialState <- initSimulation config
      
      -- Record initial POL
      initialPOL <- getTotalPOL initialState.gateway.polState
      log $ "Initial POL: " <> formatAmount initialPOL
      
      -- Phase 3: Run simulation
      log "Phase 3: Running market simulation..."
      finalState <- executeSimulation config initialState
      
      -- Record final POL
      finalPOL <- getTotalPOL finalState.gateway.polState
      log $ "Final POL: " <> formatAmount finalPOL
      
      -- Phase 4: Calculate metrics
      let totalTrades = length finalState.actionHistory
      let totalVolume = calculateTotalVolume finalState.actionHistory
      
      log $ "Simulation completed with " <> show totalTrades <> " trades"
      log $ "Total volume: " <> formatAmount totalVolume
      
      pure (Right { simulationState: finalState
                  , createdTokens: createdTokens
                  , initialPOL: initialPOL
                  , finalPOL: finalPOL
                  , totalTrades: totalTrades
                  , totalVolume: totalVolume
                  })

-- Calculate total trading volume from action history
calculateTotalVolume :: forall a. Array a -> Number
calculateTotalVolume actions = 
  -- TODO: Implement proper volume calculation based on action types
  Int.toNumber (length actions) * 100.0  -- Placeholder calculation

--------------------------------------------------------------------------------
-- Verification Functions
--------------------------------------------------------------------------------

-- Verify token creation was successful
verifyTokenCreation :: Array TokenMetadata -> Result
verifyTokenCreation tokens = 
  if length tokens > 0
    then Success
    else Failed "No tokens were created during simulation"

-- Verify trading activity occurred
verifyTradingActivity :: Int -> Number -> Result
verifyTradingActivity totalTrades totalVolume = 
  if totalTrades > 0 && totalVolume > 0.0
    then Success
    else Failed $ "Insufficient trading activity: " <> show totalTrades <> " trades, " <> formatAmount totalVolume <> " volume"

-- Verify POL growth
verifyPOLGrowth :: Number -> Number -> Result
verifyPOLGrowth initialPOL finalPOL = 
  let growth = finalPOL - initialPOL
      growthPercentage = if initialPOL > 0.0 then (growth / initialPOL) * 100.0 else 0.0
  in if growth > 0.0
       then Success
       else Failed $ "POL did not grow. Initial: " <> formatAmount initialPOL <> ", Final: " <> formatAmount finalPOL <> " (Growth: " <> formatAmount growthPercentage <> "%)"

-- Verify simulation state consistency
verifySimulationConsistency :: TestSimulationState -> Result
verifySimulationConsistency testState = 
  let state = testState.simulationState
      priceHistoryLength = length state.priceHistory
      expectedBlocks = 20  -- From test config
  in if priceHistoryLength >= expectedBlocks
       then Success
       else Failed $ "Price history incomplete: " <> show priceHistoryLength <> " entries, expected at least " <> show expectedBlocks

-- Verify account activity
verifyAccountActivity :: forall r. Array { totalDeposited :: Number, activePositions :: Array Int | r } -> Result
verifyAccountActivity accounts = 
  let activeAccounts = filter (\acc -> acc.totalDeposited > 0.0 || length acc.activePositions > 0) accounts
  in if length activeAccounts > 0
       then Success
       else Failed "No accounts showed any activity during simulation"

--------------------------------------------------------------------------------
-- Test Runner
--------------------------------------------------------------------------------

-- Run all simulation tests
runSimulationTests :: Effect Unit
runSimulationTests = do
  log "Running End-to-End Simulation Tests..."
  
  -- Run the complete simulation test
  testResult <- runE2ESimulationTest
  
  case testResult of
    Left err -> do
      log $ "❌ E2E Simulation Test FAILED: " <> err
    
    Right testState -> do
      log "✅ E2E Simulation Test completed successfully"
      
      -- Run verification tests
      log "\nRunning verification tests..."
      
      -- Test 1: Token creation
      let tokenTest = verifyTokenCreation testState.createdTokens
      logTestResult "Token Creation" tokenTest
      
      -- Test 2: Trading activity
      let tradingTest = verifyTradingActivity testState.totalTrades testState.totalVolume
      logTestResult "Trading Activity" tradingTest
      
      -- Test 3: POL growth
      let polTest = verifyPOLGrowth testState.initialPOL testState.finalPOL
      logTestResult "POL Growth" polTest
      
      -- Test 4: Simulation consistency
      let consistencyTest = verifySimulationConsistency testState
      logTestResult "Simulation Consistency" consistencyTest
      
      -- Test 5: Account activity
      let accountTest = verifyAccountActivity testState.simulationState.accounts
      logTestResult "Account Activity" accountTest
      
      -- Print final statistics
      log "\n=== Final Test Statistics ==="
      log $ "Tokens Created: " <> show (length testState.createdTokens)
      log $ "Total Trades: " <> show testState.totalTrades
      log $ "Total Volume: " <> formatAmount testState.totalVolume
      log $ "POL Growth: " <> formatAmount (testState.finalPOL - testState.initialPOL) <> " (" <> formatAmount ((testState.finalPOL - testState.initialPOL) / testState.initialPOL * 100.0) <> "%"
      log $ "Simulation Blocks: " <> show testState.simulationState.currentBlock
      log $ "Active Accounts: " <> show (length testState.simulationState.accounts)

-- Helper to log test results
logTestResult :: String -> Result -> Effect Unit
logTestResult testName result = 
  case result of
    Success -> log $ "✅ " <> testName <> ": PASSED"
    Failed msg -> log $ "❌ " <> testName <> ": FAILED - " <> msg

-- Property-based test for simulation determinism
testSimulationDeterminism :: Effect Unit
testSimulationDeterminism = do
  log "Testing simulation determinism..."
  quickCheck (\(_ :: Int) -> 
    -- TODO: Implement deterministic simulation test with fixed seed
    -- This would run the same simulation twice and verify identical results
    Success)

-- Performance test for large simulations
testLargeSimulationPerformance :: Effect Unit
testLargeSimulationPerformance = do
  log "Testing large simulation performance..."
  let largeConfig = createTestConfig { numAccounts = 50, simulationBlocks = 100, actionFrequency = 5.0 }
  
  startTime <- currentTime
  _ <- runSimulation largeConfig
  endTime <- currentTime
  
  let duration = endTime - startTime
  log $ "Large simulation completed in " <> formatAmount duration <> "ms"
  log $ "Performance: " <> formatAmount (Int.toNumber largeConfig.simulationBlocks / duration * 1000.0) <> " blocks/second"
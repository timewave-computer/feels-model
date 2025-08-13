-- | Test suite entry point for the Feels Protocol
-- |
-- | Coordinates execution of all test modules including:
-- | - Property-based tests for mathematical invariants
-- | - Unit tests for individual components
-- | - Integration tests for cross-module workflows
-- | - Simulation tests for market scenarios
module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Model (runModelTests)
import Test.Simulation (runSimulationTests)
import Test.LendingYieldTest (lendingYieldTests)

--------------------------------------------------------------------------------
-- TEST SUITE EXECUTION
--------------------------------------------------------------------------------

-- | Main test runner that executes all active test suites
main :: Effect Unit
main = do
  log "Starting Feels Protocol Test Suite"
  log "===================================="
  
  -- Core mathematical property tests
  runModelTests
  
  -- Market simulation and scenario tests
  runSimulationTests
  
  -- Lending yield system tests
  lendingYieldTests
  
  log "===================================="
  log "Test suite execution completed" 
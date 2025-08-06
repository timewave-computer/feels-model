-- Test suite entry point for the Feels protocol.
-- Coordinates execution of all test modules including property-based tests,
-- unit tests, and integration tests that validate the unified lending paradigm.
module Test.Main where

import Prelude
import Effect (Effect)
import Test.Model (runModelTests)
import Test.Simulation (runSimulationTests)

main :: Effect Unit
main = do
  runModelTests
  runSimulationTests 
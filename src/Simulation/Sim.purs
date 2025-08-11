-- Main Simulation Module - Re-exports all simulation functionality
-- Maintains backward compatibility while providing organized module structure
module Simulation.Sim
  ( module Simulation.Agents
  , module Simulation.Market  
  , module Simulation.Actions
  , module Simulation.Engine
  , module Simulation.Analysis
  ) where

-- Re-export from Agents module
import Simulation.Agents (AccountProfile(..), SimulatedAccount, generateAccounts)

-- Re-export from Market module  
import Simulation.Market (MarketScenario(..), SimulationConfig, generateMarketScenario)

-- Re-export from Actions module
import Simulation.Actions (TradingAction(..), generateTradingSequence, getRecentlyCreatedTokens)

-- Re-export from Engine module
import Simulation.Engine (SimulationState, initSimulation, initSimulationWithPoolRegistry, executeSimulation, runSimulation, runSimulationWithPoolRegistry, getSimulationStats)

-- Re-export from Analysis module
import Simulation.Analysis (SimulationResults, calculateResults)
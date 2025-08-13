-- Main Simulation Module - Re-exports all simulation functionality
-- Maintains backward compatibility while providing organized module structure
module Simulation.Sim
  ( module Simulation.Agent
  , module Simulation.Market  
  , module Simulation.Action
  , module Simulation.Engine
  , module Simulation.Analysis
  ) where

-- Re-export from Agent module
import Simulation.Agent (AccountProfile(..), SimulatedAccount, generateAccounts)

-- Re-export from Market module  
import Simulation.Market (MarketScenario(..), SimulationConfig, generateMarketScenario)

-- Re-export from Action module
import Simulation.Action (TradingAction(..), generateTradingSequence, getRecentlyCreatedTokens)

-- Re-export from Engine module
import Simulation.Engine (SimulationState, initSimulation, initSimulationWithPoolRegistry, executeSimulation, runSimulation, runSimulationWithPoolRegistry, getSimulationStats)

-- Re-export from Analysis module
import Simulation.Analysis (SimulationResults, calculateResults)
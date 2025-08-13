-- | Market Scenarios and Price Dynamics for Simulation Engine
-- |
-- | This module defines comprehensive market conditions and realistic price
-- | movement patterns for DeFi protocol simulations. It models various market
-- | regimes with appropriate volatility, trends, and behavioral characteristics.
-- |
-- | Market Scenarios:
-- | - Bull Market: Sustained upward price trends with moderate volatility
-- | - Bear Market: Downward pressure with increased selling activity
-- | - Sideways Market: Range-bound trading with minimal directional bias
-- | - Volatile Market: High-amplitude price swings in both directions
-- | - Crash Scenario: Sudden market collapse followed by gradual recovery
-- | - Recovery Market: Post-crash healing with diminishing volatility
-- |
-- | Price Dynamics:
-- | - Realistic volatility ranges based on DeFi market observations
-- | - Time-dependent effects for scenarios like crash recovery
-- | - Configurable parameters for different simulation requirements
module Simulation.Market
  ( MarketScenario(..)
  , SimulationConfig
  , generateMarketScenario
  ) where

import Prelude
import Data.Int as Int
import Effect (Effect)
import Effect.Random (random)

-- Import AccountProfile from Agent module for configuration
import Simulation.Agent (AccountProfile)

--------------------------------------------------------------------------------
-- MARKET SCENARIO TYPE DEFINITIONS
--------------------------------------------------------------------------------
-- Comprehensive market conditions for realistic DeFi simulation scenarios

-- | Market regime classifications with distinct price behavior patterns
-- | Each scenario models different phases of crypto/DeFi market cycles
data MarketScenario
  = BullMarket      -- Sustained uptrend: +0.2% to +1.2% bias with moderate volatility
  | BearMarket      -- Sustained downtrend: -1.2% to -2.0% bias with elevated volatility
  | SidewaysMarket  -- Range-bound: ±0.15% neutral movements with low volatility
  | VolatileMarket  -- High volatility: ±3% to ±10% price swings in both directions
  | CrashScenario   -- Market collapse: -5% to -20% initial drops with slow recovery
  | RecoveryMarket  -- Post-crash healing: diminishing positive bias over time

derive instance eqMarketScenario :: Eq MarketScenario

instance showMarketScenario :: Show MarketScenario where
  show BullMarket = "Bull Market"
  show BearMarket = "Bear Market"
  show SidewaysMarket = "Sideways Market"
  show VolatileMarket = "Volatile Market"
  show CrashScenario = "Market Crash"
  show RecoveryMarket = "Market Recovery"

--------------------------------------------------------------------------------
-- SIMULATION CONFIGURATION PARAMETERS
--------------------------------------------------------------------------------
-- Comprehensive configuration for realistic DeFi protocol simulations

-- | Complete simulation configuration combining market conditions with protocol parameters
-- | Controls all aspects of simulation from agent behavior to market dynamics
type SimulationConfig =
  { scenario :: MarketScenario                  -- Market regime to simulate
  , numAccounts :: Int                          -- Population size for simulation
  , simulationBlocks :: Int                     -- Duration of simulation in blocks
  , initialJitoSOLPrice :: Number               -- Starting price reference point
  , priceVolatility :: Number                   -- Volatility multiplier (0.0-1.0)
  , accountProfiles :: Array AccountProfile     -- Distribution of agent types
  , actionFrequency :: Number                   -- Average actions per block (0.0-10.0)
  , juniorTranchePreference :: Number           -- Agent preference for junior positions (0.0-1.0)
  }

--------------------------------------------------------------------------------
-- MARKET SCENARIO PRICE GENERATION ENGINE
--------------------------------------------------------------------------------
-- Realistic price movement generation based on market regime characteristics

-- | Generate realistic price movements for different market scenarios
-- | Returns percentage price change per block based on scenario and time progression
generateMarketScenario :: SimulationConfig -> Int -> Effect Number
generateMarketScenario config currentBlock = do
  baseVolatility <- random
  let volatility = config.priceVolatility * baseVolatility
  
  -- Generate scenario-specific price movements with realistic characteristics
  case config.scenario of
    BullMarket -> do
      -- Sustained upward momentum with moderate volatility
      -- Models crypto bull runs with consistent but not extreme gains
      trend <- random
      let movement = 0.002 + trend * 0.01  -- +0.2% to +1.2% upward bias
      pure movement
    
    BearMarket -> do  
      -- Persistent downward pressure with elevated volatility
      -- Models crypto bear markets with consistent selling pressure
      trend <- random
      let movement = -0.012 - trend * 0.008  -- -1.2% to -2.0% downward bias
      pure movement
    
    SidewaysMarket -> do
      -- Range-bound trading with minimal directional bias
      -- Models consolidation periods between major market moves
      noise <- random  
      pure $ (noise - 0.5) * 0.003  -- ±0.15% neutral oscillation
    
    VolatileMarket -> do
      -- High-amplitude price swings characteristic of crypto volatility
      -- Models periods of uncertainty with large price movements
      swing <- random
      direction <- random
      let magnitude = 0.03 + swing * 0.07  -- ±3% to ±10% dramatic moves
      pure $ if direction > 0.5 then magnitude else -magnitude
    
    CrashScenario -> do
      -- Initial crash phase followed by gradual stabilization
      -- Models flash crashes and subsequent market recovery patterns
      if currentBlock < 10 
        then do
          -- Acute crash phase with severe downward pressure
          crash <- random
          pure $ -0.05 - crash * 0.15  -- -5% to -20% crash drops
        else do
          -- Gradual recovery phase with reduced volatility
          recovery <- random  
          pure $ -0.001 + recovery * 0.003  -- Slow healing process
    
    RecoveryMarket -> do
      -- Post-crash recovery with diminishing upward momentum over time
      -- Models the healing process after major market disruptions
      recovery <- random
      let phase = Int.toNumber currentBlock / 100.0  -- Time-decay factor
      pure $ 0.002 + recovery * 0.008 * (1.0 - phase)  -- Diminishing recovery strength
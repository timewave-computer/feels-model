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
module Simulation.Scenario
  ( MarketScenario(..)
  , SimulationConfig
  , generateMarketScenario
  -- Scenario-specific behavior functions
  , getVolatilityMultiplier
  , getMarketTradingBias
  , getActionFrequencyMultiplier
  , getSizingMultiplier
  , getMarketExitBias
  , getOpportunityMultiplier
  , getPanicMultiplier
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

--------------------------------------------------------------------------------
-- SCENARIO-SPECIFIC BEHAVIOR FUNCTIONS
--------------------------------------------------------------------------------
-- Functions that determine agent behavior based on market scenarios

-- | Get volatility multiplier for trading activity based on market scenario
-- | Volatile markets trigger more frequent and larger trades
getVolatilityMultiplier :: MarketScenario -> Number
getVolatilityMultiplier scenario = case scenario of
  VolatileMarket -> 3.0   -- 3x more trades in volatile markets
  CrashScenario -> 2.5    -- 2.5x panic selling/buying
  BearMarket -> 1.8       -- 1.8x increased exit activity
  RecoveryMarket -> 1.5   -- 1.5x opportunity buying
  BullMarket -> 1.2       -- 1.2x moderate increase
  SidewaysMarket -> 1.0   -- Normal activity

-- | Get market trading bias (buy vs sell probability) based on scenario
-- | Returns probability of buying (0.0 = all sells, 1.0 = all buys)
getMarketTradingBias :: MarketScenario -> Number
getMarketTradingBias scenario = case scenario of
  BullMarket -> 0.7       -- 70% buy probability in bull markets
  BearMarket -> 0.3       -- 30% buy probability in bear markets
  RecoveryMarket -> 0.6   -- 60% buy the dip
  CrashScenario -> 0.2    -- 20% buying during panic
  VolatileMarket -> 0.5   -- 50% neutral (arbitrage opportunities)
  SidewaysMarket -> 0.5   -- 50% neutral

-- | Get action frequency multiplier for market scenarios
-- | How much more active agents become in different market conditions
getActionFrequencyMultiplier :: MarketScenario -> Number
getActionFrequencyMultiplier scenario = case scenario of
  VolatileMarket -> 3.0   -- 3x more trades in volatile markets
  CrashScenario -> 2.5    -- 2.5x panic trading
  BearMarket -> 1.8       -- 1.8x increased activity
  RecoveryMarket -> 1.5   -- 1.5x opportunity seeking
  BullMarket -> 1.2       -- 1.2x moderate increase
  SidewaysMarket -> 1.0   -- Normal activity

-- | Get position sizing multiplier for different market scenarios
-- | Affects both buy and sell order sizes based on market conditions
getSizingMultiplier :: MarketScenario -> { buy :: Number, sell :: Number }
getSizingMultiplier scenario = case scenario of
  VolatileMarket -> { buy: 3.0, sell: 3.0 }     -- 3x for arbitrage opportunities
  CrashScenario -> { buy: 2.5, sell: 3.5 }     -- 2.5x buy, 3.5x panic selling
  BearMarket -> { buy: 2.0, sell: 2.5 }        -- 2x defensive, 2.5x risk reduction
  RecoveryMarket -> { buy: 1.8, sell: 1.0 }    -- 1.8x opportunity, normal sells
  BullMarket -> { buy: 1.0, sell: 1.0 }        -- Normal sizing
  SidewaysMarket -> { buy: 1.0, sell: 1.0 }    -- Normal sizing

-- | Get market exit bias for different scenarios
-- | Returns { exitBias, entryBias } probabilities for protocol entry/exit
getMarketExitBias :: MarketScenario -> { exitBias :: Number, entryBias :: Number }
getMarketExitBias scenario = case scenario of
  BearMarket -> { exitBias: 0.7, entryBias: 0.1 }      -- 70% exit, 10% entry
  CrashScenario -> { exitBias: 0.8, entryBias: 0.05 }  -- 80% panic exit, 5% entry
  VolatileMarket -> { exitBias: 0.5, entryBias: 0.3 }  -- 50% exit, 30% entry (arb)
  RecoveryMarket -> { exitBias: 0.2, entryBias: 0.6 }  -- 20% exit, 60% buy the dip
  BullMarket -> { exitBias: 0.2, entryBias: 0.5 }      -- 20% profit taking, 50% FOMO
  SidewaysMarket -> { exitBias: 0.3, entryBias: 0.3 }  -- 30% each, balanced

-- | Get opportunity multiplier for protocol entry during different market scenarios
-- | Higher values indicate more aggressive buying during market opportunities
getOpportunityMultiplier :: MarketScenario -> Number
getOpportunityMultiplier scenario = case scenario of
  RecoveryMarket -> 2.0  -- 2x buying the dip
  VolatileMarket -> 1.5  -- 1.5x arbitrage capital
  BullMarket -> 1.2      -- 1.2x FOMO buying
  CrashScenario -> 1.0   -- Normal entry (contrarian opportunity)
  BearMarket -> 1.0      -- Normal entry
  SidewaysMarket -> 1.0  -- Normal entry

-- | Get panic multiplier for protocol exit during different market scenarios
-- | Represents percentage of holdings that users typically exit
getPanicMultiplier :: MarketScenario -> Number
getPanicMultiplier scenario = case scenario of
  CrashScenario -> 0.8   -- 80% of holdings in panic
  BearMarket -> 0.6      -- 60% risk reduction
  VolatileMarket -> 0.4  -- 40% partial exit
  RecoveryMarket -> 0.3  -- 30% normal profit taking
  BullMarket -> 0.3      -- 30% normal profit taking
  SidewaysMarket -> 0.3  -- 30% normal profit taking
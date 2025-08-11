-- Market Scenarios and Price Dynamics for Simulation Engine
-- Defines market conditions and price movement patterns
module Simulation.Market
  ( MarketScenario(..)
  , SimulationConfig
  , generateMarketScenario
  ) where

import Prelude
import Data.Int as Int
import Effect (Effect)
import Effect.Random (random)

-- Import AccountProfile from Agents module
import Simulation.Agents (AccountProfile)

--------------------------------------------------------------------------------
-- Market Scenario Types
--------------------------------------------------------------------------------

-- Market scenario types
data MarketScenario
  = BullMarket      -- Rising prices, high optimism
  | BearMarket      -- Falling prices, high pessimism
  | SidewaysMarket  -- Range-bound, low volatility
  | VolatileMarket  -- High price swings
  | CrashScenario   -- Sudden market collapse
  | RecoveryMarket  -- Post-crash recovery

derive instance eqMarketScenario :: Eq MarketScenario

instance showMarketScenario :: Show MarketScenario where
  show BullMarket = "Bull Market"
  show BearMarket = "Bear Market"
  show SidewaysMarket = "Sideways Market"
  show VolatileMarket = "Volatile Market"
  show CrashScenario = "Market Crash"
  show RecoveryMarket = "Market Recovery"

-- Simulation configuration
type SimulationConfig =
  { scenario :: MarketScenario
  , numAccounts :: Int                    -- Number of simulated accounts
  , simulationBlocks :: Int               -- How many blocks to simulate
  , initialJitoSOLPrice :: Number         -- Starting price of JitoSOL
  , priceVolatility :: Number             -- Price change volatility (0.0-1.0)
  , accountProfiles :: Array AccountProfile  -- Mix of account types
  , actionFrequency :: Number             -- Actions per block (0.0-10.0)
  , juniorTranchePreference :: Number     -- Preference for junior tranche positions (0.0-1.0)
  }

--------------------------------------------------------------------------------
-- Market Scenario Generation
--------------------------------------------------------------------------------

-- Generate price movements for different market scenarios
generateMarketScenario :: SimulationConfig -> Int -> Effect Number
generateMarketScenario config currentBlock = do
  baseVolatility <- random
  let volatility = config.priceVolatility * baseVolatility
  
  -- Log scenario effect periodically
  -- when (currentBlock <= 5 || currentBlock `mod` 20 == 0) $ do
  --   log $ "Market scenario " <> show config.scenario <> " at block " <> show currentBlock
  
  case config.scenario of
    BullMarket -> do
      trend <- random
      let movement = 0.002 + trend * 0.01  -- 0.2% to 1.2% upward bias (stronger effect)
      pure movement
    
    BearMarket -> do  
      trend <- random
      let movement = -0.012 - trend * 0.008  -- -1.2% to -2.0% downward bias (stronger effect)
      pure movement
    
    SidewaysMarket -> do
      noise <- random  
      pure $ (noise - 0.5) * 0.003  -- -0.15% to +0.15%
    
    VolatileMarket -> do
      swing <- random
      direction <- random
      let magnitude = 0.03 + swing * 0.07  -- 3% to 10% moves (much stronger)
      pure $ if direction > 0.5 then magnitude else -magnitude
    
    CrashScenario -> do
      if currentBlock < 10 
        then do
          crash <- random
          pure $ -0.05 - crash * 0.15  -- -5% to -20% drops
        else do
          recovery <- random  
          pure $ -0.001 + recovery * 0.003  -- Slow recovery
    
    RecoveryMarket -> do
      recovery <- random
      let phase = Int.toNumber currentBlock / 100.0
      pure $ 0.002 + recovery * 0.008 * (1.0 - phase)  -- Diminishing recovery
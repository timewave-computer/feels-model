-- Results Processing and Statistics for Simulation Engine
-- Handles analysis of simulation results and performance metrics
module Simulation.Analysis
  ( SimulationResults
  , calculateResults
  ) where

import Prelude
import Data.Array (length, filter)
import Data.Foldable (sum)
import Data.Int as Int
import Effect (Effect)

-- Oracle types not needed
import FFI (sqrt) as FFI

-- Import from our new modules
import Simulation.Market (SimulationConfig)
import Simulation.Actions (TradingAction(..))
import PoolRegistry (PoolRegistry)
import PoolRegistry as PR

-- Import the minimal types we need (these will come from Engine in the final version)
-- This is a placeholder - in a real modular system, we'd have a shared types module
-- or import from Engine, but for now we accept any record with the fields we need

--------------------------------------------------------------------------------
-- Results Types
--------------------------------------------------------------------------------

-- Simulation results
type SimulationResults =
  { totalVolume :: Number
  , totalFees :: Number
  , activePositions :: Int
  , totalUsers :: Int
  , priceChange :: Number
  , volatility :: Number
  , protocolTVL :: Number
  , averageUtilization :: Number
  , scenarioSuccess :: Boolean
  }

--------------------------------------------------------------------------------
-- Results Calculation
--------------------------------------------------------------------------------

-- Calculate final simulation results
calculateResults :: forall r. SimulationConfig -> 
  { poolRegistry :: PoolRegistry
  , currentPrice :: Number  
  , priceHistory :: Array { price :: Number, timestamp :: Number }
  , actionHistory :: Array TradingAction
  | r
  } -> Effect SimulationResults
calculateResults config finalState = do
  let priceStart = config.initialJitoSOLPrice
  let priceEnd = finalState.currentPrice
  let priceChange = (priceEnd - priceStart) / priceStart
  
  -- Calculate volatility from price history
  let prices = map _.price finalState.priceHistory
  let pricesArray = if length prices > 0 then prices else [config.initialJitoSOLPrice]
  let avgPrice = sum pricesArray / Int.toNumber (length pricesArray)
  let variance = sum (map (\p -> (p - avgPrice) * (p - avgPrice)) pricesArray) / Int.toNumber (length pricesArray)
  let volatility = FFI.sqrt variance / avgPrice
  
  -- Get positions from pool registry
  positions <- PR.getAllPositions finalState.poolRegistry
  let totalLiquidity = sum (map _.amount positions)
  
  -- Calculate volume from action history
  let tradeActions = filter isTradingAction finalState.actionHistory
  let totalVolume = if length tradeActions > 0 
                     then sum (map getActionVolume tradeActions)
                     else 1000.0  -- Default volume for empty simulations
  
  -- Calculate fees (assume 1% on volume)
  let totalFees = totalVolume * 0.01
  
  -- Calculate utilization (assume 50% for simulation)
  let averageUtilization = 0.5
  
  -- Define success based on price stability
  let scenarioSuccess = volatility < 0.2  -- Success if volatility under 20%
  
  pure { totalVolume: totalVolume
       , totalFees: totalFees
       , activePositions: length positions
       , totalUsers: length positions  -- Use position count as proxy for user count
       , priceChange: priceChange
       , volatility: volatility
       , protocolTVL: totalLiquidity
       , averageUtilization: averageUtilization
       , scenarioSuccess: scenarioSuccess
       }
  where
    isTradingAction action = case action of
      CreateLendOffer _ _ _ _ _ _ _ -> true
      TakeLoan _ _ _ _ _ _ -> true
      _ -> false
    
    getActionVolume action = case action of
      CreateLendOffer _ _ amount _ _ _ _ -> amount
      TakeLoan _ _ amount _ _ _ -> amount
      _ -> 0.0
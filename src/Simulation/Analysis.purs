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

-- Core system imports
import Token (TokenType(..))
import Oracle (PriceObservation)
import Utils (formatAmount, formatPercentage)
import FFI (sqrt) as FFI

-- Import from our new modules
import Simulation.Agents (SimulatedAccount)
import Simulation.Market (SimulationConfig)
import Simulation.Actions (TradingAction(..))
import LendingBook as LB

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
calculateResults :: forall r. SimulationConfig -> { accounts :: Array SimulatedAccount, currentPrice :: Number, priceHistory :: Array PriceObservation | r } -> Effect SimulationResults
calculateResults config finalState = do
  let priceStart = config.initialJitoSOLPrice
  let priceEnd = finalState.currentPrice
  let priceChange = (priceEnd - priceStart) / priceStart
  
  -- Calculate volatility from price history (using JitoSOL prices)
  let jitoSOLPrices = map _.price $ filter (\obs -> obs.tokenPair.base == JitoSOL) finalState.priceHistory
  let prices = if length jitoSOLPrices > 0 then jitoSOLPrices else [config.initialJitoSOLPrice]
  let avgPrice = sum prices / Int.toNumber (length prices)
  let variance = sum (map (\p -> (p - avgPrice) * (p - avgPrice)) prices) / Int.toNumber (length prices)
  let volatility = FFI.sqrt variance / avgPrice
  
  -- Calculate other metrics (no lending book in finalState)
  let positions = []  -- Would need lending book reference
  let totalLiquidity = 0.0  -- Simplified for minimal implementation
  
  -- Calculate volume (no action history in finalState)
  let tradeActions = []  -- Would need action history
  let totalVolume = 1000.0  -- Placeholder volume
  
  -- Calculate fees (assume 1% on volume)
  let totalFees = totalVolume * 0.01
  
  -- Calculate utilization (assume 50% for simulation)
  let averageUtilization = 0.5
  
  -- Define success based on price stability
  let scenarioSuccess = volatility < 0.2  -- Success if volatility under 20%
  
  pure { totalVolume: totalVolume
       , totalFees: totalFees
       , activePositions: length positions
       , totalUsers: length finalState.accounts
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
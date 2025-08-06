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
  let jitoSOLPrices = map _.impliedPrice $ filter (\obs -> obs.baseAsset == JitoSOL) finalState.priceHistory
  let prices = if length jitoSOLPrices > 0 then jitoSOLPrices else [config.initialJitoSOLPrice]
  let avgPrice = sum prices / Int.toNumber (length prices)
  let variance = sum (map (\p -> (p - avgPrice) * (p - avgPrice)) prices) / Int.toNumber (length prices)
  let volatility = FFI.sqrt variance / avgPrice
  
  -- Calculate other metrics (TODO: Fix when getTotalLiquidity signature is clarified)
  let totalLiquidity = 0.0  -- getTotalLiquidity finalState.lendingBook
  
  pure { totalVolume: 0.0  -- TODO: Track total volume
       , totalFees: 0.0    -- TODO: Track total fees  
       , activePositions: 0 -- TODO: Count active positions
       , totalUsers: length finalState.accounts
       , priceChange: priceChange
       , volatility: volatility
       , protocolTVL: totalLiquidity
       , averageUtilization: 0.0  -- TODO: Calculate utilization
       , scenarioSuccess: true     -- TODO: Define success criteria
       }
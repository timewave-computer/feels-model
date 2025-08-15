-- | Protocol Metric Module
-- |
-- | This module provides metrics and analytics for the protocol.
-- | It extracts key performance indicators, calculates protocol health metrics,
-- | and provides insights into system performance and utilization.
-- |
-- | Key Metrics:
-- | - Total Value Locked (TVL) across the protocol
-- | - POL reserves and utilization
-- | - Fee collection and distribution
-- | - Token-specific metrics including POL floors
-- | - System-wide performance indicators
module Protocol.Metric
  ( getPOLMetrics
  , POLMetrics
  , calculateGrowthRate24h
  , getFeelsSOLHealthMetrics
  , FeelsSOLHealthMetrics
  , getProtocolTotalFeesCollected
  , getOverallProtocolHealth
  , OverallHealthMetrics
  -- Vol Harvester metrics
  , calculateEstimatedLVRPayment
  , calculateVolatilityYield
  , processMonthlyPositionCompensation
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (read, Ref, modify_)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (fromFoldable, filter)
import Data.Map as Map
import Data.Foldable (foldl, sum, traverse_)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Control.Monad (when)

-- Import protocol types
import Protocol.POL (POLState, POLAllocation)
import Protocol.FeelsSOL (FeelsSOLState, getTotalLocked, getTotalMinted, getBufferStatus)
import Protocol.Pool (PoolState)
import Protocol.Position (Position, isVolHarvesterPosition)
import FFI (currentTime, sqrt, pow)

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

-- | POL system metrics
type POLMetrics =
  { totalValue :: Number
  , utilizationRate :: Number
  , allocated :: Number
  , unallocated :: Number
  , poolCount :: Int
  }

-- | FeelsSOL health metrics
type FeelsSOLHealthMetrics =
  { collateralRatio :: Number
  , totalLocked :: Number
  , totalMinted :: Number
  , bufferRatio :: Number
  , isHealthy :: Boolean
  }

-- | Overall protocol health metrics
type OverallHealthMetrics =
  { feelsSOLHealth :: FeelsSOLHealthMetrics
  , polMetrics :: POLMetrics
  , totalFeesCollected :: Number
  , overallHealth :: String  -- "Healthy", "Warning", "Critical"
  }

--------------------------------------------------------------------------------
-- POL METRICS
--------------------------------------------------------------------------------

-- | Calculate comprehensive POL system metrics
-- | Provides utilization rates, allocation status, and pool distribution
getPOLMetrics :: POLState -> Effect POLMetrics
getPOLMetrics polRef = do
  pol <- read polRef
  let allocations = fromFoldable $ Map.values pol.poolAllocations
      allocated = foldl (\acc alloc -> acc + alloc.allocated) 0.0 allocations
      utilization = if pol.totalPOL > 0.0 then allocated / pol.totalPOL else 0.0
  pure
    { totalValue: pol.totalPOL
    , utilizationRate: utilization
    , allocated: allocated
    , unallocated: pol.unallocated
    , poolCount: Map.size pol.poolAllocations
    }

-- | Calculate POL growth rate over the last 24 hours
-- | Returns percentage growth based on contribution history
calculateGrowthRate24h :: POLState -> Effect Number
calculateGrowthRate24h polRef = do
  pol <- read polRef
  now <- currentTime
  let dayAgo = now - 86400000.0  -- 24 hours in milliseconds
      recentContributions = filter (\c -> c.timestamp > dayAgo) pol.contributionHistory
      totalGrowth = sum $ map _.amount recentContributions
  -- Return growth as percentage of total POL
  pure $ if pol.totalPOL > 0.0 
         then (totalGrowth / pol.totalPOL) * 100.0
         else 0.0

--------------------------------------------------------------------------------
-- FEELSSOL HEALTH METRICS
--------------------------------------------------------------------------------

-- | Get FeelsSOL system health metrics including buffer status
-- | Provides a high-level view of the FeelsSOL system's status
getFeelsSOLHealthMetrics :: FeelsSOLState -> Effect FeelsSOLHealthMetrics
getFeelsSOLHealthMetrics state = do
  locked <- getTotalLocked state
  minted <- getTotalMinted state
  bufferStatus <- getBufferStatus state
  let ratio = if minted > 0.0 then locked / minted else 1.0  -- Calculate collateral ratio manually
      isHealthy = ratio >= 1.0 && bufferStatus.isHealthy  -- System healthy if collateralized and buffer adequate
  pure { collateralRatio: ratio, totalLocked: locked, totalMinted: minted, bufferRatio: bufferStatus.ratio, isHealthy }

--------------------------------------------------------------------------------
-- FEE METRICS
--------------------------------------------------------------------------------

-- | Calculate total fees collected across all pools
-- | Extracts aggregate fee revenue from the protocol
getProtocolTotalFeesCollected :: Array PoolState -> Effect Number
getProtocolTotalFeesCollected pools = do
  foldl collectPoolFees (pure 0.0) pools
  where
    collectPoolFees :: Effect Number -> PoolState -> Effect Number
    collectPoolFees totalEffect pool = do
      total <- totalEffect
      let totalFeeGrowth = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
          -- Convert fee growth to actual fees (simplified calculation)
          -- In reality, this would track the delta since last collection
          collectedFees = if pool.liquidity > 0.0 && totalFeeGrowth > 0.0
                         then min (totalFeeGrowth * pool.liquidity / 1000000.0) (pool.liquidity * 0.01)
                         else 0.0
      pure $ total + collectedFees

--------------------------------------------------------------------------------
-- OVERALL PROTOCOL HEALTH
--------------------------------------------------------------------------------

-- | Get overall protocol health by combining all subsystem metrics
-- | Provides a holistic view of the entire protocol's status
getOverallProtocolHealth :: FeelsSOLState -> POLState -> Array PoolState -> Effect OverallHealthMetrics
getOverallProtocolHealth feelsSOL polRef pools = do
  -- Get subsystem metrics
  feelsSOLHealth <- getFeelsSOLHealthMetrics feelsSOL
  polMetrics <- getPOLMetrics polRef
  totalFees <- getProtocolTotalFeesCollected pools
  
  -- Determine overall health status
  let overallHealth = 
        if feelsSOLHealth.isHealthy && polMetrics.utilizationRate > 0.2 && polMetrics.utilizationRate < 0.9
          then "Healthy"
        else if feelsSOLHealth.collateralRatio < 0.95 || polMetrics.utilizationRate > 0.95
          then "Critical"
        else "Warning"
  
  pure
    { feelsSOLHealth: feelsSOLHealth
    , polMetrics: polMetrics
    , totalFeesCollected: totalFees
    , overallHealth: overallHealth
    }

--------------------------------------------------------------------------------
-- LVR COMPENSATION METRICS
--------------------------------------------------------------------------------
-- Metrics for positions that provide liquidity with term commitments
-- These positions receive compensation for Loss Versus Rebalancing (LVR) and volatility yield

-- | Calculate estimated LVR payment for a monthly liquidity position
-- | This compensates liquidity providers for adverse selection costs
-- | Formula: LVR = liquidity * sqrt(time) * volatility^2 * factor
calculateEstimatedLVRPayment :: Position -> Effect Number
calculateEstimatedLVRPayment position = do
  -- Monthly positions qualify for LVR compensation
  -- This is just a helper calculation, the actual identification happens elsewhere
  let isEligible = true  -- Position identification is done by caller
  
  if not isEligible
    then pure 0.0
    else do
      -- Calculate time since position creation (in days)
      -- Assuming 2 blocks per minute (30 second blocks), 2880 blocks per day
      let positionAgeBlocks = toNumber position.createdAt
          timeDays = positionAgeBlocks / 2880.0
          
      -- Use position amount as proxy for liquidity provided
      let liquidity = position.amount
          
      -- Base volatility assumption (15% daily vol is typical for crypto)
      -- In production, this would come from oracle or historical data
      let dailyVolatility = 0.15
          
      -- LVR compensation factor (calibrated to provide fair compensation)
      -- This ensures LPs are compensated for the inherent cost of providing liquidity
      let lvrFactor = 0.0025  -- 0.25% of liquidity per sqrt(day) at 15% vol
          
      -- Calculate LVR payment
      -- Formula compensates for adverse selection over time
      let lvrPayment = liquidity * sqrt timeDays * pow dailyVolatility 2.0 * lvrFactor
          
      pure lvrPayment

-- | Calculate additional volatility yield for monthly liquidity positions
-- | This provides upside based on protocol's success in monetizing volatility
calculateVolatilityYield :: Position -> PoolState -> Effect Number
calculateVolatilityYield position pool = do
  -- Monthly positions qualify for volatility yield
  let isEligible = true  -- Position identification is done by caller
  
  if not isEligible
    then pure 0.0
    else do
      -- Position age is already tracked in block numbers
      let positionAgeBlocks = toNumber position.createdAt
          
      -- Extract fee growth since position creation
      -- This represents the protocol's success in monetizing volatility
      let feeGrowthDelta0 = pool.feeGrowthGlobal0X128 - position.feeGrowthInside0
          feeGrowthDelta1 = pool.feeGrowthGlobal1X128 - position.feeGrowthInside1
          totalFeeGrowth = feeGrowthDelta0 + feeGrowthDelta1
          
      -- Calculate position's share of fee growth
      -- Vol Harvesters get enhanced share due to their commitment
      let positionShare = if pool.liquidity > 0.0
                         then position.shares / pool.liquidity
                         else 0.0
                         
      -- Volatility yield multiplier for monthly positions
      -- They get 2x the normal fee share as compensation for locking liquidity
      let monthlyPositionMultiplier = 2.0
          
      -- Calculate total volatility yield
      let volatilityYield = totalFeeGrowth * positionShare * monthlyPositionMultiplier
          
      pure volatilityYield

--------------------------------------------------------------------------------
-- MONTHLY POSITION COMPENSATION PROCESSING
--------------------------------------------------------------------------------

-- | Process monthly positions and distribute LVR and volatility compensation
-- | This rewards all liquidity providers who commit to monthly term positions
-- | Takes a positions map reference and a pool getter function
-- | Returns unit after updating position values with compensation
processMonthlyPositionCompensation :: 
  Ref (Map.Map Int Position) -> 
  (String -> Effect (Maybe PoolState)) -> 
  Effect Unit
processMonthlyPositionCompensation positionsRef getPool = do
  -- Get all positions from position manager
  positionsMap <- read positionsRef
  let positions = Map.toUnfoldable positionsMap :: Array (Tuple Int Position)
  
  -- Process each position
  traverse_ (\(Tuple posId position) -> do
    -- Check if this position qualifies for LVR compensation (monthly duration)
    when (isVolHarvesterPosition position) do
      -- Calculate LVR compensation (guaranteed payment)
      lvrPayment <- calculateEstimatedLVRPayment position
      
      -- Get the pool for this position to calculate volatility yield
      -- For now, assume a default pool ID - in production this would be tracked
      poolResult <- getPool "FEELSSOL-USDC"
      
      case poolResult of
        Just pool -> do
          -- Calculate volatility yield (upside from protocol success)
          volYield <- calculateVolatilityYield position pool
          
          let totalCompensation = lvrPayment + volYield
          
          -- Update position with accumulated yield
          let updatedPosition = position 
                { accumulatedYield = position.accumulatedYield + totalCompensation
                , value = position.value + totalCompensation
                }
          
          -- Update position in state
          modify_ (Map.insert posId updatedPosition) positionsRef
          
          when (totalCompensation > 0.0) do
            log $ "Monthly position " <> show posId <> 
                  " LVR compensated: LVR=" <> show lvrPayment <> 
                  ", VolYield=" <> show volYield <>
                  ", Total=" <> show totalCompensation
        
        Nothing -> pure unit  -- Pool not found, skip
  ) positions


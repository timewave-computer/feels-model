-- | Protocol Vault Implementation - Protocol-level strategy and metrics
-- |
-- | This module manages the top-level protocol strategy that:
-- | - Collects protocol-wide metrics across all pools
-- | - Makes allocation decisions for POL distribution
-- | - Manages pool vaults and their parameters
-- | - Implements the protocol's capital allocation strategy
module Protocol.ProtocolVault
  ( -- Core types
    ProtocolVault
  , ProtocolStrategy
  , ProtocolMetrics
  , AllocationDecision
  , AllocationStrategy(..)
  -- Metric types (from old Metric.purs)
  , FeelsSOLHealthMetrics
  , POLMetrics
  -- Vault creation
  , createProtocolVault
  -- Balance sheet operations
  , getTotalProtocolPOL
  , getPoolAllocations
  , getProtocolPerformance
  -- State management
  , registerPoolVault
  , unregisterPoolVault
  , getPoolVault
  , updatePoolParameters
  , updateProtocolMetrics
  , calculateProtocolHealth
  , getProtocolMetrics
  , updateStrategyParameters
  -- Functions
  , allocateToPools
  , rebalancePOL
  , collectProtocolFees
  , executeAllocationStrategy
  -- Metric functions (from old Metric.purs)
  , getPOLMetrics
  , calculateGrowthRate24h
  , getFeelsSOLHealthMetrics
  , getProtocolTotalFeesCollected
  -- Vol Harvester metrics
  , calculateEstimatedLVRPayment
  , calculateVolatilityYield
  , processMonthlyPositionCompensation
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Array ((:), fromFoldable, filter, sortBy, take, find)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (sum, traverse_, foldM)
import Data.Ord (compare, min, max)
import Data.Int (toNumber)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (when)

import Protocol.Vault (Vault, VaultState, BalanceSheet, Asset, deposit, withdraw, allocate, updateStrategy, totalAssets, defaultSharePrice)
import Protocol.Token (TokenType(..))
import Protocol.Common (BlockNumber)
import Protocol.PoolVault (PoolVault, getPoolMetrics, updatePoolMetrics)
import Protocol.PositionVault (Position, isVolHarvesterPosition)
import Protocol.Pool (PoolState)
import Protocol.FeelsSOLVault (FeelsSOLState)
import Protocol.Vault as Vault
import FFI (currentTime, sqrt, pow)

--------------------------------------------------------------------------------
-- SECTION 1: BALANCE SHEET
--------------------------------------------------------------------------------

-- | FeelsSOL health metrics
type FeelsSOLHealthMetrics =
  { collateralRatio :: Number
  , totalLocked :: Number
  , totalMinted :: Number
  , bufferRatio :: Number
  , isHealthy :: Boolean
  }

-- | POL metrics (simplified, now part of ProtocolMetrics)
type POLMetrics = ProtocolMetrics

-- | Protocol-wide metrics (from Metric.purs)
type ProtocolMetrics =
  { totalValueLocked :: Number         -- Total TVL across protocol
  , totalPOL :: Number                 -- Total POL value
  , polUtilization :: Number           -- Overall POL utilization
  , totalFeesCollected :: Number       -- Lifetime fees collected
  , avgPoolPerformance :: Number       -- Average pool performance
  , poolCount :: Int                   -- Number of active pools
  , growthRate24h :: Number            -- 24h POL growth rate
  , healthScore :: Number              -- Overall protocol health (0-1)
  -- FeelsSOL metrics
  , feelsSOLCollateralRatio :: Number
  , feelsSOLMinted :: Number
  , jitoSOLLocked :: Number
  -- Risk metrics
  , systemRisk :: Number               -- Aggregate risk score
  , concentrationRisk :: Number        -- POL concentration risk
  }

-- | Allocation decision for a pool
type AllocationDecision =
  { poolId :: String
  , currentAllocation :: Number
  , targetAllocation :: Number
  , allocationDelta :: Number          -- Change needed
  , priority :: Number                 -- Urgency of reallocation
  }

-- | Allocation strategy types
data AllocationStrategy
  = PerformanceBased      -- Allocate based on pool performance
  | VolumeBased          -- Allocate based on trading volume
  | RiskAdjusted         -- Allocate considering risk scores
  | Balanced             -- Equal weight with performance tilt

derive instance eqAllocationStrategy :: Eq AllocationStrategy

-- | Get total protocol POL
getTotalProtocolPOL :: Ref ProtocolVault -> Effect Number
getTotalProtocolPOL protocolRef = do
  vault <- read protocolRef
  pure $ totalAssets vault.state.balanceSheet

-- | Get all pool allocations
getPoolAllocations :: Ref ProtocolVault -> Effect (Array PoolAllocation)
getPoolAllocations protocolRef = collectPoolAllocations protocolRef

-- | Get protocol performance metrics
getProtocolPerformance :: Ref ProtocolVault -> Effect 
  { totalPOL :: Number
  , utilization :: Number
  , growthRate24h :: Number
  , avgPoolPerformance :: Number
  }
getProtocolPerformance protocolRef = do
  metrics <- getProtocolMetrics protocolRef
  pure
    { totalPOL: metrics.totalPOL
    , utilization: metrics.polUtilization
    , growthRate24h: metrics.growthRate24h
    , avgPoolPerformance: metrics.avgPoolPerformance
    }

--------------------------------------------------------------------------------
-- SECTION 2: STATE
--------------------------------------------------------------------------------

-- | Performance history snapshot
type PerformanceSnapshot =
  { timestamp :: Number
  , totalPOL :: Number
  , utilization :: Number
  , feesCollected :: Number
  }

-- | Pool data for metrics collection
type PoolData =
  { pol :: Number
  , deployed :: Number
  , fees :: Number
  , performance :: Number
  }

-- | Pool allocation information
type PoolAllocation =
  { poolId :: String
  , allocation :: Number
  , performance :: Number
  }

-- | Allocation target for rebalancing
type AllocationTarget =
  { poolId :: String
  , target :: Number
  }

-- | Protocol-level strategy
type ProtocolStrategy =
  { allocationStrategy :: AllocationStrategy
  , poolVaults :: Map String (Ref PoolVault)  -- Pool ID -> Vault ref
  -- Allocation parameters
  , maxPoolAllocation :: Number        -- Max % per pool (e.g., 0.3)
  , minPoolAllocation :: Number        -- Min % per pool (e.g., 0.05)
  , rebalanceThreshold :: Number       -- Min delta to trigger rebalance
  , rebalanceFrequency :: Int          -- Blocks between rebalances
  , lastRebalanceBlock :: BlockNumber
  -- Risk parameters
  , maxSystemRisk :: Number            -- Max acceptable system risk
  , riskBuffer :: Number               -- Safety margin for allocations
  -- Fee parameters
  , protocolFeeShare :: Number         -- % of fees kept by protocol
  , feeDistributionRatio ::            -- How fees are distributed
    { stakers :: Number                -- To FeelsSOL stakers
    , pol :: Number                    -- To POL growth
    , treasury :: Number               -- To treasury
    }
  -- Performance tracking
  , performanceHistory :: Array PerformanceSnapshot  -- Historical snapshots
  }

-- | Protocol vault type
type ProtocolVault = Vault ProtocolStrategy

-- | Register a new pool vault
registerPoolVault :: Ref ProtocolVault -> String -> Ref PoolVault -> Effect Unit
registerPoolVault protocolRef poolId poolVaultRef = do
  modify_ (\vault ->
    let strategy = vault.state.strategyState
        newPoolVaults = Map.insert poolId poolVaultRef strategy.poolVaults
        updatedStrategy = strategy { poolVaults = newPoolVaults }
        updatedState = vault.state { strategyState = updatedStrategy }
    in vault { state = updatedState }
  ) protocolRef
  
  log $ "Registered pool vault for " <> poolId

-- | Unregister a pool vault
unregisterPoolVault :: Ref ProtocolVault -> String -> Effect Unit
unregisterPoolVault protocolRef poolId = do
  modify_ (\vault ->
    let strategy = vault.state.strategyState
        newPoolVaults = Map.delete poolId strategy.poolVaults
        updatedStrategy = strategy { poolVaults = newPoolVaults }
        updatedState = vault.state { strategyState = updatedStrategy }
    in vault { state = updatedState }
  ) protocolRef
  
  log $ "Unregistered pool vault for " <> poolId

-- | Get a pool vault reference
getPoolVault :: Ref ProtocolVault -> String -> Effect (Maybe (Ref PoolVault))
getPoolVault protocolRef poolId = do
  vault <- read protocolRef
  pure $ Map.lookup poolId vault.state.strategyState.poolVaults

-- | Update pool-specific parameters
updatePoolParameters :: 
  Ref ProtocolVault -> 
  String -> 
  { maxDeploymentRatio :: Number
  , minLiquidityBuffer :: Number
  } -> 
  Effect Unit
updatePoolParameters protocolRef poolId params = do
  vault <- read protocolRef
  case Map.lookup poolId vault.state.strategyState.poolVaults of
    Nothing -> log $ "Pool not found: " <> poolId
    Just poolRef -> do
      poolVault <- read poolRef
      let poolStrategy = poolVault.state.strategyState
          updatedStrategy = poolStrategy
            { maxDeploymentRatio = params.maxDeploymentRatio
            , minLiquidityBuffer = params.minLiquidityBuffer
            }
          updatedState = poolVault.state { strategyState = updatedStrategy }
      modify_ (\v -> v { state = updatedState }) poolRef

-- | Update protocol-wide metrics
updateProtocolMetrics :: Ref ProtocolVault -> Effect Unit
updateProtocolMetrics protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
  
  -- Collect metrics from all pools
  poolMetricsList <- traverse getPoolMetricsData (Map.toUnfoldable strategy.poolVaults)
  
  -- Calculate aggregate metrics
  let totalPOL = totalAssets vault.state.balanceSheet
      poolPOL = sum $ map (\m -> m.totalPOL) poolMetricsList
      totalDeployed = sum $ map (\m -> m.deployedAmount) poolMetricsList
      utilization = if totalPOL > 0.0 then totalDeployed / totalPOL else 0.0
      avgPerformance = if length poolMetricsList > 0
                      then sum (map (\m -> m.performance) poolMetricsList) / toNumber (length poolMetricsList)
                      else 0.0
  
  -- Update performance history
  now <- currentTime
  let newSnapshot = 
        { timestamp: now
        , totalPOL: totalPOL
        , utilization: utilization
        , feesCollected: sum $ map (\m -> m.totalFeesCollected) poolMetricsList
        }
      
      updatedHistory = take 100 (newSnapshot : strategy.performanceHistory)  -- Keep last 100
      updatedStrategy = strategy { performanceHistory = updatedHistory }
      updatedState = vault.state { strategyState = updatedStrategy }
  
  modify_ (\v -> v { state = updatedState }) protocolRef
  
  where
    getPoolMetricsData :: Tuple String (Ref PoolVault) -> Effect _
    getPoolMetricsData (Tuple _ poolRef) = getPoolMetrics poolRef

-- | Calculate protocol health score
calculateProtocolHealth :: Ref ProtocolVault -> Effect Number
calculateProtocolHealth protocolRef = do
  vault <- read protocolRef
  metrics <- getProtocolMetrics protocolRef
  
  let -- Health factors
      utilizationHealth = if metrics.polUtilization < 0.2
                         then 0.5  -- Too low
                         else if metrics.polUtilization > 0.9
                         then 0.7  -- Too high
                         else 1.0  -- Optimal range
      
      concentrationHealth = 1.0 - metrics.concentrationRisk
      
      growthHealth = if metrics.growthRate24h < 0.0
                    then 0.5
                    else min 1.0 (metrics.growthRate24h / 10.0)  -- 10% daily growth is max score
      
      performanceHealth = metrics.avgPoolPerformance
      
      -- Weighted health score
      healthScore = (utilizationHealth * 0.3 + 
                    concentrationHealth * 0.3 + 
                    growthHealth * 0.2 + 
                    performanceHealth * 0.2)
  
  pure healthScore

-- | Get comprehensive protocol metrics
getProtocolMetrics :: Ref ProtocolVault -> Effect ProtocolMetrics
getProtocolMetrics protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
      totalPOL = totalAssets vault.state.balanceSheet
  
  -- Collect pool data
  poolDataList <- traverse getPoolData (Map.toUnfoldable strategy.poolVaults)
  
  let totalDeployed = sum $ map (\d -> d.deployed) poolDataList
      totalFees = sum $ map (\d -> d.fees) poolDataList
      avgPerformance = if length poolDataList > 0
                      then sum (map (\d -> d.performance) poolDataList) / toNumber (length poolDataList)
                      else 0.0
      
      -- Calculate concentration risk (Herfindahl index)
      poolShares = map (\d -> if totalPOL > 0.0 then d.pol / totalPOL else 0.0) poolDataList
      concentrationRisk = sum $ map (\s -> s * s) poolShares
      
      -- Calculate 24h growth from history
      growthRate = calculateGrowthRate24h' strategy.performanceHistory
  
  pure
    { totalValueLocked: totalPOL  -- Simplified - would include all protocol TVL
    , totalPOL: totalPOL
    , polUtilization: if totalPOL > 0.0 then totalDeployed / totalPOL else 0.0
    , totalFeesCollected: totalFees
    , avgPoolPerformance: avgPerformance
    , poolCount: Map.size strategy.poolVaults
    , growthRate24h: growthRate
    , healthScore: 0.0  -- Will be calculated separately
    , feelsSOLCollateralRatio: 1.0  -- Would come from FeelsSOL vault
    , feelsSOLMinted: 0.0  -- Would come from FeelsSOL vault
    , jitoSOLLocked: 0.0  -- Would come from FeelsSOL vault
    , systemRisk: min 1.0 (concentrationRisk * 2.0)  -- Simplified risk calc
    , concentrationRisk: concentrationRisk
    }
  
  where
    getPoolData :: Tuple String (Ref PoolVault) -> Effect PoolData
    getPoolData (Tuple _ poolRef) = do
      metrics <- getPoolMetrics poolRef
      pure { pol: metrics.totalPOL, deployed: metrics.deployedAmount, fees: metrics.totalFeesCollected, performance: metrics.performance }
    
    calculateGrowthRate24h' :: Array PerformanceSnapshot -> Number
    calculateGrowthRate24h' [] = 0.0
    calculateGrowthRate24h' (current : rest) =
      let dayAgo = current.timestamp - 86400000.0
          oldSnapshot = find (\s -> s.timestamp <= dayAgo) rest
      in case oldSnapshot of
        Nothing -> 0.0
        Just old -> if old.totalPOL > 0.0
                   then ((current.totalPOL - old.totalPOL) / old.totalPOL) * 100.0
                   else 0.0

--------------------------------------------------------------------------------
-- SECTION 3: FUNCTIONS
--------------------------------------------------------------------------------

-- | Create the protocol vault
createProtocolVault :: String -> Effect (Ref ProtocolVault)
createProtocolVault name = do
  now <- currentTime
  
  -- Initialize strategy
  let initialStrategy =
        { allocationStrategy: PerformanceBased
        , poolVaults: Map.empty
        , maxPoolAllocation: 0.3           -- 30% max per pool
        , minPoolAllocation: 0.05          -- 5% min per pool
        , rebalanceThreshold: 0.02         -- 2% delta triggers rebalance
        , rebalanceFrequency: 100          -- Every 100 blocks
        , lastRebalanceBlock: 0
        , maxSystemRisk: 0.7               -- 70% max risk
        , riskBuffer: 0.1                  -- 10% safety margin
        , protocolFeeShare: 0.15           -- 15% of fees to protocol
        , feeDistributionRatio:
          { stakers: 0.4                   -- 40% to stakers
          , pol: 0.4                       -- 40% to POL
          , treasury: 0.2                  -- 20% to treasury
          }
        , performanceHistory: [{ timestamp: now, totalPOL: 10000.0, utilization: 0.0, feesCollected: 0.0 }]
        }
  
  -- Initial balance sheet with protocol reserves
  let initialBalanceSheet =
        { assets: [{ tokenType: FeelsSOL, amount: 10000.0, venue: "Protocol-Reserve" }]
        , liabilities: []
        , totalShares: 0.0
        }
      
      initialState =
        { balanceSheet: initialBalanceSheet
        , strategyState: initialStrategy
        , lastUpdateBlock: 0
        }
  
  -- Create vault ref
  vaultRef <- new (unsafeCoerce unit :: ProtocolVault)
  
  -- Create vault implementation
  let vault =
        { name: name
        , state: initialState
        
        -- Protocol vault accepts FeelsSOL deposits
        , deposit: \tokenType amount depositor -> do
            case tokenType of
              FeelsSOL -> do
                currentVault <- read vaultRef
                let result = deposit defaultSharePrice currentVault.state tokenType amount depositor
                modify_ (\v -> v { state = result.state }) vaultRef
                pure result.shares
              _ -> pure 0.0
        
        -- Withdraw from protocol reserves
        , withdraw: \shares withdrawer -> do
            currentVault <- read vaultRef
            case withdraw defaultSharePrice currentVault.state shares withdrawer of
              Nothing -> pure 0.0
              Just result -> do
                modify_ (\v -> v { state = result.state }) vaultRef
                pure result.amount
        
        -- Allocation strategy execution
        , allocate: do
            currentVault <- read vaultRef
            let currentBlock = currentVault.state.lastUpdateBlock
                strategy = currentVault.state.strategyState
            
            -- Check if rebalance is due
            when (currentBlock - strategy.lastRebalanceBlock >= strategy.rebalanceFrequency) do
              log "Protocol vault executing allocation strategy..."
              executeAllocationStrategyInternal vaultRef
        
        -- Update strategy parameters
        , updateStrategy: \newStrategy -> do
            modify_ (\v -> v { state = updateStrategy v.state newStrategy }) vaultRef
        
        -- Standard share pricing
        , sharePrice: defaultSharePrice
        }
  
  -- Write vault to ref
  _ <- write vault vaultRef
  pure vaultRef

-- | Allocate POL to pools based on strategy
allocateToPools :: Ref ProtocolVault -> Array AllocationDecision -> Effect Unit
allocateToPools protocolRef decisions = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
  
  -- Process each allocation decision
  traverse_ (processAllocation protocolRef) decisions
  
  -- Update last rebalance block
  let updatedStrategy = strategy { lastRebalanceBlock = vault.state.lastUpdateBlock }
      updatedState = vault.state { strategyState = updatedStrategy }
  modify_ (\v -> v { state = updatedState }) protocolRef
  
  where
    processAllocation :: Ref ProtocolVault -> AllocationDecision -> Effect Unit
    processAllocation pRef decision = do
      vault <- read pRef
      let strategy = vault.state.strategyState
      
      case Map.lookup decision.poolId strategy.poolVaults of
        Nothing -> log $ "Pool vault not found: " <> decision.poolId
        Just poolVaultRef -> do
          -- Transfer POL to/from pool vault
          if decision.allocationDelta > 0.0
            then do
              -- Allocate more to pool
              success <- transferToPool pRef poolVaultRef decision.allocationDelta
              when success do
                log $ "Allocated " <> show decision.allocationDelta <> " to " <> decision.poolId
            else if decision.allocationDelta < 0.0
            then do
              -- Withdraw from pool
              let withdrawAmount = abs decision.allocationDelta
              success <- transferFromPool pRef poolVaultRef withdrawAmount
              when success do
                log $ "Withdrew " <> show withdrawAmount <> " from " <> decision.poolId
            else pure unit  -- No change needed

-- | Rebalance POL across pools
rebalancePOL :: Ref ProtocolVault -> Effect Unit
rebalancePOL protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
  
  -- Collect current allocations
  allocations <- collectPoolAllocations protocolRef
  
  -- Calculate target allocations based on strategy
  let decisions = calculateAllocationDecisions strategy allocations
  
  -- Filter significant rebalances
  let significantDecisions = filter (\d -> abs d.allocationDelta > strategy.rebalanceThreshold * d.currentAllocation) decisions
  
  -- Execute rebalancing
  when (length significantDecisions > 0) do
    log $ "Rebalancing POL across " <> show (length significantDecisions) <> " pools"
    allocateToPools protocolRef significantDecisions

-- | Collect fees from all pool vaults
collectProtocolFees :: Ref ProtocolVault -> Effect Number
collectProtocolFees protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
      poolVaults = Map.toUnfoldable strategy.poolVaults :: Array (Tuple String (Ref PoolVault))
  
  -- Collect fees from each pool
  totalFees <- foldM collectFromPool 0.0 poolVaults
  
  when (totalFees > 0.0) do
    -- Distribute fees according to strategy
    let stakersShare = totalFees * strategy.feeDistributionRatio.stakers
        polShare = totalFees * strategy.feeDistributionRatio.pol
        treasuryShare = totalFees * strategy.feeDistributionRatio.treasury
    
    -- Add POL share to protocol reserves
    let protocolAssets = vault.state.balanceSheet.assets
        updatedAssets = map (\a ->
          if a.venue == "Protocol-Reserve"
          then a { amount = a.amount + polShare }
          else a
        ) protocolAssets
        
        updatedBS = vault.state.balanceSheet { assets = updatedAssets }
        updatedState = vault.state { balanceSheet = updatedBS }
    
    modify_ (\v -> v { state = updatedState }) protocolRef
    
    log $ "Collected " <> show totalFees <> " in fees. POL: " <> show polShare <>
          ", Stakers: " <> show stakersShare <> ", Treasury: " <> show treasuryShare
  
  pure totalFees
  
  where
    collectFromPool :: Number -> Tuple String (Ref PoolVault) -> Effect Number
    collectFromPool acc (Tuple poolId poolRef) = do
      metrics <- getPoolMetrics poolRef
      pure $ acc + metrics.totalFeesCollected * strategy.protocolFeeShare

-- | Execute allocation strategy
executeAllocationStrategy :: Ref ProtocolVault -> Effect Unit
executeAllocationStrategy = executeAllocationStrategyInternal

-- | Internal allocation strategy execution
executeAllocationStrategyInternal :: Ref ProtocolVault -> Effect Unit
executeAllocationStrategyInternal protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
  
  -- Collect current state
  allocations <- collectPoolAllocations protocolRef
  
  -- Calculate decisions based on strategy type
  let decisions = calculateAllocationDecisions strategy allocations
  
  -- Execute allocations
  allocateToPools protocolRef decisions
  
  -- Update metrics after allocation
  updateProtocolMetrics protocolRef

-- | Update strategy parameters
updateStrategyParameters :: Ref ProtocolVault -> ProtocolStrategy -> Effect Unit
updateStrategyParameters protocolRef newStrategy = do
  modify_ (\vault ->
    let updatedState = vault.state { strategyState = newStrategy }
    in vault { state = updatedState }
  ) protocolRef

--------------------------------------------------------------------------------
-- METRIC FUNCTIONS
--------------------------------------------------------------------------------

-- | Get POL metrics (delegates to getProtocolMetrics)
getPOLMetrics :: Ref ProtocolVault -> Effect ProtocolMetrics
getPOLMetrics = getProtocolMetrics

-- | Calculate POL growth rate over the last 24 hours
calculateGrowthRate24h :: Ref ProtocolVault -> Effect Number
calculateGrowthRate24h protocolRef = do
  vault <- read protocolRef
  let strategy = vault.state.strategyState
  now <- currentTime
  let dayAgo = now - 86400000.0  -- 24 hours in milliseconds
      history = strategy.performanceHistory
      
      -- Find entries from 24h ago and now
      currentPOL = case history of
        [] -> 0.0
        (h : _) -> h.totalPOL
        
      pol24hAgo = findPOL24hAgo dayAgo history
      
  -- Return growth as percentage of total POL
  pure $ if pol24hAgo > 0.0 
         then ((currentPOL - pol24hAgo) / pol24hAgo) * 100.0
         else 0.0
  where
    findPOL24hAgo :: Number -> Array PerformanceSnapshot -> Number
    findPOL24hAgo _ [] = 0.0
    findPOL24hAgo targetTime (h : rest) =
      if h.timestamp <= targetTime
      then h.totalPOL
      else findPOL24hAgo targetTime rest

-- | Get FeelsSOL system health metrics
getFeelsSOLHealthMetrics :: FeelsSOLState -> Effect FeelsSOLHealthMetrics
getFeelsSOLHealthMetrics vaultRef = do
  vault <- read vaultRef
  let locked = Vault.totalAssets vault.state.balanceSheet
      minted = vault.state.balanceSheet.totalShares
      strategy = vault.state.strategyState
      
      -- Calculate buffer status
      currentBuffer = strategy.jitoSOLBuffer
      target = locked * strategy.bufferTargetRatio
      bufferRatio = if locked > 0.0 then currentBuffer / locked else 0.0
      minRatio = 0.005  -- 0.5% minimum buffer
      bufferHealthy = bufferRatio >= minRatio
      
      -- Calculate collateral ratio
      ratio = if minted > 0.0 then locked / minted else 1.0
      isHealthy = ratio >= 1.0 && bufferHealthy
      
  pure { collateralRatio: ratio, totalLocked: locked, totalMinted: minted, bufferRatio: bufferRatio, isHealthy }

-- | Calculate total fees collected across all pools
getProtocolTotalFeesCollected :: Array PoolState -> Effect Number
getProtocolTotalFeesCollected pools = do
  foldl collectPoolFees (pure 0.0) pools
  where
    collectPoolFees :: Effect Number -> PoolState -> Effect Number
    collectPoolFees totalEffect pool = do
      total <- totalEffect
      let totalFeeGrowth = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
          -- Convert fee growth to actual fees (simplified calculation)
          collectedFees = if pool.liquidity > 0.0 && totalFeeGrowth > 0.0
                         then min (totalFeeGrowth * pool.liquidity / 1000000.0) (pool.liquidity * 0.01)
                         else 0.0
      pure $ total + collectedFees

--------------------------------------------------------------------------------
-- LVR COMPENSATION METRICS
--------------------------------------------------------------------------------

-- | Calculate estimated LVR payment for a monthly liquidity position
calculateEstimatedLVRPayment :: Position -> Number -> Effect Number
calculateEstimatedLVRPayment position poolVolatility = do
  -- Monthly positions qualify for LVR compensation
  let isEligible = true  -- Position identification is done by caller
  
  if not isEligible
    then pure 0.0
    else do
      -- Calculate time since position creation (in days)
      -- Assuming 2 blocks per minute (30 second blocks), 2880 blocks per day
      let timeDays = toNumber position.createdAt / 2880.0
          
      -- Use position amount as proxy for liquidity provided
      let liquidity = position.amount
          
      -- Use pool volatility for accurate LVR calculation
      let dailyVolatility = poolVolatility
          
      -- LVR compensation factor (calibrated to provide fair compensation)
      let lvrFactor = 0.0025  -- 0.25% of liquidity per sqrt(day) at 15% vol
          
      -- Calculate LVR payment
      let lvrPayment = liquidity * sqrt timeDays * pow dailyVolatility 2.0 * lvrFactor
          
      pure lvrPayment


-- | Calculate additional volatility yield for monthly liquidity positions
calculateVolatilityYield :: Position -> PoolState -> Effect Number
calculateVolatilityYield position pool = do
  -- Monthly positions qualify for volatility yield
  let isEligible = true  -- Position identification is done by caller
  
  if not isEligible
    then pure 0.0
    else do
      -- Extract fee growth since position creation
      let feeGrowthDelta0 = pool.feeGrowthGlobal0X128 - position.feeGrowthInside0
          feeGrowthDelta1 = pool.feeGrowthGlobal1X128 - position.feeGrowthInside1
          totalFeeGrowth = feeGrowthDelta0 + feeGrowthDelta1
          
      -- Calculate position's share of fee growth
      let positionShare = if pool.liquidity > 0.0
                         then position.shares / pool.liquidity
                         else 0.0
                         
      -- Volatility yield multiplier for monthly positions
      let monthlyPositionMultiplier = 2.0
          
      -- Calculate total volatility yield
      let volatilityYield = totalFeeGrowth * positionShare * monthlyPositionMultiplier
          
      pure volatilityYield

-- | Process monthly positions and distribute LVR and volatility compensation
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
      -- Get the pool for this position to calculate volatility yield
      poolResult <- getPool "FEELSSOL-USDC"
      
      case poolResult of
        Just pool -> do
          -- Get pool volatility for LVR calculation
          let poolVolatility = pool.volatility
          
          -- Calculate LVR compensation (guaranteed payment) with pool volatility
          lvrPayment <- calculateEstimatedLVRPayment position poolVolatility
          
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

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

-- | Transfer POL to a pool vault
transferToPool :: Ref ProtocolVault -> Ref PoolVault -> Number -> Effect Boolean
transferToPool protocolRef poolRef amount = do
  protocolVault <- read protocolRef
  let protocolAssets = protocolVault.state.balanceSheet.assets
      reserveAsset = find (\a -> a.venue == "Protocol-Reserve") protocolAssets
  
  case reserveAsset of
    Nothing -> pure false
    Just reserve -> 
      if reserve.amount < amount
        then pure false
        else do
          -- Update protocol vault assets
          let updatedProtocolAssets = map (\a ->
                if a.venue == "Protocol-Reserve"
                then a { amount = a.amount - amount }
                else a
              ) protocolAssets
              
              updatedProtocolBS = protocolVault.state.balanceSheet { assets = updatedProtocolAssets }
              updatedProtocolState = protocolVault.state { balanceSheet = updatedProtocolBS }
          
          modify_ (\v -> v { state = updatedProtocolState }) protocolRef
          
          -- Add to pool vault
          poolVault <- read poolRef
          let newPoolAsset = { tokenType: FeelsSOL, amount: amount, venue: "Pool-Unallocated" }
              updatedPoolAssets = newPoolAsset : poolVault.state.balanceSheet.assets
              updatedPoolBS = poolVault.state.balanceSheet { assets = updatedPoolAssets }
              updatedPoolState = poolVault.state { balanceSheet = updatedPoolBS }
          
          modify_ (\v -> v { state = updatedPoolState }) poolRef
          pure true

-- | Transfer POL from a pool vault back to protocol
transferFromPool :: Ref ProtocolVault -> Ref PoolVault -> Number -> Effect Boolean
transferFromPool protocolRef poolRef amount = do
  poolVault <- read poolRef
  let poolAssets = poolVault.state.balanceSheet.assets
      unallocatedAsset = find (\a -> a.venue == "Pool-Unallocated") poolAssets
  
  case unallocatedAsset of
    Nothing -> pure false
    Just unallocated ->
      if unallocated.amount < amount
        then pure false
        else do
          -- Update pool vault assets
          let updatedPoolAssets = map (\a ->
                if a.venue == "Pool-Unallocated"
                then a { amount = a.amount - amount }
                else a
              ) poolAssets
              
              updatedPoolBS = poolVault.state.balanceSheet { assets = filter (\a -> a.amount > 0.0) updatedPoolAssets }
              updatedPoolState = poolVault.state { balanceSheet = updatedPoolBS }
          
          modify_ (\v -> v { state = updatedPoolState }) poolRef
          
          -- Add back to protocol vault
          protocolVault <- read protocolRef
          let protocolAssets = protocolVault.state.balanceSheet.assets
              updatedProtocolAssets = map (\a ->
                if a.venue == "Protocol-Reserve"
                then a { amount = a.amount + amount }
                else a
              ) protocolAssets
              
              updatedProtocolBS = protocolVault.state.balanceSheet { assets = updatedProtocolAssets }
              updatedProtocolState = protocolVault.state { balanceSheet = updatedProtocolBS }
          
          modify_ (\v -> v { state = updatedProtocolState }) protocolRef
          pure true

-- | Collect current pool allocations
collectPoolAllocations :: Ref ProtocolVault -> Effect (Array PoolAllocation)
collectPoolAllocations protocolRef = do
  vault <- read protocolRef
  let poolVaults = Map.toUnfoldable vault.state.strategyState.poolVaults :: Array (Tuple String (Ref PoolVault))
  
  traverse (\(Tuple poolId poolRef) -> do
    metrics <- getPoolMetrics poolRef
    pure { poolId: poolId, allocation: metrics.totalPOL, performance: metrics.performance }
  ) poolVaults

-- | Calculate allocation decisions based on strategy
calculateAllocationDecisions :: 
  ProtocolStrategy -> 
  Array PoolAllocation -> 
  Array AllocationDecision
calculateAllocationDecisions strategy currentAllocations =
  let totalPOL = sum $ map (\a -> a.allocation) currentAllocations
      
      -- Calculate target allocations based on strategy type
      targetAllocations = case strategy.allocationStrategy of
        PerformanceBased -> calculatePerformanceBasedTargets
        VolumeBased -> calculateVolumeBasedTargets  -- Simplified
        RiskAdjusted -> calculateRiskAdjustedTargets  -- Simplified
        Balanced -> calculateBalancedTargets
      
      -- Create decisions
      decisions = map (\current ->
        let target = findTarget current.poolId targetAllocations
            targetAmount = target * totalPOL
            delta = targetAmount - current.allocation
            priority = abs delta / (current.allocation + 1.0)  -- Avoid div by 0
        in { poolId: current.poolId
           , currentAllocation: current.allocation
           , targetAllocation: targetAmount
           , allocationDelta: delta
           , priority: priority
           }
      ) currentAllocations
      
  in sortBy (\a b -> compare b.priority a.priority) decisions  -- Sort by priority desc
  
  where
    calculatePerformanceBasedTargets :: Array AllocationTarget
    calculatePerformanceBasedTargets =
      let totalPerformance = sum $ map (\a -> a.performance) currentAllocations
          rawTargets = map (\a ->
            { poolId: a.poolId
            , target: if totalPerformance > 0.0 
                     then a.performance / totalPerformance
                     else 1.0 / toNumber (length currentAllocations)
            }
          ) currentAllocations
          
          -- Apply min/max constraints
          constrainedTargets = map (\t ->
            t { target = min strategy.maxPoolAllocation (max strategy.minPoolAllocation t.target) }
          ) rawTargets
          
      in constrainedTargets
    
    calculateVolumeBasedTargets = calculatePerformanceBasedTargets  -- Placeholder
    calculateRiskAdjustedTargets = calculatePerformanceBasedTargets  -- Placeholder
    
    calculateBalancedTargets :: Array AllocationTarget
    calculateBalancedTargets =
      let poolCount = length currentAllocations
          baseAllocation = 1.0 / toNumber poolCount
          
          -- Add performance tilt
          targets = map (\a ->
            let performanceTilt = (a.performance - 0.5) * 0.2  -- +/- 10% based on performance
                target = baseAllocation * (1.0 + performanceTilt)
            in { poolId: a.poolId
               , target: min strategy.maxPoolAllocation (max strategy.minPoolAllocation target)
               }
          ) currentAllocations
          
      in targets
    
    findTarget :: String -> Array AllocationTarget -> Number
    findTarget poolId targets =
      case find (\t -> t.poolId == poolId) targets of
        Nothing -> strategy.minPoolAllocation
        Just t -> t.target
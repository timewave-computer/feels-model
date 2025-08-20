-- | Protocol Vault - Protocol-level capital management using ledger-based vault abstraction
-- |
-- | This module implements the protocol's capital management system as a ledger-based vault where:
-- | - Ledger entries track POL allocations to different pools
-- | - Each entry represents capital allocated to a specific pool with performance tracking
-- | - Strategy state manages allocation decisions and rebalancing
-- |
-- | Key Features:
-- | - Multi-pool capital allocation with performance tracking
-- | - Automatic rebalancing based on configurable strategies
-- | - Complete audit trail of all POL movements
-- | - Fee collection and distribution management
module Protocol.ProtocolVault
  ( -- Balance Sheet types
    ProtocolEntry
  , AllocationDecision
  , ProtocolMetrics
  , PerformanceSnapshot
  -- State types
  , ProtocolStrategy
  , AllocationStrategy(..)
  -- Vault creation functions
  , createProtocolVault
  , initializeProtocolVault
  -- POL management functions
  , allocatePOL
  , withdrawPOL
  , rebalancePools
  -- Fee management functions
  , collectProtocolFees
  , distributeFees
  -- Query functions
  , getProtocolMetrics
  , getPoolAllocation
  , calculateHealthScore
  -- Strategy management functions
  , updateAllocationStrategy
  , setRebalanceParameters
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array (filter, sortBy, find, take, mapWithIndex)
import Data.Array as Array
import Data.Foldable (sum, traverse_, foldM)
import Data.Ord (compare)
import Data.Number (abs)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Protocol.Vault 
  ( LedgerEntry(..), LedgerVaultState, LedgerVault, ShareAmount
  , class LedgerOps, createEmptyLedgerState, createLedgerVault
  , getAccountEntries, getAllAccounts, getActiveEntries
  , entryValue, isActive, aggregateEntries, applyStrategy
  )
import Protocol.Common (BlockNumber)
import Protocol.Token (TokenType(..))
import Protocol.Error (ProtocolError(..))
import Protocol.Config (defaultProtocolConfig)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- BALANCE SHEET
--------------------------------------------------------------------------------

-- | Protocol-specific ledger entry for POL allocations
type ProtocolEntry =
  { poolId :: String                -- Pool receiving allocation
  , polAmount :: ShareAmount         -- Amount of POL allocated
  , allocationBlock :: BlockNumber   -- When allocation was made
  , performance :: Number            -- Pool performance metric (0-1)
  , feesCollected :: Number          -- Fees collected from this allocation
  , isActive :: Boolean              -- Whether allocation is currently active
  }

-- | Allocation decision for rebalancing
type AllocationDecision =
  { poolId :: String
  , currentAllocation :: Number
  , targetAllocation :: Number
  , allocationDelta :: Number      -- Change needed
  , priority :: Number             -- Urgency of reallocation
  }

-- | Protocol-wide metrics
type ProtocolMetrics =
  { totalPOL :: Number                 -- Total POL under management
  , deployedPOL :: Number              -- POL allocated to pools
  , utilization :: Number              -- Deployment ratio
  , totalFeesCollected :: Number       -- Lifetime fees
  , avgPoolPerformance :: Number       -- Average performance across pools
  , poolCount :: Int                   -- Number of active pools
  , healthScore :: Number              -- Overall health (0-1)
  , concentrationRisk :: Number        -- Herfindahl index
  }

-- | Performance snapshot for tracking
type PerformanceSnapshot =
  { timestamp :: Number
  , totalPOL :: Number
  , utilization :: Number
  , feesCollected :: Number
  }

--------------------------------------------------------------------------------
-- STATE
--------------------------------------------------------------------------------

-- | Protocol strategy state
type ProtocolStrategy =
  { allocationStrategy :: AllocationStrategy
  -- Allocation parameters
  , maxPoolAllocation :: Number        -- Max % per pool
  , minPoolAllocation :: Number        -- Min % per pool
  , rebalanceThreshold :: Number       -- Min delta to trigger
  , rebalanceFrequency :: Int          -- Blocks between rebalances
  , lastRebalanceBlock :: BlockNumber
  -- Fee parameters
  , protocolFeeShare :: Number         -- % of fees to protocol
  , feeDistribution ::                 -- How to distribute fees
    { stakers :: Number
    , pol :: Number
    , treasury :: Number
    }
  -- Risk parameters
  , maxConcentrationRisk :: Number     -- Max acceptable concentration
  , minHealthScore :: Number           -- Min acceptable health
  -- Performance tracking
  , performanceHistory :: Array PerformanceSnapshot
  , totalProtocolFees :: Number        -- Total fees collected
  }

-- | Allocation strategy types
data AllocationStrategy
  = PerformanceBased      -- Allocate based on pool performance
  | VolumeBased          -- Allocate based on trading volume
  | RiskAdjusted         -- Allocate considering risk scores
  | Balanced             -- Equal weight with performance tilt

derive instance eqAllocationStrategy :: Eq AllocationStrategy

instance showAllocationStrategy :: Show AllocationStrategy where
  show PerformanceBased = "Performance"
  show VolumeBased = "Volume"
  show RiskAdjusted = "Risk-Adjusted"
  show Balanced = "Balanced"

-- | Ledger operations instance for protocol entries
instance ledgerOpsProtocol :: LedgerOps ProtocolEntry where
  -- Extract POL value from entry
  entryValue (LedgerEntry e) = e.data.polAmount
  
  -- Check if allocation is still active
  isActive _ (LedgerEntry e) = e.data.isActive
  
  -- Sum all POL amounts
  aggregateEntries entries = sum $ map (\(LedgerEntry e) -> e.data.polAmount) entries
  
  -- Apply rebalancing strategy
  applyStrategy strategy entries = 
    -- For now, no transformation
    -- Future: could mark underperforming allocations for withdrawal
    entries
  
  -- Select entries for withdrawal - prioritize underperforming allocations
  selectForWithdrawal requestedAmount entries =
    let
      -- Sort entries by performance (lowest first) with indices
      indexedEntries = Array.mapWithIndex (\idx entry -> { entry: entry, index: idx }) entries
      sortedByPerformance = Array.sortBy (\a b -> 
        case a.entry, b.entry of
          LedgerEntry ae, LedgerEntry be -> compare ae.data.performance be.data.performance
      ) indexedEntries
      
      -- Select entries starting from worst performers
      selectFromEntries :: Array { entry :: LedgerEntry ProtocolEntry, index :: Int } -> 
                          ShareAmount -> 
                          Array { entry :: LedgerEntry ProtocolEntry, index :: Int } -> 
                          Maybe (Array { entry :: LedgerEntry ProtocolEntry, index :: Int })
      selectFromEntries [] remaining selected =
        if remaining > 0.0 then Nothing else Just selected
      selectFromEntries (e : es) remaining selected =
        let value = entryValue e.entry
            newRemaining = remaining - value
        in if newRemaining <= 0.0
           then Just (e : selected)
           else selectFromEntries es newRemaining (e : selected)
    
    in selectFromEntries sortedByPerformance requestedAmount []

--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------

-- Vault Creation Functions

-- | Initialize a new protocol vault
initializeProtocolVault :: BlockNumber -> Effect (Either ProtocolError (Ref (LedgerVault ProtocolEntry ProtocolStrategy)))
initializeProtocolVault currentBlock = do
  currentTime' <- currentTime
  
  let initialStrategy =
        { allocationStrategy: PerformanceBased
        , maxPoolAllocation: defaultProtocolConfig.protocol.maxPoolAllocation
        , minPoolAllocation: defaultProtocolConfig.protocol.minPoolAllocation
        , rebalanceThreshold: defaultProtocolConfig.protocol.rebalanceThreshold
        , rebalanceFrequency: defaultProtocolConfig.protocol.rebalanceFrequency
        , lastRebalanceBlock: currentBlock
        , protocolFeeShare: defaultProtocolConfig.protocol.protocolFeeShare
        , feeDistribution:
          { stakers: defaultProtocolConfig.protocol.feeToStakers
          , pol: defaultProtocolConfig.protocol.feeToPOL
          , treasury: defaultProtocolConfig.protocol.feeToTreasury
          }
        , maxConcentrationRisk: defaultProtocolConfig.risk.maxConcentrationRisk
        , minHealthScore: defaultProtocolConfig.risk.minHealthScore
        , performanceHistory: [{ timestamp: currentTime', totalPOL: 0.0, utilization: 0.0, feesCollected: 0.0 }]
        , totalProtocolFees: 0.0
        }
      
      initialState = createEmptyLedgerState initialStrategy currentBlock
  
  vault <- createProtocolVault "Protocol" initialState
  pure $ Right vault

-- | Create protocol vault with ledger operations
createProtocolVault :: String -> LedgerVaultState ProtocolEntry ProtocolStrategy -> Effect (Ref (LedgerVault ProtocolEntry ProtocolStrategy))
createProtocolVault name initialState = do
  stateRef <- new initialState
  let vault = createLedgerVault name initialState stateRef
  new vault

-- POL Management Functions

-- | Allocate POL to a pool
allocatePOL ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  String ->           -- Pool ID
  Number ->           -- POL amount
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError ShareAmount)
allocatePOL vaultRef poolId polAmount currentBlock = do
  vault <- read vaultRef
  state <- read vault.state
  
  -- Check allocation limits
  let currentAllocation = getPoolAllocation' state poolId
      totalPOL = state.totalBalance
      newAllocationRatio = if totalPOL > 0.0 
                          then (currentAllocation + polAmount) / totalPOL
                          else 0.0
  
  if newAllocationRatio > state.strategyState.maxPoolAllocation
    then pure $ Left $ InvalidCommandError $ 
         "Allocation would exceed max pool allocation of " <> 
         show (state.strategyState.maxPoolAllocation * 100.0) <> "%"
    else do
      -- Create allocation entry
      let entry: ProtocolEntry =
            { poolId: poolId
            , polAmount: polAmount
            , allocationBlock: currentBlock
            , performance: 0.5  -- Initial neutral performance
            , feesCollected: 0.0
            , isActive: true
            }
      
      -- Record allocation
      result <- vault.deposit poolId entry currentBlock
      
      -- Update performance history
      updatePerformanceHistory vaultRef currentBlock
      
      pure $ Right result.shares

-- | Withdraw POL from a pool
withdrawPOL ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  String ->           -- Pool ID
  ShareAmount ->      -- Amount to withdraw
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError Number)
withdrawPOL vaultRef poolId amount currentBlock = do
  vault <- read vaultRef
  
  -- Check pool has sufficient allocation
  state <- read vault.state
  let poolAllocation = getPoolAllocation' state poolId
  
  if poolAllocation < amount
    then pure $ Left $ InvalidCommandError "Insufficient POL in pool"
    else do
      -- Process withdrawal
      result <- vault.withdraw poolId amount currentBlock
      case result of
        Nothing -> pure $ Left $ InvalidCommandError "Withdrawal failed"
        Just withdrawResult -> do
          -- Mark affected entries as inactive
          updatePoolEntries vaultRef poolId currentBlock
          
          -- Update performance history
          updatePerformanceHistory vaultRef currentBlock
          
          pure $ Right withdrawResult.amount

-- | Rebalance POL across pools
rebalancePools :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> BlockNumber -> Effect Unit
rebalancePools vaultRef currentBlock = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
  
  -- Check if rebalance is due
  if currentBlock - strategy.lastRebalanceBlock < strategy.rebalanceFrequency
    then pure unit
    else do
      -- Calculate current allocations
      let allocations = calculateCurrentAllocations state
          totalPOL = state.totalBalance
      
      -- Calculate target allocations based on strategy
      let decisions = calculateAllocationDecisions strategy allocations totalPOL
      
      -- Filter significant rebalances
      let significantDecisions = filter (\d -> 
            abs d.allocationDelta > strategy.rebalanceThreshold * d.currentAllocation
          ) decisions
      
      -- Execute rebalancing
      traverse_ (executeRebalance vaultRef currentBlock) significantDecisions
      
      -- Update last rebalance block
      let newStrategy = strategy { lastRebalanceBlock = currentBlock }
      vault.updateStrategy newStrategy
      
      log $ "Rebalanced " <> show (Array.length significantDecisions) <> " pools"

-- Fee Management Functions

-- | Collect protocol fees from pools
collectProtocolFees ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  Array { poolId :: String, fees :: Number } ->
  BlockNumber ->
  Effect Number
collectProtocolFees vaultRef poolFees currentBlock = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
  
  -- Update fee data for each pool
  totalFees <- foldM (\acc feeData -> do
    -- Update pool entries with fee information
    updatePoolFees vaultRef feeData.poolId feeData.fees
    pure $ acc + feeData.fees * strategy.protocolFeeShare
  ) 0.0 poolFees
  
  -- Update total protocol fees
  let newStrategy = strategy { totalProtocolFees = strategy.totalProtocolFees + totalFees }
  vault.updateStrategy newStrategy
  
  pure totalFees

-- | Distribute collected fees
distributeFees ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  Effect { stakers :: Number, pol :: Number, treasury :: Number }
distributeFees vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      totalFees = strategy.totalProtocolFees
      distribution = strategy.feeDistribution
  
  let stakersAmount = totalFees * distribution.stakers
      polAmount = totalFees * distribution.pol
      treasuryAmount = totalFees * distribution.treasury
  
  -- Reset distributed fees
  let newStrategy = strategy { totalProtocolFees = 0.0 }
  vault.updateStrategy newStrategy
  
  pure { stakers: stakersAmount, pol: polAmount, treasury: treasuryAmount }

-- Query Functions

-- | Get comprehensive protocol metrics
getProtocolMetrics :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> Effect ProtocolMetrics
getProtocolMetrics vaultRef = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
      activeEntries = getActiveEntries state state.lastUpdateBlock
  
  -- Calculate metrics
  let totalPOL = state.totalBalance
      deployedPOL = aggregateEntries activeEntries
      utilization = if totalPOL > 0.0 then deployedPOL / totalPOL else 0.0
      
      -- Pool metrics
      poolAllocations = calculateCurrentAllocations state
      poolCount = Array.length poolAllocations
      
      -- Performance metrics
      performances = map _.performance poolAllocations
      avgPerformance = if poolCount > 0
                      then sum performances / toNumber poolCount
                      else 0.0
      
      -- Risk metrics
      concentrationRisk = calculateConcentrationRisk poolAllocations totalPOL
      healthScore = calculateHealthScore' utilization avgPerformance concentrationRisk
  
  pure
    { totalPOL: totalPOL
    , deployedPOL: deployedPOL
    , utilization: utilization
    , totalFeesCollected: sum $ map (\(LedgerEntry e) -> e.data.feesCollected) activeEntries
    , avgPoolPerformance: avgPerformance
    , poolCount: poolCount
    , healthScore: healthScore
    , concentrationRisk: concentrationRisk
    }

-- | Get allocation for a specific pool
getPoolAllocation :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> String -> Effect Number
getPoolAllocation vaultRef poolId = do
  vault <- read vaultRef
  state <- read vault.state
  pure $ getPoolAllocation' state poolId

-- | Calculate protocol health score
calculateHealthScore :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> Effect Number
calculateHealthScore vaultRef = do
  metrics <- getProtocolMetrics vaultRef
  pure metrics.healthScore

-- Strategy Management Functions

-- | Update allocation strategy
updateAllocationStrategy ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  AllocationStrategy ->
  Effect Unit
updateAllocationStrategy vaultRef newStrategy = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState { allocationStrategy = newStrategy }
  vault.updateStrategy strategy

-- | Set rebalance parameters
setRebalanceParameters ::
  Ref (LedgerVault ProtocolEntry ProtocolStrategy) ->
  { threshold :: Number, frequency :: Int } ->
  Effect Unit
setRebalanceParameters vaultRef params = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
        { rebalanceThreshold = params.threshold
        , rebalanceFrequency = params.frequency
        }
  vault.updateStrategy strategy

-- Helper Functions

-- | Get pool allocation from state
getPoolAllocation' :: LedgerVaultState ProtocolEntry ProtocolStrategy -> String -> Number
getPoolAllocation' state poolId =
  let entries = getAccountEntries state poolId
      activeEntries = filter (isActive state.lastUpdateBlock) entries
  in aggregateEntries activeEntries

-- | Calculate current allocations for all pools
calculateCurrentAllocations :: LedgerVaultState ProtocolEntry ProtocolStrategy -> Array { poolId :: String, allocation :: Number, performance :: Number }
calculateCurrentAllocations state =
  let poolIds = getAllAccounts state
  in map (\poolId ->
    let entries = getAccountEntries state poolId
        activeEntries = filter (isActive state.lastUpdateBlock) entries
        allocation = aggregateEntries activeEntries
        -- Average performance of active entries
        performances = map (\(LedgerEntry e) -> e.data.performance) activeEntries
        avgPerformance = if Array.length performances > 0
                        then sum performances / toNumber (Array.length performances)
                        else 0.5
    in { poolId: poolId, allocation: allocation, performance: avgPerformance }
  ) poolIds

-- | Calculate allocation decisions
calculateAllocationDecisions ::
  ProtocolStrategy ->
  Array { poolId :: String, allocation :: Number, performance :: Number } ->
  Number ->
  Array AllocationDecision
calculateAllocationDecisions strategy currentAllocations totalPOL =
  let targets = case strategy.allocationStrategy of
        PerformanceBased -> calculatePerformanceTargets currentAllocations totalPOL strategy
        VolumeBased -> calculatePerformanceTargets currentAllocations totalPOL strategy  -- Simplified
        RiskAdjusted -> calculatePerformanceTargets currentAllocations totalPOL strategy  -- Simplified
        Balanced -> calculateBalancedTargets currentAllocations totalPOL strategy
  
  in map (\alloc ->
    let target = fromMaybe alloc.allocation $ find (\t -> t.poolId == alloc.poolId) targets
        delta = target - alloc.allocation
        priority = abs delta / (alloc.allocation + 1.0)
    in { poolId: alloc.poolId
       , currentAllocation: alloc.allocation
       , targetAllocation: target
       , allocationDelta: delta
       , priority: priority
       }
  ) currentAllocations

-- | Calculate performance-based targets
calculatePerformanceTargets ::
  Array { poolId :: String, allocation :: Number, performance :: Number } ->
  Number ->
  ProtocolStrategy ->
  Array { poolId :: String, allocation :: Number }
calculatePerformanceTargets allocations totalPOL strategy =
  let totalPerformance = sum $ map _.performance allocations
  in map (\alloc ->
    let rawTarget = if totalPerformance > 0.0
                   then alloc.performance / totalPerformance
                   else 1.0 / toNumber (Array.length allocations)
        constrainedTarget = min strategy.maxPoolAllocation (max strategy.minPoolAllocation rawTarget)
        targetAmount = constrainedTarget * totalPOL
    in { poolId: alloc.poolId, allocation: targetAmount }
  ) allocations

-- | Calculate balanced targets
calculateBalancedTargets ::
  Array { poolId :: String, allocation :: Number, performance :: Number } ->
  Number ->
  ProtocolStrategy ->
  Array { poolId :: String, allocation :: Number }
calculateBalancedTargets allocations totalPOL strategy =
  let poolCount = Array.length allocations
      baseAllocation = 1.0 / toNumber poolCount
  in map (\alloc ->
    let performanceTilt = (alloc.performance - 0.5) * 0.2  -- +/- 10% based on performance
        rawTarget = baseAllocation * (1.0 + performanceTilt)
        constrainedTarget = min strategy.maxPoolAllocation (max strategy.minPoolAllocation rawTarget)
        targetAmount = constrainedTarget * totalPOL
    in { poolId: alloc.poolId, allocation: targetAmount }
  ) allocations

-- | Execute a single rebalance decision
executeRebalance :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> BlockNumber -> AllocationDecision -> Effect Unit
executeRebalance vaultRef currentBlock decision = do
  if decision.allocationDelta > 0.0
    then do
      -- Allocate more to pool
      _ <- allocatePOL vaultRef decision.poolId decision.allocationDelta currentBlock
      pure unit
    else if decision.allocationDelta < 0.0
    then do
      -- Withdraw from pool
      _ <- withdrawPOL vaultRef decision.poolId (abs decision.allocationDelta) currentBlock
      pure unit
    else pure unit

-- | Update pool entries after changes
updatePoolEntries :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> String -> BlockNumber -> Effect Unit
updatePoolEntries vaultRef poolId currentBlock = do
  -- This would mark entries as inactive or update their status
  -- For now, this is a placeholder
  pure unit

-- | Update pool fee information
updatePoolFees :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> String -> Number -> Effect Unit
updatePoolFees vaultRef poolId fees = do
  -- This would update fee tracking in entries
  -- For now, this is a placeholder
  pure unit

-- | Update performance history
updatePerformanceHistory :: Ref (LedgerVault ProtocolEntry ProtocolStrategy) -> BlockNumber -> Effect Unit
updatePerformanceHistory vaultRef currentBlock = do
  vault <- read vaultRef
  state <- read vault.state
  let strategy = state.strategyState
  
  currentTime' <- currentTime
  metrics <- getProtocolMetrics vaultRef
  
  let newSnapshot =
        { timestamp: currentTime'
        , totalPOL: metrics.totalPOL
        , utilization: metrics.utilization
        , feesCollected: metrics.totalFeesCollected
        }
      
      updatedHistory = take 100 (newSnapshot : strategy.performanceHistory)
      newStrategy = strategy { performanceHistory: updatedHistory }
  
  vault.updateStrategy newStrategy

-- | Calculate concentration risk (Herfindahl index)
calculateConcentrationRisk ::
  Array { poolId :: String, allocation :: Number, performance :: Number } ->
  Number ->
  Number
calculateConcentrationRisk allocations totalPOL =
  if totalPOL <= 0.0
    then 0.0
    else
      let shares = map (\a -> a.allocation / totalPOL) allocations
      in sum $ map (\s -> s * s) shares

-- | Calculate health score from components
calculateHealthScore' :: Number -> Number -> Number -> Number
calculateHealthScore' utilization performance concentration =
  let utilizationHealth = if utilization < 0.2
                         then 0.5  -- Too low
                         else if utilization > 0.9
                         then 0.7  -- Too high
                         else 1.0  -- Optimal
      
      concentrationHealth = 1.0 - concentration
      
      -- Weighted health score
      healthScore = (utilizationHealth * 0.4 + 
                    performance * 0.4 + 
                    concentrationHealth * 0.2)
  
  in healthScore
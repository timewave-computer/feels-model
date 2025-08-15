-- | Protocol Clock Module
-- |
-- | This module handles the protocol's main event loop and time-based operations:
-- | - Block progression and timestamp management
-- | - Fee collection from pools and contribution to POL
-- | - POL distribution to pools based on performance and market conditions
-- | - Dynamic pool health monitoring and fee adjustments
-- | - Processing pending orders and state updates
-- |
-- | The protocol event loop represents the blockchain's block-by-block execution,
-- | processing transactions and updating state autonomously.
module Protocol.Clock
  ( runProtocolBlock
  , updatePoolHealthMetrics
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), snd)
import Data.Array (length, (:))
import Data.Foldable (sum, foldM, traverse_)
import Data.Int (toNumber)
import Data.Ord (abs, min, max)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref, read, write, modify_)
import Effect.Console (log)
import Control.Monad (when)

-- Import protocol types and functions
import Protocol.POL (POLState, getAllAllocations, distributePOL)
import Protocol.Pool (PoolState)
import UI.PoolRegistry (PoolRegistry, getAllPools, updatePool)
import UI.ProtocolState (ProtocolState)
import Protocol.Oracle (updatePriceWithTimestamp, takeMarketSnapshot)
import Protocol.Incentive (collectAndContributePoolFees)
import Simulation.Scenario (MarketScenario(..))
import FFI (currentTime)

--------------------------------------------------------------------------------
-- PROTOCOL EVENT LOOP
--------------------------------------------------------------------------------

-- | Execute a single protocol block
-- | This represents one block on the blockchain, processing all protocol operations
-- | Returns the POL allocation snapshot for this block
runProtocolBlock :: 
  Ref ProtocolState -> 
  Number ->              -- Oracle price for this block
  MarketScenario ->      -- Current market conditions
  Int ->                 -- Block number
  Effect { timestamp :: Number, allocations :: Map.Map String Number }
runProtocolBlock protocolRef oraclePrice scenario blockNum = do
  log $ "\n=== Protocol Block " <> show blockNum <> " ==="
  
  -- Step 1: Update block number and timestamp
  baseTime <- currentTime
  let blockTimestamp = baseTime + (toNumber blockNum * 5000.0)  -- 5 seconds per block
  
  modify_ (\s -> s { currentBlock = blockNum, timestamp = blockTimestamp }) protocolRef
  
  -- Step 2: Update oracle price
  protocolState <- read protocolRef
  _ <- updatePriceWithTimestamp oraclePrice blockTimestamp protocolState.oracle
  log $ "Oracle price updated to: " <> show oraclePrice
  
  -- Step 3: Collect and contribute fees to POL
  collectAndContributePoolFees protocolState
  
  -- Step 4: Distribute POL based on market conditions
  -- Convert pool data to metrics for POL distribution
  pools <- getAllPools protocolState.poolRegistry
  let poolMetrics = map (\(Tuple poolId pool) -> 
        { poolId: poolId
        , liquidity: pool.liquidity
        , volume: pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
        }
      ) pools
  distributePOL protocolState.polState scenario poolMetrics blockNum
  
  -- Step 5: Update pool health metrics
  updatePoolHealthMetrics protocolState
  
  -- Step 6: Capture POL allocation snapshot
  polAllocations <- getAllAllocations protocolState.polState
  let polAllocationMap = Map.fromFoldable $ map (\alloc -> Tuple alloc.poolId alloc.permanentAllocated) polAllocations
  
  -- Step 7: Update protocol state's price history
  marketSnapshot <- takeMarketSnapshot protocolState.oracle
  modify_ (\s -> s { priceHistory = { timestamp: blockTimestamp, block: blockNum, price: marketSnapshot.spot, polValue: 0.0 } : s.priceHistory }) protocolRef
  
  log $ "Protocol block " <> show blockNum <> " completed"
  
  pure { timestamp: blockTimestamp, allocations: polAllocationMap }

--------------------------------------------------------------------------------
-- POOL HEALTH METRICS
--------------------------------------------------------------------------------

-- | Update pool health metrics and fee parameters
-- | Adjusts fees based on liquidity depth and market conditions
updatePoolHealthMetrics :: forall r. { poolRegistry :: PoolRegistry | r } -> Effect Unit
updatePoolHealthMetrics state = do
  pools <- getAllPools state.poolRegistry
  
  traverse_ (\(Tuple poolId pool) -> do
    -- Calculate pool health based on liquidity and volume
    let liquidityDepth = pool.liquidity
        volume = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
        
        -- Health score: 0.0 (unhealthy) to 1.0 (very healthy)
        liquidityScore = min 1.0 (liquidityDepth / 10000.0)  -- Normalize to 10k liquidity
        volumeScore = min 1.0 (volume / 100.0)               -- Normalize to 100 volume
        healthScore = (liquidityScore + volumeScore) / 2.0
        
        -- Dynamic fee adjustment based on health
        -- Lower fees for healthy pools to attract volume
        -- Higher fees for unhealthy pools to compensate LPs
        baseFee = pool.protocolFee
        adjustedFee = if healthScore > 0.8
                      then max 10.0 (baseFee * 0.8)    -- 20% fee reduction for very healthy pools
                      else if healthScore < 0.3
                           then min 50.0 (baseFee * 1.5) -- 50% fee increase for unhealthy pools
                           else baseFee
    
    -- Update pool if fee changed significantly
    when (abs (adjustedFee - baseFee) > 1.0) do
      let updatedPool = pool { protocolFee = adjustedFee }
      updatePool poolId updatedPool state.poolRegistry
      log $ "Updated pool " <> poolId <> " health score: " <> show healthScore <> 
            ", fee: " <> show baseFee <> " -> " <> show adjustedFee
  ) pools
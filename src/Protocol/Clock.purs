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
  , ProtocolBlockResult
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array ((:))
import Data.Int (toNumber)
import Data.Map as Map
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Console (log)
import Control.Monad (when)

-- Import protocol types and functions
import Protocol.POL (POLState, getAllAllocations, distributePOL)
import Protocol.Pool (PoolState, updatePoolHealthMetrics)
import Protocol.Oracle (Oracle, updatePriceWithTimestamp)
import Protocol.Metric (processMonthlyPositionCompensation)
import Protocol.Position (Position)
import Protocol.Incentive (collectFeesFromPools, PoolFeeData)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

-- | Result of running a protocol block
type ProtocolBlockResult =
  { timestamp :: Number
  , allocations :: Map.Map String Number
  }

--------------------------------------------------------------------------------
-- PROTOCOL EVENT LOOP
--------------------------------------------------------------------------------

-- | Execute a single protocol block
-- | This represents one block on the blockchain, processing all protocol operations
-- | Returns the POL allocation snapshot for this block
-- |
-- | Parameters:
-- | - oracle: The price oracle for updating prices
-- | - polState: The POL state for managing protocol liquidity
-- | - pools: Array of pool states to process
-- | - positionsRef: Reference to positions map for LVR compensation
-- | - getPool: Function to retrieve a pool by ID
-- | - updatePool: Function to update a pool state
-- | - oraclePrice: The oracle price for this block
-- | - blockNum: The current block number
runProtocolBlock :: 
  { oracle :: Oracle
  , polState :: POLState
  , pools :: Array (Tuple String PoolState)
  , positionsRef :: Ref (Map.Map Int Position)
  , getPool :: String -> Effect (Maybe PoolState)
  , updatePool :: String -> PoolState -> Effect Unit
  } ->
  Number ->              -- Oracle price for this block
  Int ->                 -- Block number
  Effect ProtocolBlockResult
runProtocolBlock params oraclePrice blockNum = do
  log $ "\n=== Protocol Block " <> show blockNum <> " ==="
  
  -- Step 1: Calculate block timestamp
  baseTime <- currentTime
  let blockTimestamp = baseTime + (toNumber blockNum * 5000.0)  -- 5 seconds per block
  
  -- Step 2: Update oracle price
  updatePriceWithTimestamp oraclePrice blockTimestamp params.oracle
  log $ "Oracle price updated to: " <> show oraclePrice
  
  -- Step 3: Collect fees from pools and contribute to POL
  let poolFeeData = map (\(Tuple poolId pool) -> 
        { poolId: poolId
        , liquidity: pool.liquidity
        , feeGrowthGlobal0X128: pool.feeGrowthGlobal0X128
        , feeGrowthGlobal1X128: pool.feeGrowthGlobal1X128
        }
      ) params.pools
  collectFeesFromPools params.polState poolFeeData
  
  -- Step 4: Distribute POL allocations based on current scenario and pool metrics
  -- Convert pool data to metrics for POL distribution
  let poolMetrics = map (\(Tuple poolId pool) -> 
        { poolId: poolId
        , liquidity: pool.liquidity
        , volume: pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
        }
      ) params.pools
  distributePOL params.polState poolMetrics blockNum
  
  -- Step 5: Process monthly positions and distribute LVR compensation
  processMonthlyPositionCompensation params.positionsRef params.getPool
  
  -- Step 6: Update pool health metrics
  traverse_ (\(Tuple poolId pool) -> do
    let updatedPool = updatePoolHealthMetrics pool
    -- Only update if fees changed
    when (pool.protocolFee /= updatedPool.protocolFee) do
      params.updatePool poolId updatedPool
      log $ "Updated pool " <> poolId <> " fee: " <> show pool.protocolFee <> " -> " <> show updatedPool.protocolFee
  ) params.pools
  
  -- Step 7: Capture POL allocation snapshot
  polAllocations <- getAllAllocations params.polState
  let polAllocationMap = Map.fromFoldable $ map (\alloc -> Tuple alloc.poolId alloc.permanentAllocated) polAllocations
  
  log $ "Protocol block " <> show blockNum <> " completed"
  
  pure { timestamp: blockTimestamp, allocations: polAllocationMap }
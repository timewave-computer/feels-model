-- | Pool registry management for the UI.
-- | Provides centralized management of pools and positions.
-- | In Solana, pools and positions would be Program Derived Addresses (PDAs).
module UI.PoolRegistry
  ( PoolRegistry
  , initPoolRegistry
  , addPool
  , removePool
  , getPool
  , getAllPools
  , getNextPositionId
  -- Position management
  , addPosition
  , removePosition
  , getPosition
  , getUserPositions
  , getAllPositions
  , getPoolPositions
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Array ((:), filter, concat, mapMaybe, fromFoldable)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Protocol.Pool (PoolState)
import Protocol.Position (Position)
import Protocol.Common (PoolId, PositionId)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Global registry of all pools and positions
type PoolRegistry = Ref
  { pools :: Map PoolId PoolState           -- All pools by ID
  , positions :: Map PositionId Position    -- All positions by ID
  , nextPositionId :: PositionId            -- Next available position ID
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize empty pool registry
initPoolRegistry :: Effect PoolRegistry
initPoolRegistry = new
  { pools: Map.empty
  , positions: Map.empty
  , nextPositionId: 1
  }

--------------------------------------------------------------------------------
-- Pool Management
--------------------------------------------------------------------------------

-- | Add a new pool to the registry
addPool :: PoolId -> PoolState -> PoolRegistry -> Effect Unit
addPool poolId pool registryRef = do
  modify_ (\reg -> reg { pools = Map.insert poolId pool reg.pools }) registryRef

-- | Remove a pool from the registry
removePool :: PoolId -> PoolRegistry -> Effect Unit
removePool poolId registryRef = do
  modify_ (\reg -> reg { pools = Map.delete poolId reg.pools }) registryRef

-- | Get a specific pool
getPool :: PoolId -> PoolRegistry -> Effect (Maybe PoolState)
getPool poolId registryRef = do
  registry <- read registryRef
  pure $ Map.lookup poolId registry.pools

-- | Get all pools
getAllPools :: PoolRegistry -> Effect (Array (Tuple PoolId PoolState))
getAllPools registryRef = do
  registry <- read registryRef
  pure $ Map.toUnfoldable registry.pools

--------------------------------------------------------------------------------
-- Position ID Management
--------------------------------------------------------------------------------

-- | Get next position ID and increment counter
getNextPositionId :: PoolRegistry -> Effect PositionId
getNextPositionId registryRef = do
  registry <- read registryRef
  let nextId = registry.nextPositionId
  modify_ (\reg -> reg { nextPositionId = nextId + 1 }) registryRef
  pure nextId

--------------------------------------------------------------------------------
-- Position Management
--------------------------------------------------------------------------------

-- | Add a position to the registry and its pool
addPosition :: Position -> PoolRegistry -> Effect Unit
addPosition position registryRef = do
  -- Add to global position map
  modify_ (\reg -> reg 
    { positions = Map.insert position.id position reg.positions 
    }) registryRef
  
  -- Add position ID to pool's position book
  registry <- read registryRef
  case Map.lookup position.poolId registry.pools of
    Just pool -> do
      let updatedPool = pool 
            { positions = pool.positions 
              { positionIds = position.id : pool.positions.positionIds }
            }
      modify_ (\reg -> reg 
        { pools = Map.insert position.poolId updatedPool reg.pools 
        }) registryRef
    Nothing -> pure unit  -- Pool doesn't exist, skip

-- | Remove a position from the registry and its pool
removePosition :: PositionId -> PoolRegistry -> Effect Unit
removePosition posId registryRef = do
  registry <- read registryRef
  
  -- Find the position to get its pool ID
  case Map.lookup posId registry.positions of
    Just position -> do
      -- Remove from global position map
      modify_ (\reg -> reg 
        { positions = Map.delete posId reg.positions 
        }) registryRef
      
      -- Remove from pool's position book
      case Map.lookup position.poolId registry.pools of
        Just pool -> do
          let updatedPool = pool 
                { positions = pool.positions 
                  { positionIds = filter (_ /= posId) pool.positions.positionIds }
                }
          modify_ (\reg -> reg 
            { pools = Map.insert position.poolId updatedPool reg.pools 
            }) registryRef
        Nothing -> pure unit
    Nothing -> pure unit  -- Position doesn't exist

-- | Get a specific position (searches across all pools)
getPosition :: PositionId -> PoolRegistry -> Effect (Maybe Position)
getPosition posId registryRef = do
  registry <- read registryRef
  pure $ Map.lookup posId registry.positions

-- | Get all positions for a user across all pools
getUserPositions :: String -> PoolRegistry -> Effect (Array Position)
getUserPositions userId registryRef = do
  registry <- read registryRef
  pure $ filter (\p -> p.owner == userId) $ fromFoldable $ Map.values registry.positions

-- | Get all positions across all pools
getAllPositions :: PoolRegistry -> Effect (Array Position)
getAllPositions registryRef = do
  registry <- read registryRef
  pure $ fromFoldable $ Map.values registry.positions

-- | Get all positions in a specific pool
getPoolPositions :: PoolId -> PoolRegistry -> Effect (Array Position)
getPoolPositions poolId registryRef = do
  registry <- read registryRef
  case Map.lookup poolId registry.pools of
    Just pool -> do
      let positionIds = pool.positions.positionIds
      pure $ mapMaybe (\pid -> Map.lookup pid registry.positions) positionIds
    Nothing -> pure []
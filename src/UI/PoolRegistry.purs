-- | Pool registry management for the UI.
-- | Provides centralized management of pools and positions.
-- | In Solana, pools and positions would be Program Derived Addresses (PDAs).
module UI.PoolRegistry
  ( PoolRegistry
  , initPoolRegistry
  , addPool
  , updatePool
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
  , getPositionsRef
  , getPoolPositions
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:), filter, concat, mapMaybe, fromFoldable)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Protocol.Pool (Pool)
import Protocol.PositionVault (VaultPosition)
import Protocol.Common (PoolId, PositionId)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Global registry of all pools and positions
type PoolRegistry = Ref
  { pools :: Map PoolId Pool                 -- All pools by ID
  , positions :: Map PositionId VaultPosition    -- All positions by ID
  , poolPositions :: Map PoolId (Array PositionId)  -- Track which positions belong to each pool
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
  , poolPositions: Map.empty
  , nextPositionId: 1
  }

--------------------------------------------------------------------------------
-- Pool Management
--------------------------------------------------------------------------------

-- | Add a new pool to the registry
addPool :: PoolId -> Pool -> PoolRegistry -> Effect Unit
addPool poolId pool registryRef = do
  modify_ (\reg -> reg { pools = Map.insert poolId pool reg.pools }) registryRef

-- | Update an existing pool in the registry
updatePool :: PoolId -> Pool -> PoolRegistry -> Effect Unit
updatePool poolId pool registryRef = do
  modify_ (\reg -> reg { pools = Map.insert poolId pool reg.pools }) registryRef

-- | Remove a pool from the registry
removePool :: PoolId -> PoolRegistry -> Effect Unit
removePool poolId registryRef = do
  modify_ (\reg -> reg { pools = Map.delete poolId reg.pools }) registryRef

-- | Get a specific pool
getPool :: PoolId -> PoolRegistry -> Effect (Maybe Pool)
getPool poolId registryRef = do
  registry <- read registryRef
  pure $ Map.lookup poolId registry.pools

-- | Get all pools
getAllPools :: PoolRegistry -> Effect (Array (Tuple PoolId Pool))
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
addPosition :: PoolId -> VaultPosition -> PoolRegistry -> Effect Unit
addPosition poolId position registryRef = do
  -- Add to global position map and track pool relationship
  modify_ (\reg -> 
    let updatedPositions = Map.insert position.id position reg.positions
        currentPoolPositions = fromMaybe [] (Map.lookup poolId reg.poolPositions)
        updatedPoolPositions = Map.insert poolId (position.id : currentPoolPositions) reg.poolPositions
    in reg 
      { positions = updatedPositions
      , poolPositions = updatedPoolPositions
      }) registryRef
  
  -- Position successfully added to registry
  pure unit

-- | Remove a position from the registry and its pool
removePosition :: PositionId -> PoolRegistry -> Effect Unit
removePosition posId registryRef = do
  registry <- read registryRef
  
  -- Find the position to get its pool ID
  case Map.lookup posId registry.positions of
    Just position -> do
      -- Remove from global position map and pool-position tracking
      modify_ (\reg -> 
        let updatedPositions = Map.delete posId reg.positions
            -- Remove position ID from all pool mappings
            updatedPoolPositions = map (filter (_ /= posId)) reg.poolPositions
        in reg 
          { positions = updatedPositions
          , poolPositions = updatedPoolPositions
          }) registryRef
      
      -- Pool cleanup would happen here if needed
      -- (e.g., updating pool liquidity if this was an LP position)
      pure unit
    Nothing -> pure unit  -- Position doesn't exist

-- | Get a specific position (searches across all pools)
getPosition :: PositionId -> PoolRegistry -> Effect (Maybe VaultPosition)
getPosition posId registryRef = do
  registry <- read registryRef
  pure $ Map.lookup posId registry.positions

-- | Get all positions for a user across all pools
getUserPositions :: String -> PoolRegistry -> Effect (Array VaultPosition)
getUserPositions userId registryRef = do
  registry <- read registryRef
  pure $ filter (\p -> p.owner == userId) $ fromFoldable $ Map.values registry.positions

-- | Get all positions across all pools
getAllPositions :: PoolRegistry -> Effect (Array VaultPosition)
getAllPositions registryRef = do
  registry <- read registryRef
  pure $ fromFoldable $ Map.values registry.positions

-- | Get positions map as a separate Ref (for compatibility with Protocol modules)
getPositionsRef :: PoolRegistry -> Effect (Ref (Map PositionId VaultPosition))
getPositionsRef registryRef = do
  registry <- read registryRef
  new registry.positions

-- | Get all positions in a specific pool
getPoolPositions :: PoolId -> PoolRegistry -> Effect (Array VaultPosition)
getPoolPositions poolId registryRef = do
  registry <- read registryRef
  -- Get position IDs for this pool
  case Map.lookup poolId registry.poolPositions of
    Just positionIds -> do
      -- Look up each position by ID
      let positions = mapMaybe (\posId -> Map.lookup posId registry.positions) positionIds
      pure positions
    Nothing -> pure []  -- No positions in this pool
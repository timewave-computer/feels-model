-- | On-Chain Tick Operations for AMM Functionality
-- |
-- | This module provides the essential tick data structure and operations
-- | needed for Automated Market Maker (AMM) functionality. Ticks represent
-- | discrete price points where liquidity can be placed.
-- |
-- | Key concepts:
-- | - Ticks are price boundaries where liquidity changes
-- | - Each tick tracks net liquidity change and total liquidity
-- | - Fee growth tracking enables accurate fee distribution
module Protocol.Tick 
  ( Tick
  , initializeTick
  , createTick
  , updateTickLiquidity
  , crossTick
  , hasLiquidity
  ) where

import Prelude
import Data.Ord (abs)

--------------------------------------------------------------------------------
-- TICK TYPE DEFINITION
--------------------------------------------------------------------------------
-- Core tick data structure optimized for on-chain storage

-- | Minimal tick data for efficient on-chain storage
-- | This structure contains only essential information needed for AMM operations
type Tick =
  { liquidityNet :: Number              -- Net liquidity change when crossing this tick
  , liquidityGross :: Number            -- Total liquidity referencing this tick
  , feeGrowthOutside0 :: Number         -- Fee growth on the other side (token0)
  , feeGrowthOutside1 :: Number         -- Fee growth on the other side (token1)
  , initialized :: Boolean              -- Whether this tick has been initialized
  }

--------------------------------------------------------------------------------
-- TICK INITIALIZATION
--------------------------------------------------------------------------------
-- Functions for creating and initializing tick structures

-- | Initialize an empty, uninitialized tick
-- | Used when creating new tick positions in the AMM
initializeTick :: Tick
initializeTick =
  { liquidityNet: 0.0
  , liquidityGross: 0.0
  , feeGrowthOutside0: 0.0
  , feeGrowthOutside1: 0.0
  , initialized: false
  }

-- | Create a tick with initial liquidity (compatibility function)
-- | Used for setting up initial pool state with provided liquidity
createTick :: Number -> Number -> Tick
createTick _price liquidity = 
  { liquidityNet: 0.0                   -- No net change initially
  , liquidityGross: liquidity           -- Set total liquidity
  , feeGrowthOutside0: 0.0              -- No fee growth initially
  , feeGrowthOutside1: 0.0              -- No fee growth initially
  , initialized: true                   -- Mark as initialized
  }

--------------------------------------------------------------------------------
-- LIQUIDITY OPERATIONS
--------------------------------------------------------------------------------
-- Functions for modifying tick liquidity during position changes

-- | Update tick when adding or removing liquidity
-- | isUpper: true if this is the upper tick of a position, false for lower
updateTickLiquidity :: Number -> Boolean -> Tick -> Tick
updateTickLiquidity liquidityDelta isUpper tick =
  let 
    -- Update net liquidity based on position boundary
    -- Upper ticks subtract liquidity, lower ticks add liquidity
    liquidityNetAfter = if isUpper
      then tick.liquidityNet - liquidityDelta
      else tick.liquidityNet + liquidityDelta
      
    -- Total liquidity always increases by absolute amount
    liquidityGrossAfter = tick.liquidityGross + abs liquidityDelta
    
  in tick
    { liquidityNet = liquidityNetAfter
    , liquidityGross = liquidityGrossAfter
    , initialized = true                -- Mark as initialized after update
    }

--------------------------------------------------------------------------------
-- SWAP EXECUTION
--------------------------------------------------------------------------------
-- Functions for tick operations during swap execution

-- | Update tick fee growth tracking when crossing during a swap
-- | This ensures accurate fee distribution to liquidity providers
crossTick :: Number -> Number -> Tick -> Tick
crossTick feeGrowthGlobal0 feeGrowthGlobal1 tick =
  tick
    { feeGrowthOutside0 = feeGrowthGlobal0 - tick.feeGrowthOutside0
    , feeGrowthOutside1 = feeGrowthGlobal1 - tick.feeGrowthOutside1
    }

--------------------------------------------------------------------------------
-- TICK QUERIES
--------------------------------------------------------------------------------
-- Utility functions for inspecting tick state

-- | Check if a tick has any liquidity
-- | Used to determine if tick operations are needed
hasLiquidity :: Tick -> Boolean
hasLiquidity tick = tick.liquidityGross > 0.0
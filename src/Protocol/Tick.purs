-- | On-Chain Tick Operations for AMM Functionality
-- |
-- | This module provides the essential tick data structure and operations
-- | needed for Automated Market Maker (AMM) functionality. Ticks represent
-- | discrete price points where liquidity can be placed.
-- |
-- | === How Ticks Work ===
-- |
-- | Ticks divide the price space into discrete intervals. Each tick represents
-- | a specific price point where liquidity can change:
-- |
-- |     Price
-- |      ↑
-- |      |    ... -3  -2  -1   0   1   2   3 ...
-- |      |     |   |   |   |   |   |   |   |
-- |  1.0003 ---|---|---|---|---|---|---|---|--- tick 3
-- |  1.0002 ---|---|---|---|---|---|---|---|--- tick 2  
-- |  1.0001 ---|---|---|---|---|---|---|---|--- tick 1
-- |  1.0000 ---|---|---|---|---●---|---|---|--- tick 0 (current price)
-- |  0.9999 ---|---|---|---|---|---|---|---|--- tick -1
-- |  0.9998 ---|---|---|---|---|---|---|---|--- tick -2
-- |  0.9997 ---|---|---|---|---|---|---|---|--- tick -3
-- |      |     |   |   |   |   |   |   |   |
-- |      └─────────────────────────────────────→ Tick Index
-- |
-- | Liquidity providers place liquidity between two ticks (a range).
-- | As the price moves and crosses ticks, the active liquidity changes.
-- |
-- | === Definitions ===
-- |
-- | 1. **Liquidity** - The amount of capital available for trading at a price range.
-- |    In AMM terms: L = sqrt(x * y), where x and y are token reserves.
-- |
-- | 2. **liquidityNet** - The change in active liquidity when crossing this tick.
-- |    Positive = entering position ranges, Negative = exiting position ranges.
-- |
-- | 3. **liquidityGross** - Total liquidity that references this tick (either as
-- |    start or end of a position). Used to know when a tick can be deleted.
-- |
-- | 4. **feeGrowthOutside** - Cumulative fees earned on the "other side" of this
-- |    tick relative to current price. Enables O(1) fee calculations.
module Protocol.Tick 
  ( Tick
  , emptyTick
  , initializeTick
  , updateTickLiquidity
  , crossTick
  , hasLiquidity
  ) where

import Prelude
import Data.Ord (abs)

--------------------------------------------------------------------------------
-- TICK TYPE
--------------------------------------------------------------------------------
-- Core tick data structure optimized for on-chain storage

-- | Minimal tick data for efficient on-chain storage
-- | This structure contains the essential information needed for AMM operations
-- |
-- | === Understanding Liquidity Net vs Gross ===
-- |
-- | Consider positions A and B with their tick ranges:
-- |
-- |      tick -200    tick -100     tick 0      tick 100     tick 200
-- |         |            |            |            |            |
-- |         |<--- Pos A: 1000 liq --->|            |            |
-- |         |            |<---- Pos B: 500 liq --->|            |
-- |         |            |            |            |            |
-- |
-- | At each tick:
-- | - tick -200: liquidityNet = +1000 (A starts), liquidityGross = 1000
-- | - tick -100: liquidityNet = +500 (B starts), liquidityGross = 500  
-- | - tick 0:    liquidityNet = -1000 (A ends), liquidityGross = 1000
-- | - tick 100:  liquidityNet = -500 (B ends), liquidityGross = 500
-- |
-- | When price crosses a tick moving right:
-- | - Add liquidityNet to active liquidity (can be positive or negative)
-- | - If liquidityNet > 0: entering new position ranges
-- | - If liquidityNet < 0: exiting position ranges
-- |
-- | liquidityGross tells us total liquidity referencing this tick (for cleanup)
type Tick =
  { liquidityNet :: Number              -- Net liquidity change when crossing this tick
  , liquidityGross :: Number            -- Total liquidity referencing this tick  
  , feeGrowthOutside0 :: Number         -- Fee growth on the "other side" (token0)
  , feeGrowthOutside1 :: Number         -- Fee growth on the "other side" (token1)
  , initialized :: Boolean              -- Whether this tick has been initialized
  }

-- | === Understanding feeGrowthOutside ===
-- |
-- | feeGrowthOutside tracks cumulative fees on the "other side" of a tick.
-- | This clever mechanism allows efficient fee calculation for any position.
-- |
-- | Example with current price at tick 50:
-- |
-- |     tick -100        tick 0         tick 50        tick 100       tick 200
-- |        |              |              |●|              |              |
-- |        |              |              |↑|              |              |
-- |        |              |           current             |              |
-- |        |              |            price              |              |
-- |        |              |              |                |              |
-- |  fees: 10            20             30               40             50
-- |
-- | For each tick, feeGrowthOutside represents:
-- | - If tick < current price: fees accumulated to the LEFT of the tick
-- | - If tick >= current price: fees accumulated to the RIGHT of the tick
-- |
-- | To calculate fees earned by position [tickLower, tickUpper]:
-- | 1. Get feeGrowthInside = totalFees - feeGrowthBelow - feeGrowthAbove
-- | 2. Where:
-- |    - feeGrowthBelow = feeGrowthOutside of tickLower
-- |    - feeGrowthAbove = feeGrowthOutside of tickUpper
-- |
-- | This works because fees "outside" the position range are exactly
-- | what we need to subtract from total fees to get fees "inside".

--------------------------------------------------------------------------------
-- TICK CREATION
--------------------------------------------------------------------------------
-- Functions for creating empty ticks and initializing ticks with liquidity

-- | Create an empty tick with default values
-- | All liquidity values are zero and the tick is marked as uninitialized
emptyTick :: Tick
emptyTick =
  { liquidityNet: 0.0
  , liquidityGross: 0.0
  , feeGrowthOutside0: 0.0
  , feeGrowthOutside1: 0.0
  , initialized: false
  }

-- | Initialize a tick with the given liquidity
-- | Creates an initialized tick with proper liquidityNet based on tick position
-- |
-- | Parameters:
-- |   liquidity - The initial liquidity amount for this tick
-- |   isUpper   - True if this is an upper tick boundary (liquidityNet will be negative)
initializeTick :: Number -> Boolean -> Tick
initializeTick liquidity isUpper =
  { liquidityNet: if isUpper then -liquidity else liquidity
  , liquidityGross: liquidity
  , feeGrowthOutside0: 0.0
  , feeGrowthOutside1: 0.0
  , initialized: true
  }


--------------------------------------------------------------------------------
-- LIQUIDITY OPERATIONS
--------------------------------------------------------------------------------
-- Functions for modifying tick liquidity during position changes

-- | Update tick liquidity when a position is added or removed
-- | Modifies both liquidityNet and liquidityGross appropriately
-- |
-- | Parameters:
-- |   liquidityDelta - Amount of liquidity to add (positive) or remove (negative)
-- |   isUpper        - True if this is the upper tick of a position
-- |   tick           - The tick to update
-- |
-- | Example: Adding 1000 liquidity to position [tick 10, tick 30]
-- |
-- |     Before:                          After:
-- |     tick 10      tick 30            tick 10      tick 30
-- |        |           |                   |           |
-- |        |           |                   |+1000      |-1000    (net)
-- |        |           |                   |1000       |1000     (gross)
-- |        |           |                   |           |
-- |
-- | When price moves left-to-right and crosses tick 10:
-- | - We ADD +1000 to active liquidity (entering the position)
-- | When price crosses tick 30:
-- | - We ADD -1000 to active liquidity (exiting the position)
updateTickLiquidity :: Number -> Boolean -> Tick -> Tick
updateTickLiquidity liquidityDelta isUpper tick =
  let 
    -- Update net liquidity based on position boundary
    -- Lower tick: +liquidity (enter position when crossing left-to-right)
    -- Upper tick: -liquidity (exit position when crossing left-to-right)
    liquidityNetAfter = if isUpper
      then tick.liquidityNet - liquidityDelta
      else tick.liquidityNet + liquidityDelta
      
    -- Total liquidity always increases by absolute amount
    -- This tracks how many positions reference this tick
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

-- | Cross a tick during swap execution  
-- | Updates fee growth tracking by "flipping" the inside/outside perspective
-- |
-- | Parameters:
-- |   feeGrowthGlobal0 - Current global fee growth for token0
-- |   feeGrowthGlobal1 - Current global fee growth for token1
-- |   tick             - The tick being crossed
-- |
-- | When we cross a tick, we need to "flip" what's considered inside vs outside.
-- |
-- | Example: Price moving from tick 40 to tick 60, crossing tick 50:
-- |
-- |     Before crossing:                 After crossing:
-- |     tick 40    tick 50    tick 60    tick 40    tick 50    tick 60
-- |        |         |●→        |           |       ←●|         |
-- |    outside     outside    inside     inside    inside    outside
-- |     (20)        (25)                            (25)      
-- |
-- | Global fees = 45                     Global fees = 45
-- | feeGrowthOutside = 25               feeGrowthOutside = 45 - 25 = 20
-- |
-- | The formula (global - outside) flips the perspective when we cross the tick.
-- | This maintains consistency: fees that were "outside" are now "inside" and vice versa.
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
-- | Minimal on-chain tick representation
-- | Only essential data needed for AMM operations
module Protocol.Tick 
  ( OnChainTick
  , initializeTick
  , createTick
  , updateTickLiquidity
  , crossTick
  , hasLiquidity
  ) where

import Prelude
import Data.Ord (abs)

--------------------------------------------------------------------------------
-- On-Chain Tick Types (Minimal)
--------------------------------------------------------------------------------

-- | Minimal tick data for on-chain storage
type OnChainTick =
  { liquidityNet :: Number      -- Net liquidity change when crossing this tick
  , liquidityGross :: Number    -- Total liquidity referencing this tick
  , feeGrowthOutside0 :: Number -- Fee growth on the other side (token0)
  , feeGrowthOutside1 :: Number -- Fee growth on the other side (token1)
  , initialized :: Boolean      -- Whether this tick has been initialized
  }

-- | Initialize an empty tick
initializeTick :: OnChainTick
initializeTick =
  { liquidityNet: 0.0
  , liquidityGross: 0.0
  , feeGrowthOutside0: 0.0
  , feeGrowthOutside1: 0.0
  , initialized: false
  }

-- | Create a tick with price and liquidity (for compatibility)
createTick :: Number -> Number -> OnChainTick
createTick _price liquidity = 
  { liquidityNet: 0.0
  , liquidityGross: liquidity
  , feeGrowthOutside0: 0.0
  , feeGrowthOutside1: 0.0
  , initialized: true
  }

-- | Update tick when adding/removing liquidity
updateTickLiquidity :: Number -> Boolean -> OnChainTick -> OnChainTick
updateTickLiquidity liquidityDelta isUpper tick =
  let 
    liquidityNetAfter = if isUpper
      then tick.liquidityNet - liquidityDelta
      else tick.liquidityNet + liquidityDelta
      
    liquidityGrossAfter = tick.liquidityGross + abs liquidityDelta
    
  in tick
    { liquidityNet = liquidityNetAfter
    , liquidityGross = liquidityGrossAfter
    , initialized = true
    }

-- | Cross a tick during swap execution
crossTick :: Number -> Number -> OnChainTick -> OnChainTick
crossTick feeGrowthGlobal0 feeGrowthGlobal1 tick =
  tick
    { feeGrowthOutside0 = feeGrowthGlobal0 - tick.feeGrowthOutside0
    , feeGrowthOutside1 = feeGrowthGlobal1 - tick.feeGrowthOutside1
    }

-- | Check if tick has any liquidity
hasLiquidity :: OnChainTick -> Boolean
hasLiquidity tick = tick.liquidityGross > 0.0
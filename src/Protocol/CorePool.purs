-- | Minimal on-chain pool implementation optimized for Solana
-- | Only essential state for swaps and critical POL decisions
module Protocol.CorePool where

import Prelude

--------------------------------------------------------------------------------
-- Core Types (Minimal State)
--------------------------------------------------------------------------------

-- | Minimal tick data - just what's needed for swaps
type OnChainTick =
  { liquidityNet :: Int        -- Net liquidity change when crossing
  , liquidityGross :: Number    -- Total liquidity referencing this tick
  , feeGrowthOutside :: Number  -- Fee growth on other side
  }

-- | Minimal pool state for on-chain
type OnChainPool =
  { -- Essential swap state
    sqrtPrice :: Number
  , currentTick :: Int
  , liquidity :: Number
  
    -- Minimal metrics for instant POL decisions
  , volume24h :: Number         -- Single rolling value
  , feeGrowthGlobal :: Number
  , twap :: Number              -- Time-weighted average price
  , lastUpdateSlot :: Int
  
    -- POL state
  , polAmount :: Number
  , polTickLower :: Int
  , polTickUpper :: Int
  }

-- | POL trigger conditions (can evaluate in single instruction)
type POLTrigger =
  { volumeThreshold :: Number      -- Minimum 24h volume
  , priceDeviation :: Number       -- Max deviation from TWAP (basis points)
  , liquidityDepth :: Number       -- Minimum liquidity requirement
  , activityDecay :: Int           -- Blocks since last trade
  }

-- | Events emitted for off-chain indexing
data PoolEvent
  = SwapExecuted
    { pool :: String
    , amountIn :: Number
    , amountOut :: Number
    , sqrtPrice :: Number
    , tick :: Int
    , fee :: Number
    , slot :: Int
    }
  | LiquidityChanged
    { pool :: String
    , tickLower :: Int
    , tickUpper :: Int
    , liquidityDelta :: Number
    , slot :: Int
    }
  | POLDeployed
    { pool :: String
    , amount :: Number
    , tickLower :: Int
    , tickUpper :: Int
    , triggerType :: POLTriggerType
    , slot :: Int
    }

data POLTriggerType
  = VolumeTrigger
  | PriceDeviationTrigger
  | LiquidityDepthTrigger
  | EmergencySupport

--------------------------------------------------------------------------------
-- Efficient On-Chain Operations
--------------------------------------------------------------------------------

-- | Check if POL should be deployed (simple, gas-efficient)
checkPOLTrigger :: OnChainPool -> POLTrigger -> Int -> Boolean
checkPOLTrigger pool trigger currentSlot =
  let
    -- Simple boolean checks that can run efficiently
    volumeCheck = pool.volume24h > trigger.volumeThreshold
    
    priceCheck = 
      let deviation = abs (pool.sqrtPrice - pool.twap) / pool.twap * 10000.0
      in deviation > trigger.priceDeviation
      
    liquidityCheck = pool.liquidity > trigger.liquidityDepth
    
    activityCheck = (currentSlot - pool.lastUpdateSlot) < trigger.activityDecay
    
  in volumeCheck && priceCheck && liquidityCheck && activityCheck

-- | Execute swap with minimal state updates
executeSwap :: OnChainPool -> Number -> Int -> { pool :: OnChainPool, event :: PoolEvent }
executeSwap pool amountIn currentSlot =
  let
    -- Simplified swap math (real implementation would use sqrt price math)
    amountOut = amountIn * pool.liquidity / (pool.liquidity + amountIn)
    newSqrtPrice = pool.sqrtPrice * (1.0 + amountIn / pool.liquidity)
    newTick = sqrtPriceToTick newSqrtPrice
    fee = amountIn * 0.003  -- 0.3% fee
    
    -- Update minimal state
    updatedPool = pool
      { sqrtPrice = newSqrtPrice
      , currentTick = newTick
      , volume24h = pool.volume24h + amountIn  -- Simple accumulator
      , feeGrowthGlobal = pool.feeGrowthGlobal + fee / pool.liquidity
      , lastUpdateSlot = currentSlot
      }
      
    event = SwapExecuted
      { pool: "pool_id"
      , amountIn: amountIn
      , amountOut: amountOut
      , sqrtPrice: newSqrtPrice
      , tick: newTick
      , fee: fee
      , slot: currentSlot
      }
      
  in { pool: updatedPool, event: event }

-- | Deploy POL with single state update
deployPOL :: OnChainPool -> Number -> Int -> Int -> POLTriggerType -> OnChainPool
deployPOL pool amount tickLower tickUpper triggerType =
  pool
    { polAmount = pool.polAmount + amount
    , polTickLower = tickLower
    , polTickUpper = tickUpper
    }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

sqrtPriceToTick :: Number -> Int
sqrtPriceToTick sqrtPrice = 
  -- Simplified - real implementation uses log base 1.0001
  floor (log sqrtPrice * 10000.0)

-- | Update TWAP (time-weighted average price)
updateTWAP :: OnChainPool -> Int -> Number -> OnChainPool
updateTWAP pool currentSlot newPrice =
  let
    timeDelta = currentSlot - pool.lastUpdateSlot
    alpha = min 1.0 (toNumber timeDelta / 1000.0)  -- Decay factor
    newTwap = pool.twap * (1.0 - alpha) + newPrice * alpha
  in pool { twap = newTwap }

-- | Check if tick is active (has liquidity)
isActiveTick :: Int -> OnChainPool -> Boolean
isActiveTick tick pool =
  tick >= pool.polTickLower && tick < pool.polTickUpper && pool.polAmount > 0.0
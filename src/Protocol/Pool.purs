-- | Minimal on-chain pool implementation optimized for Solana
-- | Only essential state for swaps and critical POL decisions
-- | Core AMM mechanics with efficient state management
module Protocol.Pool
  ( -- Core Types
    PoolState
  , PoolConfig
  , SwapParams
  , SwapResult
  , LiquidityParams
  , LiquidityResult
  -- On-chain minimal types (from CorePool)
  , OnChainTick
  , OnChainPool
  , POLTrigger
  , PoolEvent(..)
  , POLTriggerType(..)
  -- Functions
  , initializePool
  , swap
  , addLiquidity
  , removeLiquidity
  , getPoolPrice
  , getPoolLiquidity
  -- On-chain operations (from CorePool)
  , checkPOLTrigger
  , executeSwap
  , deployPOL
  , updateTWAP
  , isActiveTick
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber, floor)
import Data.Ord (abs)
import Math (sqrt, pow, log)
import Protocol.Token (TokenType)
import Protocol.Position (TermCommitment)
import Protocol.Common (PositionId, BlockNumber)

--------------------------------------------------------------------------------
-- On-Chain Pool State (Minimal)
--------------------------------------------------------------------------------

-- | Essential pool state for on-chain storage
type PoolState =
  { token0 :: TokenType              -- Base token (usually FeelsSOL)
  , token1 :: TokenType              -- Quote token
  , sqrtPriceX96 :: Number          -- Current sqrt price (Q64.96 format)
  , liquidity :: Number             -- Active liquidity
  , tick :: Int                     -- Current tick
  , feeGrowthGlobal0X128 :: Number  -- Global fee growth token0
  , feeGrowthGlobal1X128 :: Number  -- Global fee growth token1
  , protocolFee :: Number           -- Protocol fee share (basis points)
  , unlocked :: Boolean             -- Reentrancy guard
  , offering :: Maybe OfferingId    -- Active offering (if any)
  }

-- | Offering identifier
type OfferingId = String

-- | Pool configuration parameters
type PoolConfig =
  { tickSpacing :: Int              -- Minimum tick spacing
  , fee :: Number                   -- Swap fee tier (basis points)
  , maxLiquidityPerTick :: Number   -- Liquidity cap per tick
  }

--------------------------------------------------------------------------------
-- Core Types (Minimal State) - From CorePool
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
-- Swap Operations
--------------------------------------------------------------------------------

-- | Parameters for swap execution
type SwapParams =
  { zeroForOne :: Boolean           -- Direction of swap
  , amountSpecified :: Number       -- Amount to swap (positive=exact in, negative=exact out)
  , sqrtPriceLimitX96 :: Number     -- Price limit for swap
  }

-- | Result of swap execution
type SwapResult =
  { amount0 :: Number               -- Token0 delta
  , amount1 :: Number               -- Token1 delta
  , sqrtPriceX96 :: Number          -- New pool price
  , liquidity :: Number             -- New active liquidity
  , tick :: Int                     -- New current tick
  , gasUsed :: Number               -- Estimated gas usage
  }

-- | Execute a swap
swap :: PoolState -> SwapParams -> SwapResult
swap pool params =
  let
    -- Simplified swap logic - real implementation would:
    -- 1. Calculate swap amounts using constant product formula
    -- 2. Update ticks as price moves
    -- 3. Collect fees
    -- 4. Check price limits
    
    priceImpact = params.amountSpecified / pool.liquidity * 0.01
    newPrice = if params.zeroForOne
      then pool.sqrtPriceX96 * (1.0 - priceImpact)
      else pool.sqrtPriceX96 * (1.0 + priceImpact)
      
    newTick = sqrtPriceToTick newPrice
    
    -- Simple fee calculation
    feeAmount = abs params.amountSpecified * pool.protocolFee / 10000.0
    
  in { amount0: if params.zeroForOne then params.amountSpecified else -params.amountSpecified
     , amount1: if params.zeroForOne then -params.amountSpecified * 0.99 else params.amountSpecified * 0.99
     , sqrtPriceX96: newPrice
     , liquidity: pool.liquidity
     , tick: newTick
     , gasUsed: 150000.0  -- Estimated
     }

--------------------------------------------------------------------------------
-- Liquidity Operations
--------------------------------------------------------------------------------

-- | Parameters for adding liquidity
type LiquidityParams =
  { tickLower :: Int                -- Lower tick boundary
  , tickUpper :: Int                -- Upper tick boundary
  , amount :: Number                -- Liquidity amount to add
  , recipient :: String             -- Position recipient
  }

-- | Result of liquidity operation
type LiquidityResult =
  { positionId :: PositionId        -- Created/updated position
  , liquidity :: Number             -- Total position liquidity
  , amount0 :: Number               -- Token0 deposited
  , amount1 :: Number               -- Token1 deposited
  }

-- | Add liquidity to pool
addLiquidity :: PoolState -> LiquidityParams -> LiquidityResult
addLiquidity pool params =
  let
    -- Calculate token amounts needed for liquidity
    amount0 = calculateAmount0ForLiquidity pool.sqrtPriceX96 params.tickLower params.tickUpper params.amount
    amount1 = calculateAmount1ForLiquidity pool.sqrtPriceX96 params.tickLower params.tickUpper params.amount
    
    -- Position ID would be generated on-chain
    positionId = "POS-" <> show params.tickLower <> "-" <> show params.tickUpper
    
  in { positionId: positionId
     , liquidity: params.amount
     , amount0: amount0
     , amount1: amount1
     }

-- | Remove liquidity from pool
removeLiquidity :: PoolState -> PositionId -> Number -> LiquidityResult
removeLiquidity pool positionId amount =
  { positionId: positionId
  , liquidity: -amount
  , amount0: amount * 0.5  -- Simplified
  , amount1: amount * 0.5  -- Simplified
  }

--------------------------------------------------------------------------------
-- Pool Initialization
--------------------------------------------------------------------------------

-- | Initialize a new pool
initializePool :: TokenType -> TokenType -> Number -> PoolConfig -> PoolState
initializePool token0 token1 initialPrice config =
  { token0: token0
  , token1: token1
  , sqrtPriceX96: priceToSqrtPriceX96 initialPrice
  , liquidity: 0.0
  , tick: priceToTick initialPrice
  , feeGrowthGlobal0X128: 0.0
  , feeGrowthGlobal1X128: 0.0
  , protocolFee: config.fee
  , unlocked: true
  , offering: Nothing
  }

--------------------------------------------------------------------------------
-- View Functions
--------------------------------------------------------------------------------

-- | Get current pool price
getPoolPrice :: PoolState -> Number
getPoolPrice pool = sqrtPriceX96ToPrice pool.sqrtPriceX96

-- | Get current pool liquidity
getPoolLiquidity :: PoolState -> Number
getPoolLiquidity pool = pool.liquidity

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

priceToSqrtPriceX96 :: Number -> Number
priceToSqrtPriceX96 price = sqrt price * pow 2.0 96.0

sqrtPriceX96ToPrice :: Number -> Number
sqrtPriceX96ToPrice sqrtPriceX96 = pow (sqrtPriceX96 / pow 2.0 96.0) 2.0

priceToTick :: Number -> Int
priceToTick price = floor (log price / log 1.0001)

sqrtPriceToTick :: Number -> Int
sqrtPriceToTick sqrtPrice = priceToTick (sqrtPrice * sqrtPrice)

calculateAmount0ForLiquidity :: Number -> Int -> Int -> Number -> Number
calculateAmount0ForLiquidity sqrtPrice tickLower tickUpper liquidity =
  -- Simplified calculation
  liquidity * abs (1.0 / sqrt (pow 1.0001 (toNumber tickLower)) - 1.0 / sqrtPrice)

calculateAmount1ForLiquidity :: Number -> Int -> Int -> Number -> Number  
calculateAmount1ForLiquidity sqrtPrice tickLower tickUpper liquidity =
  -- Simplified calculation
  liquidity * abs (sqrtPrice - sqrt (pow 1.0001 (toNumber tickLower)))

-- Math helpers now imported from standard libraries
-- sqrt, pow, log from Math
-- floor, toNumber from Data.Int  
-- abs from Data.Ord
unsafeCoerce = unsafeCoerce

--------------------------------------------------------------------------------
-- Efficient On-Chain Operations (from CorePool)
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
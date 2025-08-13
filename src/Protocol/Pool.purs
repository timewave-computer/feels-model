-- | Core AMM mechanics with efficient state management
-- |
-- | This module implements concentrated liquidity AMM mechanics using a tick-based system.
-- | It defines two pool representations:
-- |
-- | 1. PoolState - Complete AMM pool state for full-featured operations (swaps, liquidity)
-- | 2. POLPool - Minimal pool state optimized for Protocol-Owned Liquidity decisions
-- |
-- | Ticks provide discrete price levels for liquidity placement, while the underlying
-- | AMM math (x*y=L²) is used for continuous price discovery within ranges
-- | and accurate swap execution across tick boundaries.
module Protocol.Pool
  ( PoolState
  , OfferingId
  , PoolConfig
  , SwapParams
  , SwapResult
  , LiquidityParams
  , LiquidityResult
  , POLPool
  , PoolEvent(..)
  , TrancheState
  , TrancheValues
  , initializePool
  , swap
  , addLiquidity
  , removeLiquidity
  , getPoolPrice
  , getPoolLiquidity
  , executeSwap
  , updateTWAP
  , isTickInPOLRange
  -- Tranche value functions
  , calculateTrancheValues
  , updateTrancheValues
  , distributePoolPnL
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber, floor)
import Data.Ord (abs, min, max)
import FFI (sqrt, log, pow)
import Protocol.Token (TokenType)
import Protocol.Tick (Tick)
import Protocol.Position (TermCommitment, Tranche(..), Position)
import Protocol.Common (PositionId, BlockNumber)
import Protocol.POL (POLTriggerType(..))

--------------------------------------------------------------------------------
-- On-Chain Pool State
--------------------------------------------------------------------------------

-- | Essential pool state for on-chain storage
type PoolState =
  { token0 :: TokenType             -- Base token (usually FeelsSOL)
  , token1 :: TokenType             -- Quote token
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
-- POL-Optimized Types
--------------------------------------------------------------------------------

-- | Minimal pool state optimized for Protocol-Owned Liquidity (POL) operations
-- | This stripped-down representation contains only the essential data needed
-- | for POL decision-making and efficient on-chain storage/computation.
-- | Unlike PoolState which tracks full AMM state, POLPool focuses on metrics
-- | critical for automated liquidity management.
type POLPool =
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

--------------------------------------------------------------------------------
-- Tranche Value Management
--------------------------------------------------------------------------------

-- | State tracking for each tranche's aggregate value
type TrancheState =
  { seniorValue :: Number      -- Total value in senior tranche
  , juniorValue :: Number      -- Total value in junior tranche
  , seniorShares :: Number     -- Total senior shares outstanding
  , juniorShares :: Number     -- Total junior shares outstanding
  }

-- | Individual tranche value calculation results
type TrancheValues =
  { senior :: Number           -- Value allocated to senior tranche
  , junior :: Number           -- Value allocated to junior tranche
  }

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
    -- 1. Extract current reserves from sqrt price and liquidity
    -- Using x * y = L^2 and sqrt(y/x) = sqrtPrice
    currentSqrtPrice = pool.sqrtPriceX96 / 2.0 `pow` 48.0  -- Convert from Q64.96 to regular
    reserve0 = pool.liquidity / currentSqrtPrice
    reserve1 = pool.liquidity * currentSqrtPrice
    
    -- 2. Apply fee to input amount (0.3% = 30 basis points)
    feeRate = pool.protocolFee / 10000.0  -- Convert basis points to decimal
    amountInAfterFee = abs params.amountSpecified * (1.0 - feeRate)
    feeAmount = abs params.amountSpecified * feeRate
    
    -- 3. Calculate output using constant product formula: x * y = k
    -- For zeroForOne (token0 -> token1): dy = y * dx / (x + dx)
    -- For oneForZero (token1 -> token0): dx = x * dy / (y + dy)
    swapResult = 
      if params.zeroForOne
        then 
          let out = reserve1 * amountInAfterFee / (reserve0 + amountInAfterFee)
          in { amountOut: out, newReserve0: reserve0 + amountInAfterFee, newReserve1: reserve1 - out }
        else 
          let out = reserve0 * amountInAfterFee / (reserve1 + amountInAfterFee)
          in { amountOut: out, newReserve0: reserve0 - out, newReserve1: reserve1 + amountInAfterFee }
    
    amountOut = swapResult.amountOut
    newReserve0 = swapResult.newReserve0
    newReserve1 = swapResult.newReserve1
    
    -- 4. Calculate new sqrt price from new reserves
    newSqrtPrice = sqrt (newReserve1 / newReserve0)
    newSqrtPriceX96 = newSqrtPrice * 2.0 `pow` 48.0  -- Convert back to Q64.96
    
    -- 5. Update tick based on new price
    newTick = sqrtPriceToTick newSqrtPriceX96
    
    -- 6. Check price limits
    priceInBounds = if params.sqrtPriceLimitX96 == 0.0
                    then true  -- No price limit
                    else if params.zeroForOne 
                         then newSqrtPriceX96 >= params.sqrtPriceLimitX96  -- Price decreases for zeroForOne
                         else newSqrtPriceX96 <= params.sqrtPriceLimitX96  -- Price increases for oneForZero
    
    -- 7. Calculate actual amounts (negative for outflows)
    finalAmount0 = if params.zeroForOne 
                   then params.amountSpecified  -- User provides token0
                   else -amountOut              -- User receives token0
    finalAmount1 = if params.zeroForOne 
                   then -amountOut              -- User receives token1
                   else params.amountSpecified  -- User provides token1
    
  in if priceInBounds
     then { amount0: finalAmount0
          , amount1: finalAmount1
          , sqrtPriceX96: newSqrtPriceX96
          , liquidity: pool.liquidity
          , tick: newTick
          , gasUsed: 150000.0
          }
     else { amount0: 0.0
          , amount1: 0.0
          , sqrtPriceX96: pool.sqrtPriceX96  -- No change if price limit hit
          , liquidity: pool.liquidity
          , tick: pool.tick
          , gasUsed: 50000.0  -- Less gas if swap fails
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
    -- For now, create a simple hash from tick range
    positionId = abs (params.tickLower * 1000 + params.tickUpper)
    
  in { positionId: positionId
     , liquidity: params.amount
     , amount0: amount0
     , amount1: amount1
     }

-- | Remove liquidity from pool
removeLiquidity :: PoolState -> PositionId -> Number -> LiquidityResult
removeLiquidity pool positionId amount =
  let
    -- For removing liquidity, we need to calculate how much of each token
    -- the LP receives back based on their liquidity share
    -- This requires knowing the tick range of the position
    -- For now, assume a position around the current price with ±10 ticks
    tickLower = pool.tick - 10
    tickUpper = pool.tick + 10
    
    -- Calculate token amounts using the same formulas as adding liquidity
    -- When removing liquidity, you get back tokens proportional to your share
    amount0 = calculateAmount0ForLiquidity pool.sqrtPriceX96 tickLower tickUpper amount
    amount1 = calculateAmount1ForLiquidity pool.sqrtPriceX96 tickLower tickUpper amount
    
  in { positionId: positionId
     , liquidity: -amount  -- Negative to indicate removal
     , amount0: amount0    -- Token0 returned to LP
     , amount1: amount1    -- Token1 returned to LP
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

-- | Convert liquidity to token0 amount for concentrated liquidity positions
-- | Essential for LP deposit/withdrawal calculations in tick-based system
calculateAmount0ForLiquidity :: Number -> Int -> Int -> Number -> Number
calculateAmount0ForLiquidity sqrtPrice tickLower tickUpper liquidity =
  -- Liquidity is abstract; we need these formulas to convert to actual tokens
  -- When current price is within range: amount0 = L * (1/sqrt(P) - 1/sqrt(Pb))
  -- When current price is below range: amount0 = L * (1/sqrt(Pa) - 1/sqrt(Pb))
  -- When current price is above range: amount0 = 0
  let
    sqrtPriceLower = sqrt (pow 1.0001 (toNumber tickLower))
    sqrtPriceUpper = sqrt (pow 1.0001 (toNumber tickUpper))
    
    -- Determine which formula to use based on current price position
    priceRange = 
      if sqrtPrice < sqrtPriceLower
        then { sqrtPriceA: sqrtPriceLower, sqrtPriceB: sqrtPriceUpper }  -- Price below range
        else if sqrtPrice > sqrtPriceUpper
          then { sqrtPriceA: sqrtPriceUpper, sqrtPriceB: sqrtPriceUpper }  -- Price above range (no token0)
          else { sqrtPriceA: sqrtPrice, sqrtPriceB: sqrtPriceUpper }        -- Price within range
    
    sqrtPriceA = priceRange.sqrtPriceA
    sqrtPriceB = priceRange.sqrtPriceB
    
    -- Calculate amount0 = L * (1/sqrtPriceA - 1/sqrtPriceB)
    amount = if sqrtPriceA == sqrtPriceB
             then 0.0  -- No token0 needed when price above range
             else liquidity * abs (1.0 / sqrtPriceA - 1.0 / sqrtPriceB)
  in amount

-- | Convert liquidity to token1 amount for concentrated liquidity positions
calculateAmount1ForLiquidity :: Number -> Int -> Int -> Number -> Number  
calculateAmount1ForLiquidity sqrtPrice tickLower tickUpper liquidity =
  -- These formulas enable capital efficiency by only using liquidity in range
  -- When current price is within range: amount1 = L * (sqrt(P) - sqrt(Pa))
  -- When current price is below range: amount1 = 0
  -- When current price is above range: amount1 = L * (sqrt(Pb) - sqrt(Pa))
  let
    sqrtPriceLower = sqrt (pow 1.0001 (toNumber tickLower))
    sqrtPriceUpper = sqrt (pow 1.0001 (toNumber tickUpper))
    
    -- Determine which formula to use based on current price position
    priceRange = 
      if sqrtPrice < sqrtPriceLower
        then { sqrtPriceA: sqrtPriceLower, sqrtPriceB: sqrtPriceLower }  -- Price below range (no token1)
        else if sqrtPrice > sqrtPriceUpper
          then { sqrtPriceA: sqrtPriceLower, sqrtPriceB: sqrtPriceUpper }  -- Price above range
          else { sqrtPriceA: sqrtPriceLower, sqrtPriceB: sqrtPrice }        -- Price within range
    
    sqrtPriceA = priceRange.sqrtPriceA
    sqrtPriceB = priceRange.sqrtPriceB
    
    -- Calculate amount1 = L * (sqrtPriceB - sqrtPriceA)
    amount = if sqrtPriceA == sqrtPriceB
             then 0.0  -- No token1 needed when price below range
             else liquidity * abs (sqrtPriceB - sqrtPriceA)
  in amount

--------------------------------------------------------------------------------
-- Efficient On-Chain Operations
--------------------------------------------------------------------------------

-- | Execute swap with minimal state updates for POL operations
executeSwap :: POLPool -> Number -> Int -> { pool :: POLPool, event :: PoolEvent }
executeSwap pool amountIn currentSlot =
  let
    -- Why we need AMM math in tick-based systems:
    -- 1. Ticks are discrete boundaries, but price moves continuously between them
    -- 2. We calculate exact swap amounts using x*y=L² within tick ranges
    -- 3. sqrt price format enables efficient price impact calculations
    -- 4. These formulas ensure price continuity when crossing tick boundaries
    
    -- Current reserves from sqrt price and liquidity
    currentPrice = pool.sqrtPrice * pool.sqrtPrice  -- price = sqrtPrice^2
    reserve0 = pool.liquidity / pool.sqrtPrice      -- x = L / sqrt(P)
    reserve1 = pool.liquidity * pool.sqrtPrice      -- y = L * sqrt(P)
    
    -- Apply fee (0.3% = 30 basis points)
    feeRate = 0.003
    amountInAfterFee = amountIn * (1.0 - feeRate)
    fee = amountIn * feeRate
    
    -- Calculate output using constant product formula x*y=k
    -- This maintains the AMM invariant between tick boundaries
    amountOut = reserve1 * amountInAfterFee / (reserve0 + amountInAfterFee)
    
    -- Calculate new reserves after swap
    newReserve0 = reserve0 + amountInAfterFee
    newReserve1 = reserve1 - amountOut
    
    -- Calculate new sqrt price from new reserves
    newSqrtPrice = sqrt (newReserve1 / newReserve0)
    
    -- Update tick based on new sqrt price
    newTick = sqrtPriceToTick newSqrtPrice
    
    -- Calculate fee growth per unit of liquidity
    feeGrowth = if pool.liquidity > 0.0 
                then fee / pool.liquidity 
                else 0.0
    
    -- Calculate time-weighted average price update
    timeDelta = currentSlot - pool.lastUpdateSlot
    alpha = min 1.0 (toNumber timeDelta / 1000.0)  -- Decay factor
    newTwap = pool.twap * (1.0 - alpha) + newSqrtPrice * alpha
    
    -- Update pool state with minimal changes
    updatedPool = pool
      { sqrtPrice = newSqrtPrice
      , currentTick = newTick
      , volume24h = pool.volume24h + amountIn  -- Accumulate volume
      , feeGrowthGlobal = pool.feeGrowthGlobal + feeGrowth
      , lastUpdateSlot = currentSlot
      , twap = newTwap
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

-- | Update TWAP (time-weighted average price)
updateTWAP :: POLPool -> Int -> Number -> POLPool
updateTWAP pool currentSlot newPrice =
  let
    timeDelta = currentSlot - pool.lastUpdateSlot
    alpha = min 1.0 (toNumber timeDelta / 1000.0)  -- Decay factor
    newTwap = pool.twap * (1.0 - alpha) + newPrice * alpha
  in pool { twap = newTwap }

-- | Check if tick is within the pool's active POL range
isTickInPOLRange :: Int -> POLPool -> Boolean
isTickInPOLRange tick pool =
  tick >= pool.polTickLower && tick < pool.polTickUpper && pool.polAmount > 0.0

--------------------------------------------------------------------------------
-- Tranche Value Calculation Functions
--------------------------------------------------------------------------------

-- | Calculate current tranche values based on total pool value and PnL
-- | Implements the waterfall logic where Junior absorbs losses first
calculateTrancheValues :: Number -> Number -> TrancheState -> TrancheValues
calculateTrancheValues initialValue currentValue trancheState =
  let
    -- Calculate profit/loss
    pnl = currentValue - initialValue
    
    -- Get initial tranche values
    initialSenior = trancheState.seniorValue
    initialJunior = trancheState.juniorValue
    
    -- Apply waterfall logic
    result = if pnl >= 0.0 then
      -- Profits: Distribute based on exposure (Junior gets 3x weight)
      let juniorWeight = 3.0  -- Junior has 3x exposure
          seniorWeight = 1.0  -- Senior has 1x exposure
          
          -- Calculate weighted distribution
          totalWeight = (initialJunior * juniorWeight) + (initialSenior * seniorWeight)
          
      in  -- Avoid division by zero
          if totalWeight > 0.0 then
            { senior: initialSenior + pnl * (initialSenior * seniorWeight / totalWeight)
            , junior: initialJunior + pnl * (initialJunior * juniorWeight / totalWeight)
            }
          else
            { senior: initialSenior, junior: initialJunior }
    else
      -- Losses: Junior absorbs first up to 90% of their capital
      let absLoss = abs pnl
          maxJuniorLoss = initialJunior * 0.9  -- Junior can lose up to 90%
          
          -- Calculate actual losses
          juniorLoss = min maxJuniorLoss absLoss
          seniorLoss = max 0.0 (absLoss - juniorLoss)
          
          -- Junior retains at least 10% of initial value
          newJunior = max (initialJunior * 0.1) (initialJunior - juniorLoss)
          newSenior = initialSenior - seniorLoss
          
      in { senior: newSenior, junior: newJunior }
  
  in result

-- | Update tranche state with new values and maintain share price consistency
updateTrancheValues :: TrancheState -> TrancheValues -> TrancheState
updateTrancheValues state newValues =
  state
    { seniorValue = newValues.senior
    , juniorValue = newValues.junior
    }

-- | Distribute pool PnL to positions based on their shares and tranche
-- | Returns updated position with new value
distributePoolPnL :: Position -> TrancheState -> TrancheValues -> Position
distributePoolPnL position oldState newValues =
  let
    -- Calculate per-share values for each tranche
    seniorPerShare = if oldState.seniorShares > 0.0
                     then newValues.senior / oldState.seniorShares
                     else 0.0
                     
    juniorPerShare = if oldState.juniorShares > 0.0
                     then newValues.junior / oldState.juniorShares
                     else 0.0
    
    -- Update position value based on tranche and shares
    newValue = case position.tranche of
      Senior -> position.shares * seniorPerShare
      Junior -> position.shares * juniorPerShare
      
  in position { value = newValue }
-- | Core AMM mechanics with efficient state management
-- |
-- | This module implements concentrated liquidity AMM mechanics using a tick-based system.
-- |
-- | === Pool State Architecture ===
-- |
-- | We maintain two complementary pool representations for different use cases:
-- |
-- |     ┌─────────────────────────────────────────┐
-- |     │           PoolState (Full State)         │
-- |     │  - Complete AMM state                    │
-- |     │  - All position tracking                 │
-- |     │  - Fee growth per token                  │
-- |     │  - Leverage tier tracking                │
-- |     │  - Used for: swaps, liquidity ops       │
-- |     └─────────────────────────────────────────┘
-- |                         │
-- |                         │ Derives metrics
-- |                         ↓
-- |     ┌─────────────────────────────────────────┐
-- |     │      POLPool (Derived Metrics View)     │
-- |     │  - Minimal state for POL decisions      │
-- |     │  - Rolling 24h volume                   │
-- |     │  - TWAP (time-weighted avg price)       │
-- |     │  - Current liquidity & price            │
-- |     │  - Used for: automated POL strategies   │
-- |     └─────────────────────────────────────────┘
-- |
-- | POLPool is NOT a separate pool - it's a lightweight view derived from PoolState
-- | containing only the metrics needed for Protocol-Owned Liquidity decisions.
-- | This separation optimizes gas costs for frequent POL operations while maintaining
-- | a single source of truth in PoolState.
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
  , LeverageState
  , LeverageValues
  , initializePool
  , swap
  , addLiquidity
  , removeLiquidity
  , getPoolPrice
  , getPoolLiquidity
  , executeSwap
  , updateTWAP
  , isTickInPOLRange
  -- Leverage value functions
  , calculateLeverageValues
  , updateLeverageValues
  , distributePoolPnL
  -- Yield functions
  , calculatePositionYield
  , updatePositionYield
  , getAPY
  , syncPositionValue
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Int (toNumber, floor)
import Data.Ord (abs, min, max)
import Data.Array (find, sortBy, zipWith, (:), mapWithIndex)
import Data.Foldable (sum)
import FFI (sqrt, log, pow)
import Protocol.Token (TokenType)
import Protocol.Tick (Tick)
import Protocol.Position (Position, Duration(..), Leverage(..), leverageMultiplier)
import Protocol.Common (PositionId, BlockNumber)
import Protocol.POL (POLTriggerType(..))

--------------------------------------------------------------------------------
-- On-Chain Pool State
--------------------------------------------------------------------------------

-- | Complete pool state - the authoritative on-chain representation
-- |
-- | This is the single source of truth for all pool operations. It contains:
-- | - Full AMM state for swap execution
-- | - All position and liquidity tracking
-- | - Detailed fee accounting per token
-- | - Leverage tier value distribution
-- |
-- | POLPool views are derived from this state, not stored separately.
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
  , leverageState :: LeverageState  -- Tracks value by leverage tier
  , totalValue :: Number            -- Total pool value
  , lastUpdateBlock :: BlockNumber  -- Last value update block
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

-- | Minimal pool metrics view for Protocol-Owned Liquidity (POL) operations
-- |
-- | IMPORTANT: This is a derived view of PoolState, not a separate pool!
-- |
-- | POLPool contains a subset of PoolState fields plus computed metrics needed
-- | for automated liquidity management decisions. It's designed to be:
-- | - Lightweight for frequent reads during POL strategy execution
-- | - Gas-efficient by avoiding unnecessary state access
-- | - Forward-compatible with cross-program invocations
-- |
-- | Derivation from PoolState:
-- | - sqrtPrice, currentTick, liquidity → Direct copies from PoolState
-- | - volume24h → Computed from swap events over last 24h
-- | - twap → Calculated from price history
-- | - feeGrowthGlobal → Simplified view of feeGrowthGlobal0/1
-- | - pol* fields → Specific to POL position within the pool
type POLPool =
  { -- Core state (mirrors PoolState)
    sqrtPrice :: Number         -- Same as PoolState.sqrtPriceX96 (simplified)
  , currentTick :: Int          -- Same as PoolState.tick
  , liquidity :: Number         -- Same as PoolState.liquidity
  
    -- Computed metrics (derived from events/history)
  , volume24h :: Number         -- Rolling 24h volume (computed)
  , feeGrowthGlobal :: Number   -- Simplified fee view (derived)
  , twap :: Number              -- Time-weighted average price (computed)
  , lastUpdateSlot :: Int       -- Last computation timestamp
  
    -- POL-specific state
  , polAmount :: Number         -- POL's liquidity in this pool
  , polTickLower :: Int         -- POL position lower bound
  , polTickUpper :: Int         -- POL position upper bound
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
-- Leverage Value Management
--------------------------------------------------------------------------------

-- | State tracking for leverage groups' aggregate value
type LeverageState =
  { totalValue :: Number        -- Total pool value
  , leverageGroups :: Array     -- Groups by leverage level
      { leverage :: Number      -- Leverage multiplier (1.0, 2.0, 3.0, etc)
      , value :: Number         -- Total value at this leverage
      , shares :: Number        -- Total shares at this leverage
      }
  }

-- | Leverage-based value calculation results
type LeverageValues =
  { values :: Array
      { leverage :: Number
      , value :: Number
      }
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

-- | Execute a swap with pool value tracking
swap :: PoolState -> SwapParams -> BlockNumber -> { result :: SwapResult, updatedPool :: PoolState }
swap pool params currentBlock =
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
    
    finalResult = if priceInBounds
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
    
    -- Update pool state with new price and fees
    updatedPool = if priceInBounds
                  then pool { sqrtPriceX96 = newSqrtPriceX96
                            , tick = newTick
                            , feeGrowthGlobal0X128 = pool.feeGrowthGlobal0X128 + feeAmount
                            , feeGrowthGlobal1X128 = pool.feeGrowthGlobal1X128 + feeAmount
                            }
                  else pool
    
    -- Update pool value and distribute PnL to leverage tiers
    finalPool = updatePoolValue updatedPool currentBlock
    
  in { result: finalResult, updatedPool: finalPool }

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

-- | Add liquidity to pool with position tracking
addLiquidity :: PoolState -> LiquidityParams -> Leverage -> BlockNumber -> { result :: LiquidityResult, updatedPool :: PoolState }
addLiquidity pool params leverage currentBlock =
  let
    -- Calculate token amounts needed for liquidity
    amount0 = calculateAmount0ForLiquidity pool.sqrtPriceX96 params.tickLower params.tickUpper params.amount
    amount1 = calculateAmount1ForLiquidity pool.sqrtPriceX96 params.tickLower params.tickUpper params.amount
    
    -- Position ID would be generated on-chain
    -- For now, create a simple hash from tick range
    positionId = abs (params.tickLower * 1000 + params.tickUpper)
    
    -- Calculate shares based on leverage
    leverageMultiplier' = leverageMultiplier leverage
    shares = params.amount * leverageMultiplier'
    
    -- Update leverage state with new position
    leverageIdx = if leverage == Senior then 0 else 1
    updatedLeverageGroups = mapWithIndex updateGroup pool.leverageState.leverageGroups
      where
        updateGroup idx group = 
          if idx == leverageIdx
          then group { value = group.value + params.amount
                     , shares = group.shares + shares
                     }
          else group
    
    -- Update pool state
    updatedPool = pool { liquidity = pool.liquidity + params.amount
                       , leverageState = pool.leverageState { leverageGroups = updatedLeverageGroups
                                                            , totalValue = pool.leverageState.totalValue + params.amount
                                                            }
                       , totalValue = pool.totalValue + params.amount
                       , lastUpdateBlock = currentBlock
                       }
    
    result = { positionId: positionId
             , liquidity: params.amount
             , amount0: amount0
             , amount1: amount1
             }
    
  in { result: result, updatedPool: updatedPool }

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
  , leverageState: initializeLeverageState
  , totalValue: 0.0
  , lastUpdateBlock: 0
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
-- Leverage State Management
--------------------------------------------------------------------------------

-- | Initialize empty leverage state
initializeLeverageState :: LeverageState
initializeLeverageState =
  { totalValue: 0.0
  , leverageGroups:
    [ { leverage: 1.0   -- Senior (1x)
      , value: 0.0
      , shares: 0.0
      }
    , { leverage: 3.0   -- Junior (3x)
      , value: 0.0
      , shares: 0.0
      }
    ]
  }

-- | Update pool value and distribute PnL to leverage tiers
updatePoolValue :: PoolState -> BlockNumber -> PoolState
updatePoolValue pool currentBlock =
  let
    -- Calculate current pool value from reserves
    currentSqrtPrice = pool.sqrtPriceX96 / 2.0 `pow` 48.0
    reserve0 = pool.liquidity / currentSqrtPrice
    reserve1 = pool.liquidity * currentSqrtPrice
    currentValue = reserve0 + reserve1  -- Simplified: assumes 1:1 value
    
    -- Only update if there's been a change
    hasValueChange = currentValue /= pool.totalValue && pool.totalValue > 0.0
    
    -- Calculate new leverage values if there's a change
    newLeverageState = if hasValueChange
      then
        let leverageValues = calculateLeverageValues pool.totalValue currentValue pool.leverageState
        in updateLeverageValues pool.leverageState leverageValues
      else pool.leverageState
  in
    pool { totalValue = currentValue
         , leverageState = newLeverageState
         , lastUpdateBlock = currentBlock
         }

-- | Sync position value with current pool state and yield
syncPositionValue :: Position -> PoolState -> BlockNumber -> Position
syncPositionValue position pool currentBlock =
  let
    -- First update yield
    positionWithYield = updatePositionYield position pool currentBlock
    
    -- Then update value based on leverage state
    newLeverageValues = { values: map (\g -> { leverage: g.leverage, value: g.value }) pool.leverageState.leverageGroups }
    
    -- Apply pool PnL distribution
    updatedPosition = distributePoolPnL positionWithYield pool.leverageState newLeverageValues
  in
    updatedPosition

-- | Get all positions for a pool and update their values
syncAllPositionValues :: Array Position -> PoolState -> BlockNumber -> Array Position
syncAllPositionValues positions pool currentBlock = map (\pos -> syncPositionValue pos pool currentBlock) positions

--------------------------------------------------------------------------------
-- Yield Calculation and Distribution
--------------------------------------------------------------------------------

-- | Calculate yield earned by a position since last update
calculatePositionYield :: Position -> PoolState -> BlockNumber -> Number
calculatePositionYield position pool currentBlock =
  let
    -- Calculate blocks elapsed since last yield claim
    blocksElapsed = toNumber (currentBlock - position.lastYieldClaim)
    
    -- Get fee growth delta since position's last update
    feeGrowthDelta0 = pool.feeGrowthGlobal0X128 - position.feeGrowthInside0
    feeGrowthDelta1 = pool.feeGrowthGlobal1X128 - position.feeGrowthInside1
    
    -- Calculate yield based on position's shares and fee growth
    -- Positions earn fees proportional to their share of liquidity
    yieldFromFees = position.shares * (feeGrowthDelta0 + feeGrowthDelta1) / 2.0
    
    -- Apply leverage multiplier - higher leverage earns more yield
    leverageBonus = leverageMultiplier position.leverage
    
    -- Duration bonus - longer commitments earn higher yield
    durationMultiplier = case position.duration of
      Spot -> 1.0      -- Base rate
      Monthly -> 1.2   -- 20% bonus for monthly commitment
    
    totalYield = yieldFromFees * leverageBonus * durationMultiplier
  in
    max 0.0 totalYield  -- Ensure non-negative yield

-- | Update position with accumulated yield
updatePositionYield :: Position -> PoolState -> BlockNumber -> Position
updatePositionYield position pool currentBlock =
  let
    newYield = calculatePositionYield position pool currentBlock
    
    -- Update position with new yield and fee growth markers
    updatedPosition = position 
      { accumulatedYield = position.accumulatedYield + newYield
      , value = position.value + newYield  -- Add yield to position value
      , lastYieldClaim = currentBlock
      , feeGrowthInside0 = pool.feeGrowthGlobal0X128
      , feeGrowthInside1 = pool.feeGrowthGlobal1X128
      }
  in
    updatedPosition

-- | Calculate APY for a position class
getAPY :: PoolState -> Duration -> Leverage -> Number
getAPY pool duration leverage =
  let
    -- Base APY from pool fees (assuming 0.3% fee and average daily volume)
    -- This is simplified - in production would use actual volume data
    baseAPY = 0.05  -- 5% base APY
    
    -- Apply multipliers
    leverageMultiplier' = leverageMultiplier leverage
    durationMultiplier = case duration of
      Spot -> 1.0
      Monthly -> 1.2
    
    -- Calculate total APY
    totalAPY = baseAPY * leverageMultiplier' * durationMultiplier
  in
    totalAPY

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
-- Pool State Conversions
--------------------------------------------------------------------------------

-- | Example: How to derive POLPool view from PoolState
-- | In practice, this would be done by:
-- | 1. Reading minimal fields from PoolState
-- | 2. Computing metrics from indexed events
-- | 3. Loading POL-specific position data
-- |
-- | derivePOLView :: PoolState -> Effect POLPool
-- | derivePOLView poolState = do
-- |   -- Get computed metrics from indexer
-- |   volume24h <- getPoolVolume24h poolState.token0 poolState.token1
-- |   twap <- calculateTWAP poolState
-- |   polPosition <- getPOLPosition poolState.token0 poolState.token1
-- |   
-- |   pure {
-- |     -- Direct fields from PoolState
-- |     sqrtPrice: poolState.sqrtPriceX96 / pow 2.0 48.0,  -- Simplify format
-- |     currentTick: poolState.tick,
-- |     liquidity: poolState.liquidity,
-- |     
-- |     -- Computed metrics
-- |     volume24h: volume24h,
-- |     feeGrowthGlobal: (poolState.feeGrowthGlobal0X128 + 
-- |                       poolState.feeGrowthGlobal1X128) / 2.0,
-- |     twap: twap,
-- |     lastUpdateSlot: currentSlot,
-- |     
-- |     -- POL position data
-- |     polAmount: polPosition.liquidity,
-- |     polTickLower: polPosition.tickLower,
-- |     polTickUpper: polPosition.tickUpper
-- |   }

--------------------------------------------------------------------------------
-- Efficient On-Chain Operations
--------------------------------------------------------------------------------

-- | Execute swap with minimal state updates for POL operations
-- | This operates on the POLPool view for gas efficiency
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
-- Leverage Value Calculation Functions
--------------------------------------------------------------------------------

-- | Calculate leverage-based values with PnL distribution
-- | Higher leverage positions absorb losses first and gain more from profits
calculateLeverageValues :: Number -> Number -> LeverageState -> LeverageValues
calculateLeverageValues initialValue currentValue leverageState =
  let
    -- Calculate profit/loss
    pnl = currentValue - initialValue
    
    -- Apply leverage-based distribution
    result = if pnl >= 0.0 then
      -- Profits: Distribute proportionally to leverage
      let totalWeightedValue = sum $ map (\g -> g.value * g.leverage) leverageState.leverageGroups
      in if totalWeightedValue > 0.0 then
           { values: map (\g -> 
               { leverage: g.leverage
               , value: g.value + pnl * (g.value * g.leverage / totalWeightedValue)
               }) leverageState.leverageGroups
           }
         else
           { values: map (\g -> { leverage: g.leverage, value: g.value }) leverageState.leverageGroups }
    else
      -- Losses: Higher leverage absorbs first
      distributeLosses (abs pnl) leverageState.leverageGroups
  
  in result
  where
    -- Helper to distribute losses starting from highest leverage
    distributeLosses :: Number -> Array { leverage :: Number, value :: Number, shares :: Number } -> LeverageValues
    distributeLosses _ [] = { values: [] }
    distributeLosses remainingLoss groups =
      let sorted = sortBy (\a b -> compare b.leverage a.leverage) groups
          distributed = distributeToGroups remainingLoss sorted []
      in { values: map (\g -> { leverage: g.leverage, value: g.value }) distributed }
    
    distributeToGroups :: Number -> Array { leverage :: Number, value :: Number, shares :: Number } -> Array { leverage :: Number, value :: Number, shares :: Number } -> Array { leverage :: Number, value :: Number, shares :: Number }
    distributeToGroups _ [] acc = acc
    distributeToGroups 0.0 remaining acc = acc <> remaining
    distributeToGroups loss groups acc = case Array.uncons groups of
      Nothing -> acc
      Just { head: g, tail: gs } ->
        let maxLoss = g.value * 0.9  -- Can lose up to 90%
            actualLoss = min loss maxLoss
            newValue = max (g.value * 0.1) (g.value - actualLoss)
            remainingLoss = loss - actualLoss
        in distributeToGroups remainingLoss gs (acc <> [g { value = newValue }])

-- | Update leverage state with new values
updateLeverageValues :: LeverageState -> LeverageValues -> LeverageState
updateLeverageValues state newValues =
  state { leverageGroups = zipWith updateGroup state.leverageGroups newValues.values }
  where
    updateGroup g v = g { value = v.value }

-- | Distribute pool PnL to positions based on their shares and leverage
-- | Returns updated position with new value
distributePoolPnL :: Position -> LeverageState -> LeverageValues -> Position
distributePoolPnL position oldState newValues =
  let
    -- Find the leverage group for this position
    posLeverage = leverageMultiplier position.leverage
    leverageGroup = find (\g -> g.leverage == posLeverage) oldState.leverageGroups
    valueGroup = find (\v -> v.leverage == posLeverage) newValues.values
    
    -- Calculate per-share value for this leverage level
    perShareValue = case { lg: leverageGroup, vg: valueGroup } of
      { lg: Just lg', vg: Just vg' } -> 
        if lg'.shares > 0.0 
        then vg'.value / lg'.shares 
        else 0.0
      _ -> 0.0
    
    -- Update position value based on shares
    newValue = position.shares * perShareValue
      
  in position { value = newValue }
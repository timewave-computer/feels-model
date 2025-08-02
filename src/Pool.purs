module Pool where

import Prelude
import Data.Array ((:), filter, find, foldl, sortBy, head, tail)
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt, abs, pow)
import Data.Int (toNumber, floor)
import Data.Ord (compare)
import Effect (Effect)
import Effect.Console (log)
import Token (TokenType(..))
import Position (Position, PositionParams, TickIndex, defaultParams, priceToTick, tickToPrice)
import System (SystemParams)
import Risk (calculatePositionFee)
import FFI (currentTime, generateId)

--------------------------------------------------------------------------------
-- Pool Types
--------------------------------------------------------------------------------

-- Tick state at a specific price level
type TickState =
  { liquidityGross :: Number  -- Total liquidity referenced at this tick
  , liquidityNet :: Number    -- Net liquidity change when crossing this tick
  , feeGrowthOutside0 :: Number  -- Fee growth on other side of tick (token0)
  , feeGrowthOutside1 :: Number  -- Fee growth on other side of tick (token1)
  , initialized :: Boolean    -- Whether the tick is initialized
  }

-- Tick with its index
type IndexedTick = { index :: TickIndex, state :: TickState }

-- Note: Using the unified Position type from Position module
-- No need for a separate LiquidityPosition type

-- Pool state
type PoolState =
  { token0 :: TokenType          -- Base token (always Synthetic SOL)
  , token1 :: TokenType          -- Quote token (TokenA or TokenB)
  , fee :: Number                -- Base fee rate (0.003 = 0.3%)
  , liquidity :: Number          -- Active liquidity
  , sqrtPriceX96 :: Number       -- Current sqrt price (fixed point)
  , currentTick :: TickIndex     -- Current tick
  , currentPrice :: Number       -- Current price (for display)
  , feeGrowthGlobal0 :: Number   -- Global fee accumulator token0
  , feeGrowthGlobal1 :: Number   -- Global fee accumulator token1
  , protocolFees0 :: Number      -- Protocol accumulated fees token0
  , protocolFees1 :: Number      -- Protocol accumulated fees token1
  , positions :: Array Position  -- All positions
  , ticks :: Array IndexedTick   -- Active ticks
  }

-- Result of a swap operation
type SwapResult =
  { amountIn :: Number
  , amountOut :: Number
  , fee :: Number
  , newPrice :: Number
  , newTick :: TickIndex
  , newLiquidity :: Number
  }

--------------------------------------------------------------------------------
-- Pool Initialization
--------------------------------------------------------------------------------

-- Initialize a new pool
initPool :: TokenType -> TokenType -> Number -> Effect PoolState
initPool token0 token1 initialPrice = do
  timestamp <- currentTime
  let currentTick = priceToTick initialPrice
  pure { token0: token0
       , token1: token1
       , fee: 0.003  -- 0.3% default fee
       , liquidity: 0.0
       , sqrtPriceX96: sqrt initialPrice * pow 2.0 48.0
       , currentTick: currentTick
       , currentPrice: initialPrice
       , feeGrowthGlobal0: 0.0
       , feeGrowthGlobal1: 0.0
       , protocolFees0: 0.0
       , protocolFees1: 0.0
       , positions: []
       , ticks: []
       }

--------------------------------------------------------------------------------
-- Liquidity Management
--------------------------------------------------------------------------------

-- Calculate liquidity from token amounts
-- L = sqrt(x * y) for concentrated liquidity
calculateLiquidity :: Number -> Number -> Number -> Number -> Number
calculateLiquidity amount0 amount1 priceLower priceUpper =
  let sqrtPriceLower = sqrt priceLower
      sqrtPriceUpper = sqrt priceUpper
      -- For simplicity, using geometric mean
      -- In production, this would use the full Uniswap V3 formula
  in sqrt (amount0 * amount1)

-- Calculate token amounts from liquidity
-- Returns the amount of each token for a given liquidity and price range
token0FromLiquidity :: Number -> TickIndex -> TickIndex -> TickIndex -> Number
token0FromLiquidity liquidity currentTick tickLower tickUpper =
  if currentTick < tickLower then
    -- All liquidity is in token0
    liquidity * (sqrt (tickToPrice tickUpper) - sqrt (tickToPrice tickLower)) / sqrt (tickToPrice tickLower)
  else if currentTick >= tickUpper then
    -- No token0
    0.0
  else
    -- Partially in range
    liquidity * (sqrt (tickToPrice tickUpper) - sqrt (tickToPrice currentTick)) / sqrt (tickToPrice currentTick)

token1FromLiquidity :: Number -> TickIndex -> TickIndex -> TickIndex -> Number
token1FromLiquidity liquidity currentTick tickLower tickUpper =
  if currentTick < tickLower then
    -- No token1
    0.0
  else if currentTick >= tickUpper then
    -- All liquidity is in token1
    liquidity * (sqrt (tickToPrice tickUpper) - sqrt (tickToPrice tickLower))
  else
    -- Partially in range
    liquidity * (sqrt (tickToPrice currentTick) - sqrt (tickToPrice tickLower))

-- Add liquidity to pool
addLiquidity :: PoolState -> String -> Number -> Number -> TickIndex -> TickIndex -> PositionParams -> SystemParams -> Effect { pool :: PoolState, position :: Position, amount0Used :: Number, amount1Used :: Number }
addLiquidity pool owner amount0Desired amount1Desired tickLower tickUpper params sysParams = do
  timestamp <- currentTime
  
  -- Calculate liquidity from amounts
  let priceLower = tickToPrice tickLower
      priceUpper = tickToPrice tickUpper
      liquidity = calculateLiquidity amount0Desired amount1Desired priceLower priceUpper
  
  -- Calculate actual amounts used (may differ due to rounding)
  let amount0Used = token0FromLiquidity liquidity pool.currentTick tickLower tickUpper
      amount1Used = token1FromLiquidity liquidity pool.currentTick tickLower tickUpper
  
  -- Create new position using unified type
  let newPosition = { id: generateId timestamp
                    , owner: owner
                    , inputToken: { tokenType: pool.token0, amount: amount0Used }  -- Using token0 as input
                    , liquidity: liquidity
                    , createdAt: timestamp
                    , params: params { tickLower = tickLower, tickUpper = tickUpper }
                    , liquidityGross: liquidity
                    , liquidityNet: liquidity
                    , feeGrowthInside0: 0.0
                    , feeGrowthInside1: 0.0
                    , tokensOwed0: 0.0
                    , tokensOwed1: 0.0
                    }
  
  -- Update ticks
  let updatedTicks = updateTickLiquidity (updateTickLiquidity pool.ticks tickLower liquidity true) tickUpper liquidity false
  
  -- Update pool liquidity if position is in range
  let newLiquidity = if pool.currentTick >= tickLower && pool.currentTick < tickUpper
                     then pool.liquidity + liquidity
                     else pool.liquidity
  
  -- Add position to pool
  let updatedPool = pool { positions = newPosition : pool.positions
                         , ticks = updatedTicks
                         , liquidity = newLiquidity
                         }
  
  pure { pool: updatedPool
       , position: newPosition
       , amount0Used: amount0Used
       , amount1Used: amount1Used
       }

--------------------------------------------------------------------------------
-- Swap Functionality
--------------------------------------------------------------------------------

-- Swap tokens in the pool
swap :: PoolState -> TokenType -> Number -> SystemParams -> Effect { pool :: PoolState, amountOut :: Number, fee :: Number }
swap pool tokenIn amountIn sysParams = do
  -- Determine if we're swapping token0 for token1 or vice versa
  let isToken0 = tokenIn == pool.token0
      zeroForOne = isToken0
  
  -- Calculate swap parameters
  let sqrtPriceX96 = pool.sqrtPriceX96
      liquidity = pool.liquidity
  
  -- Simplified swap calculation
  -- In production, this would step through ticks and handle liquidity changes
  let priceImpact = amountIn / (liquidity * 2.0)  -- Simplified price impact
      effectivePrice = if zeroForOne
                       then pool.currentPrice * (1.0 - priceImpact)
                       else pool.currentPrice * (1.0 + priceImpact)
      
      -- Calculate output amount
      amountOut = if zeroForOne
                  then amountIn * effectivePrice
                  else amountIn / effectivePrice
      
      -- Calculate fee
      baseFee = pool.fee * amountIn
      firstPosition = head pool.positions
      positionParams = case firstPosition of
        Just pos -> pos.params
        Nothing -> defaultParams pool.token1
      paramsFee = calculatePositionFee positionParams sysParams
      totalFee = baseFee + paramsFee
  
  -- Update pool state
  let newPrice = effectivePrice
      newTick = priceToTick newPrice
      newSqrtPriceX96 = sqrt newPrice * pow 2.0 48.0
      
      -- Update fee growth
      feeGrowth0 = if isToken0 then totalFee / liquidity else 0.0
      feeGrowth1 = if isToken0 then 0.0 else totalFee / liquidity
      
      updatedPool = pool { currentPrice = newPrice
                         , currentTick = newTick
                         , sqrtPriceX96 = newSqrtPriceX96
                         , feeGrowthGlobal0 = pool.feeGrowthGlobal0 + feeGrowth0
                         , feeGrowthGlobal1 = pool.feeGrowthGlobal1 + feeGrowth1
                         }
  
  pure { pool: updatedPool
       , amountOut: amountOut - totalFee
       , fee: totalFee
       }

--------------------------------------------------------------------------------
-- Position Management
--------------------------------------------------------------------------------

-- Remove liquidity from a position
removeLiquidity :: PoolState -> Int -> Number -> Effect { pool :: PoolState, amount0 :: Number, amount1 :: Number }
removeLiquidity pool positionId liquidityAmount = do
  case find (\p -> p.id == positionId) pool.positions of
    Nothing -> pure { pool: pool, amount0: 0.0, amount1: 0.0 }
    Just position -> do
      -- Calculate token amounts to return
      let amount0 = token0FromLiquidity liquidityAmount pool.currentTick position.params.tickLower position.params.tickUpper
          amount1 = token1FromLiquidity liquidityAmount pool.currentTick position.params.tickLower position.params.tickUpper
      
      -- Update position liquidity
      let updatedPosition = position { liquidity = position.liquidity - liquidityAmount }
          updatedPositions = map (\p -> if p.id == positionId then updatedPosition else p) pool.positions
      
      -- Update ticks
      let updatedTicks = updateTickLiquidity (updateTickLiquidity pool.ticks position.params.tickLower (-liquidityAmount) true) position.params.tickUpper (-liquidityAmount) false
      
      -- Update pool liquidity if position is in range
      let newLiquidity = if pool.currentTick >= position.params.tickLower && pool.currentTick < position.params.tickUpper
                         then pool.liquidity - liquidityAmount
                         else pool.liquidity
      
      let updatedPool = pool { positions = updatedPositions
                             , ticks = updatedTicks
                             , liquidity = newLiquidity
                             }
      
      pure { pool: updatedPool, amount0: amount0, amount1: amount1 }

-- Collect fees from a position
collectFees :: PoolState -> Int -> Effect { pool :: PoolState, fees0 :: Number, fees1 :: Number }
collectFees pool positionId = do
  case find (\p -> p.id == positionId) pool.positions of
    Nothing -> pure { pool: pool, fees0: 0.0, fees1: 0.0 }
    Just position -> do
      -- Calculate fees earned
      -- Simplified: in production, this would track fee growth inside position's range
      let fees0 = position.tokensOwed0
          fees1 = position.tokensOwed1
      
      -- Reset fees owed
      let updatedPosition = position { tokensOwed0 = 0.0, tokensOwed1 = 0.0 }
          updatedPositions = map (\p -> if p.id == positionId then updatedPosition else p) pool.positions
          updatedPool = pool { positions = updatedPositions }
      
      pure { pool: updatedPool, fees0: fees0, fees1: fees1 }

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get total value locked in pool (in token0 terms)
getPoolTVL :: PoolState -> Number
getPoolTVL pool =
  foldl (\acc pos ->
    let amount0 = token0FromLiquidity pos.liquidity pool.currentTick pos.params.tickLower pos.params.tickUpper
        amount1 = token1FromLiquidity pos.liquidity pool.currentTick pos.params.tickLower pos.params.tickUpper
    in acc + amount0 + (amount1 * pool.currentPrice)
  ) 0.0 pool.positions

-- Get position by ID
getPosition :: PoolState -> Int -> Maybe Position
getPosition pool positionId = find (\p -> p.id == positionId) pool.positions

-- Get positions for an owner
getOwnerPositions :: PoolState -> String -> Array Position
getOwnerPositions pool owner = filter (\p -> p.owner == owner) pool.positions

--------------------------------------------------------------------------------
-- Tick Update Functions
--------------------------------------------------------------------------------

-- Update tick liquidity when adding/removing positions
updateTickLiquidity :: Array IndexedTick -> TickIndex -> Number -> Boolean -> Array IndexedTick
updateTickLiquidity ticks tickIndex liquidityDelta isLower =
  case find (\t -> t.index == tickIndex) ticks of
    Just _ ->
      map (\t -> if t.index == tickIndex
                 then t { state = t.state { liquidityGross = t.state.liquidityGross + liquidityDelta
                                          , liquidityNet = t.state.liquidityNet + if isLower then liquidityDelta else -liquidityDelta
                                          , initialized = true
                                          }
                        }
                 else t) ticks
    Nothing ->
      let newTick = { index: tickIndex
                    , state: { liquidityGross: liquidityDelta
                             , liquidityNet: if isLower then liquidityDelta else -liquidityDelta
                             , feeGrowthOutside0: 0.0
                             , feeGrowthOutside1: 0.0
                             , initialized: true
                             }
                    }
      in sortBy (\a b -> compare a.index b.index) (newTick : ticks)
  where
    map :: forall a b. (a -> b) -> Array a -> Array b
    map _ [] = []
    map f xs = [] -- Simplified

-- Get active tick range (ticks with liquidity)
getActiveTicks :: PoolState -> Array TickIndex
getActiveTicks pool = 
  map _.index $ filter (\t -> t.state.liquidityGross > 0.0) pool.ticks

-- Calculate impermanent loss for a position
-- Returns percentage loss compared to just holding
calculateImpermanentLoss :: PoolState -> Position -> Number
calculateImpermanentLoss pool position =
  let initialPrice = tickToPrice ((position.params.tickLower + position.params.tickUpper) / 2)
      currentPrice = pool.currentPrice
      priceRatio = currentPrice / initialPrice
      -- IL = 2 * sqrt(priceRatio) / (1 + priceRatio) - 1
      il = 2.0 * sqrt priceRatio / (1.0 + priceRatio) - 1.0
  in abs il * 100.0  -- Return as percentage 
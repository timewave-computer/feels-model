-- | 3D Pool - Multi-dimensional AMM with concentrated liquidity
-- |
-- | This module implements a three-dimensional AMM using a weighted
-- | geometric mean invariant extended to (rate, duration, leverage) space:
-- |
-- | Invariant: R^wr * D^wd * L^wl = K
-- |
-- | Key features:
-- | - Concentrated liquidity in 3D "cubes" (rate × duration × leverage ranges)
-- | - Weighted geometric mean pricing between dimensions
-- | - No liquidations - positions redenominate based on performance
-- | - Continuous liquidity with precise tick math
module Protocol.Pool
  ( -- Core types
    Pool
    , TickCoordinate
    , LiquidityCube3D
    , PoolPosition
    , GlobalState3D
    , VirtualBalances
    , DimensionWeights
    , SwapResult3D
    , YieldRates
    , RiskParameters
    , CompositeBalance
    , LossDistribution
    , MaturityInfo
    , TickData
    -- Pool operations
  , initPool
  , addLiquidity3D
  , removeLiquidity3D
  , swap3D
  -- Redenomination
  , redenominatePoolPositions
  , redenominateCube
  , redenominatePoolPosition
  , redenominateCubeWithWaterfall
  -- Risk and adjustments
  , calculateEffectiveWeights
  , calculateTimeAdjustedWeight
  , calculateInvariant3DWithAdjustments
  , calculateSpotPrice3DWithRisk
  , calculateLossDistribution
  -- Composite views
  , calculateCompositeBalance
  , getCompositePrice
  , calculateSwapWithComposite
  , getMaturityInfo
  -- Tick math
  , getSqrtRateAtTick
  , getTickAtSqrtRate
  , getAmount0For3DLiquidity
  , getAmount1For3DLiquidity
  , getLiquidityFor3DAmounts
  -- AMM math
  , calculateSpotPrice3D
  , calculateInvariant3D
  , getVirtualBalances
  -- Tick management (simplified)
  , insertTick
  , removeTick
  , lookupTick
  , updateTick
  , findTicksInRange
  , getActiveTicks
  -- Helpers
  , toQ96
  , fromQ96
  ) where

import Prelude
import Data.Array (filter, sortBy, take, foldl, length, (:), any, mapMaybe, map, concat, concatMap)
import Data.Foldable (sum)
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Tuple (Tuple(..), uncurry)
import Data.Ord (min, max, abs)
import Effect (Effect)
import FFI (pow, log)
import Protocol.Token (TokenType)
import Protocol.PositionTypes (Duration(..), Leverage(..))
import Protocol.Common (BlockNumber, PositionId)
-- Time constants (12 second blocks)
blocksPerMonth :: Int  
blocksPerMonth = 201600  -- 28 days * 24 hours * 60 minutes * 60 seconds / 12 seconds per block

--------------------------------------------------------------------------------
-- CORE TYPES
--------------------------------------------------------------------------------

-- | 3D Pool state with weighted liquidity
type Pool =
  { id :: String
  , token :: TokenType                           -- Token being lent/borrowed
  , liquidityCubes :: Map String LiquidityCube3D -- Concentrated liquidity positions
  , activeTicks :: Array TickCoordinate          -- Ticks with liquidity
  , tickData :: Map String TickData              -- Tick data storage (simplified)
  , positions :: Map PositionId PoolPosition     -- Pool liquidity positions
  , globalState :: GlobalState3D                 -- Current pool state
  , feeRate :: Number                            -- Trading fee (basis points)
  , weights :: DimensionWeights                  -- AMM dimension weights
  , riskParams :: RiskParameters                 -- Risk multipliers for tranches
  , creationBlock :: BlockNumber                 -- Pool creation block
  , totalDeposited :: Number                     -- Total deposits for loss calculation
  , lastUpdateBlock :: BlockNumber               -- Last state update
  }

-- | Coordinate in 3D tick space
type TickCoordinate =
  { rateTick :: Int       -- Interest rate tick using sqrt pricing
  , durationTick :: Int   -- 0 = Flash, 1 = Monthly, 2 = Spot
  , leverageTick :: Int   -- 0 = Senior (1x), 1 = Junior (3x)
  }

-- | Concentrated liquidity in a 3D cube (pure redenomination model)
type LiquidityCube3D =
  { id :: String
  , owner :: String
  , tickLower :: TickCoordinate        -- Lower bound of cube
  , tickUpper :: TickCoordinate        -- Upper bound of cube
  , liquidity :: Number                -- Current liquidity (redenominated)
  , liquidityBase :: Number            -- Original liquidity for tracking growth
  , lastRedenomination :: BlockNumber  -- Last redenomination block
  , initialized :: Boolean
  }

-- | Pool liquidity position in the 3D pool (pure redenomination model)
type PoolPosition =
  { id :: PositionId
  , owner :: String
  , liquidityCubeId :: String
  , liquidity :: Number                -- Current liquidity (redenominated)
  , principal :: Number               -- Original investment
  , currentValue :: Number            -- Current value after redenomination
  , lastRedenomination :: BlockNumber -- Last redenomination block
  }

-- | Global pool state for 3D AMM
type GlobalState3D =
  { currentTick :: TickCoordinate      -- Current position in 3D space
  , sqrtRateX96 :: Number             -- Current sqrt rate in Q64.96
  , virtualBalances :: VirtualBalances -- Current virtual balances
  , yieldRates :: YieldRates          -- Current yield rates per dimension
  , totalLiquidity :: Number          -- Total active liquidity
  , lastRedenomination :: BlockNumber  -- Last global redenomination
  }

-- | Virtual balances for AMM invariant
type VirtualBalances =
  { r :: Number  -- Rate dimension balance
  , d :: Number  -- Duration dimension balance
  , l :: Number  -- Leverage dimension balance
  }

-- | Dimension weights for AMM invariant (sum to 1.0)
type DimensionWeights =
  { wr :: Number  -- Rate weight
  , wd :: Number  -- Duration weight  
  , wl :: Number  -- Leverage weight
  }

-- | Yield rates for different dimensions (per block)
type YieldRates =
  { rateYield :: Number      -- Yield from interest rate dimension
  , durationYield :: Number  -- Yield from time value convergence
  , leverageYield :: Number  -- Yield from leverage premium
  }

-- | Result of a swap through 3D space
type SwapResult3D =
  { amountIn :: Number
  , amountOut :: Number
  , executionTick :: TickCoordinate
  , feeAmount :: Number
  , sqrtRateAfter :: Number
  }

-- | Risk parameters for leverage tranches
type RiskParameters =
  { alphaSenior :: Number  -- Risk multiplier for Senior (1.0)
  , alphaJunior :: Number  -- Risk multiplier for Junior (3.0)
  }

-- | Composite balance view for simplified trading
type CompositeBalance = 
  { rateComponent :: Number
  , timeValueComponent :: Number  -- Duration-adjusted component
  , leverageComponent :: Number   -- Risk-adjusted component
  , total :: Number              -- Combined composite value
  }

-- | Loss distribution for risk waterfall
type LossDistribution =
  { totalLoss :: Number
  , juniorLoss :: Number
  , seniorLoss :: Number
  }

-- | Maturity information for duration positions
type MaturityInfo =
  { maturityBlock :: BlockNumber
  , timeToMaturity :: Number  -- 0 to 1
  , isMatured :: Boolean
  }

--------------------------------------------------------------------------------
-- CONSTANTS AND UTILITIES
--------------------------------------------------------------------------------

-- | Q96 for fixed-point math
toQ96 :: Number -> Number
toQ96 x = x * pow 2.0 96.0

-- | Convert from Q96
fromQ96 :: Number -> Number  
fromQ96 x = x / pow 2.0 96.0

-- | Minimum liquidity constant
minLiquidity :: Number
minLiquidity = 1000.0

-- | Blocks per year (assuming ~12 second blocks)
blocksPerYear :: Number
blocksPerYear = 2628000.0

-- | Monthly yield convergence rate
monthlyConvergenceRate :: Number
monthlyConvergenceRate = 0.001  -- 0.1% per month

--------------------------------------------------------------------------------
-- POOL INITIALIZATION
--------------------------------------------------------------------------------

-- | Initialize a new 3D pool with weighted dimensions
initPool :: String -> TokenType -> Number -> DimensionWeights -> BlockNumber -> Pool
initPool poolId token feeRate weights currentBlock =
  { id: poolId
  , token: token
  , liquidityCubes: Map.empty
  , activeTicks: []
  , tickData: Map.empty
  , positions: Map.empty
  , globalState: initGlobalState currentBlock
  , feeRate: feeRate
  , weights: weights
  , riskParams: defaultRiskParameters
  , creationBlock: currentBlock
  , totalDeposited: 0.0
  , lastUpdateBlock: currentBlock
  }

-- | Default risk parameters
defaultRiskParameters :: RiskParameters
defaultRiskParameters =
  { alphaSenior: 1.0   -- 1x risk for Senior
  , alphaJunior: 3.0   -- 3x risk for Junior
  }

-- | Initialize global state
initGlobalState :: BlockNumber -> GlobalState3D
initGlobalState currentBlock =
  { currentTick: { rateTick: 0, durationTick: 1, leverageTick: 0 }  -- Start at 0% rate, Monthly, Senior
  , sqrtRateX96: toQ96 1.0  -- sqrt(1.0) in Q64.96 format
  , virtualBalances: { r: 1000.0, d: 1000.0, l: 1000.0 }  -- Initial balanced liquidity
  , yieldRates: { rateYield: 0.0001, durationYield: 0.00005, leverageYield: 0.00008 }  -- Initial yield rates per block
  , totalLiquidity: 0.0
  , lastRedenomination: currentBlock
  }

--------------------------------------------------------------------------------
-- LIQUIDITY OPERATIONS
--------------------------------------------------------------------------------

-- | Add liquidity to a 3D range
addLiquidity3D :: Pool -> String -> TickCoordinate -> TickCoordinate -> Number -> Number -> BlockNumber -> Pool
addLiquidity3D pool owner tickLower tickUpper amount0 amount1 currentBlock =
  let
    -- Create liquidity cube ID
    cubeId = owner <> "-" <> coordinateKey tickLower <> "-" <> coordinateKey tickUpper
    
    -- Calculate liquidity from amounts
    liquidity = getLiquidityFor3DAmounts amount0 amount1 tickLower tickUpper pool.globalState.sqrtRateX96
    
    -- Create liquidity cube
    cube = { id: cubeId
           , owner: owner
           , tickLower: tickLower
           , tickUpper: tickUpper
           , liquidity: liquidity
           , liquidityBase: liquidity  -- Track original liquidity
           , lastRedenomination: currentBlock
           , initialized: true
           }
    
    -- Create pool position
    position = { id: cubeId
               , owner: owner
               , liquidityCubeId: cubeId
               , liquidity: liquidity
               , principal: amount0 + amount1
               , currentValue: amount0 + amount1
               , lastRedenomination: currentBlock
               }
    
    -- Update pool state
    updatedCubes = Map.insert cubeId cube pool.liquidityCubes
    updatedPositions = Map.insert cubeId position pool.positions
    updatedActiveTicks = addToActiveTicks pool.activeTicks tickLower tickUpper
    updatedGlobalState = pool.globalState { totalLiquidity = pool.globalState.totalLiquidity + liquidity }
    
  in pool { liquidityCubes = updatedCubes
          , positions = updatedPositions
          , activeTicks = updatedActiveTicks
          , globalState = updatedGlobalState
          , totalDeposited = pool.totalDeposited + amount0 + amount1  -- Track deposits
          , lastUpdateBlock = currentBlock
          }

-- | Remove liquidity from a pool position
removeLiquidity3D :: Pool -> PositionId -> Number -> BlockNumber -> Either String Pool
removeLiquidity3D pool positionId liquidityAmount currentBlock = do
  poolPosition <- case Map.lookup positionId pool.positions of
    Nothing -> Left "Pool position not found"
    Just p -> Right p
    
  -- First redenominate the pool position
  let redenominatedPosition = redenominatePoolPosition pool poolPosition currentBlock
  
  if redenominatedPosition.liquidity < liquidityAmount
    then Left "Insufficient liquidity"
    else do
      cube <- case Map.lookup position.liquidityCubeId pool.liquidityCubes of
        Nothing -> Left "Liquidity cube not found"
        Just c -> Right c
      
      -- Redenominate the cube first
      let redenominatedCube = redenominateCube pool currentBlock cube
      
      let
        -- Calculate amounts to return based on redenominated liquidity
        shareOfCube = liquidityAmount / redenominatedCube.liquidity
        amount0 = getAmount0For3DLiquidity liquidityAmount redenominatedCube.tickLower redenominatedCube.tickUpper pool.globalState.sqrtRateX96
        amount1 = getAmount1For3DLiquidity liquidityAmount redenominatedCube.tickLower redenominatedCube.tickUpper pool.globalState.sqrtRateX96
        
        -- Update pool position
        remainingLiquidity = redenominatedPosition.liquidity - liquidityAmount
        poolPosition' = redenominatedPosition
          { liquidity = remainingLiquidity
          , currentValue = redenominatedPosition.currentValue * remainingLiquidity / redenominatedPosition.liquidity
          }
        
        -- Update cube
        cube' = redenominatedCube 
          { liquidity = redenominatedCube.liquidity - liquidityAmount }
        
        -- Update pool state
        positions' = if remainingLiquidity > minLiquidity
                    then Map.insert positionId poolPosition' pool.positions
                    else Map.delete positionId pool.positions
        
        cubes' = if cube'.liquidity > minLiquidity
                then Map.insert cube.id cube' pool.liquidityCubes
                else Map.delete cube.id pool.liquidityCubes
        
        globalState' = pool.globalState
          { totalLiquidity = pool.globalState.totalLiquidity - liquidityAmount }
        
        -- Update total deposited (subtract withdrawn amounts)
        totalDeposited' = pool.totalDeposited - (amount0 + amount1)
        
      pure $ pool
        { positions = positions'
        , liquidityCubes = cubes'
        , globalState = globalState'
        , totalDeposited = totalDeposited'
        , lastUpdateBlock = currentBlock
        }

--------------------------------------------------------------------------------
-- SWAP OPERATIONS
--------------------------------------------------------------------------------

-- | Execute a swap through the 3D AMM with fee distribution via redenomination
swap3D :: Pool -> TickCoordinate -> Number -> Boolean -> BlockNumber -> Tuple Pool SwapResult3D
swap3D pool targetTick amountIn isExactInput currentBlock =
  let
    -- Get current virtual balances
    currentBalances = pool.globalState.virtualBalances
    
    -- Calculate price impact using Balancer formula with risk adjustment
    spotPrice = calculateSpotPrice3DWithRisk pool currentBalances pool.globalState.currentTick targetTick currentBlock
    
    -- Calculate output amount using invariant preservation with adjustments
    k_before = calculateInvariant3DWithAdjustments pool currentBalances pool.globalState.currentTick currentBlock
    
    -- Update balances based on swap direction
    Tuple newBalances amountOut = executeSwapInvariant pool currentBalances targetTick amountIn isExactInput
    
    -- Apply fee
    feeAmount = amountIn * pool.feeRate / 10000.0
    finalAmountOut = amountOut * (1.0 - pool.feeRate / 10000.0)
    
    -- Calculate new sqrt rate
    newSqrtRate = calculateNewSqrtRate pool.globalState.currentTick targetTick pool.globalState.sqrtRateX96
    
    -- Distribute fees to affected cubes via redenomination
    affectedCubes = getAffectedCubes pool pool.globalState.currentTick targetTick
    feePerCube = if length affectedCubes > 0 
                 then feeAmount / toNumber (length affectedCubes)
                 else 0.0
    
    -- Redenominate affected cubes with fee distribution
    distributeFeesCube cube =
      let
        feeYield = if cube.liquidity > 0.0
                   then feePerCube / cube.liquidity
                   else 0.0
        growthFactor = 1.0 + feeYield
      in cube { liquidity = cube.liquidity * growthFactor
              , lastRedenomination = currentBlock
              }
    
    updatedCubes = map distributeFeesCube affectedCubes
    liquidityCubes' = foldl (\m c -> Map.insert c.id c m) pool.liquidityCubes updatedCubes
    
    -- Update pool state
    globalState' = pool.globalState 
      { currentTick = targetTick
      , sqrtRateX96 = newSqrtRate
      , virtualBalances = newBalances
      }
    
    pool' = pool { liquidityCubes = liquidityCubes'
                 , globalState = globalState'
                 , lastUpdateBlock = currentBlock
                 }
    
    swapResult = { amountIn: amountIn
                 , amountOut: finalAmountOut
                 , executionTick: targetTick
                 , feeAmount: feeAmount
                 , sqrtRateAfter: newSqrtRate
                 }
    
  in Tuple pool' swapResult

-- | Execute swap preserving Balancer invariant
executeSwapInvariant :: Pool -> VirtualBalances -> TickCoordinate -> Number -> Boolean -> Tuple VirtualBalances Number
executeSwapInvariant pool balances targetTick amountIn isExactInput =
  let
    -- Determine which dimensions are being swapped
    currentTick = pool.globalState.currentTick
    
    -- Identify dimension changes
    rateChange = targetTick.rateTick /= currentTick.rateTick
    durationChange = targetTick.durationTick /= currentTick.durationTick
    leverageChange = targetTick.leverageTick /= currentTick.leverageTick
    
    -- Get risk-adjusted weights for current and target positions
    currentWeights = calculateEffectiveWeights pool.weights pool.riskParams currentTick
    targetWeights = calculateEffectiveWeights pool.weights pool.riskParams targetTick
    
    -- Apply time adjustments
    currentWd = calculateTimeAdjustedWeight currentTick.durationTick pool.creationBlock pool.lastUpdateBlock currentWeights.wd
    targetWd = calculateTimeAdjustedWeight targetTick.durationTick pool.creationBlock pool.lastUpdateBlock targetWeights.wd
    
    -- Calculate current invariant
    k_current = calculateInvariant3D balances pool.weights
    
    -- Determine primary swap dimension and calculate new balances
    result = if rateChange && not durationChange && not leverageChange then
      -- Rate dimension swap
      solveRateDimensionSwap balances amountIn currentWeights targetWeights k_current isExactInput
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension swap  
      solveDurationDimensionSwap balances amountIn currentWeights targetWeights currentWd targetWd k_current isExactInput
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap
      solveLeverageDimensionSwap balances amountIn currentWeights targetWeights k_current isExactInput  
    else
      -- Multi-dimensional swap - use composite approach
      solveMultiDimensionalSwap pool balances amountIn currentTick targetTick k_current isExactInput
    
  in result

-- | Solve swap in rate dimension preserving invariant
solveRateDimensionSwap :: VirtualBalances -> Number -> DimensionWeights -> DimensionWeights -> Number -> Boolean -> Tuple VirtualBalances Number
solveRateDimensionSwap balances amountIn currentWeights targetWeights k_target isExactInput =
  if isExactInput then
    let
      newR = balances.r + amountIn
      -- Solve for amountOut maintaining invariant
      -- K = newR^wr * D^wd * L^wl = k_target
      -- Since D and L unchanged, we need to adjust virtual balances
      amountOut = calculateOutputFromInvariant balances amountIn k_target currentWeights true
    in Tuple (balances { r = newR }) amountOut
  else
    -- Exact output case
    let
      amountNeeded = calculateInputFromInvariant balances amountIn k_target currentWeights true
      newR = balances.r + amountNeeded
    in Tuple (balances { r = newR }) amountIn

-- | Solve swap in duration dimension
solveDurationDimensionSwap :: VirtualBalances -> Number -> DimensionWeights -> DimensionWeights -> Number -> Number -> Number -> Boolean -> Tuple VirtualBalances Number  
solveDurationDimensionSwap balances amountIn currentWeights targetWeights currentWd targetWd k_target isExactInput =
  let
    -- Duration swaps affect time value
    weightRatio = targetWd / currentWd
  in
    if isExactInput then
      let
        newD = balances.d + amountIn * weightRatio
        amountOut = calculateOutputFromInvariant balances amountIn k_target currentWeights false
      in Tuple (balances { d = newD }) amountOut
    else
      let
        amountNeeded = calculateInputFromInvariant balances amountIn k_target currentWeights false
        newD = balances.d + amountNeeded * weightRatio
      in Tuple (balances { d = newD }) amountIn

-- | Solve swap in leverage dimension
solveLeverageDimensionSwap :: VirtualBalances -> Number -> DimensionWeights -> DimensionWeights -> Number -> Boolean -> Tuple VirtualBalances Number
solveLeverageDimensionSwap balances amountIn currentWeights targetWeights k_target isExactInput =
  if isExactInput then
    let
      newL = balances.l + amountIn
      amountOut = calculateOutputFromInvariant balances amountIn k_target currentWeights false
    in Tuple (balances { l = newL }) amountOut
  else
    let
      amountNeeded = calculateInputFromInvariant balances amountIn k_target currentWeights false  
      newL = balances.l + amountNeeded
    in Tuple (balances { l = newL }) amountIn

-- | Solve multi-dimensional swap using path finding
solveMultiDimensionalSwap :: Pool -> VirtualBalances -> Number -> TickCoordinate -> TickCoordinate -> Number -> Boolean -> Tuple VirtualBalances Number
solveMultiDimensionalSwap pool balances amountIn fromTick toTick k_target isExactInput =
  -- For multi-hop swaps, find optimal path and execute sequentially
  -- For now, use direct composite approach
  let
    composite = calculateCompositeBalance pool toTick pool.lastUpdateBlock
    rateRatio = composite.rateComponent / balances.r
  in
    if isExactInput then
      let
        -- Distribute input across dimensions proportionally
        rateIn = amountIn * pool.weights.wr
        durationIn = amountIn * pool.weights.wd  
        leverageIn = amountIn * pool.weights.wl
        
        newBalances = { r: balances.r + rateIn
                      , d: balances.d + durationIn
                      , l: balances.l + leverageIn
                      }
        
        -- Calculate output maintaining invariant
        k_new = calculateInvariant3D newBalances pool.weights
        outputRatio = k_target / k_new
        amountOut = amountIn * outputRatio
        
      in Tuple newBalances amountOut
    else
      -- For exact output, reverse the calculation
      let
        outputRatio = amountIn / composite.total
        requiredInput = amountIn / outputRatio
        
        rateIn = requiredInput * pool.weights.wr
        durationIn = requiredInput * pool.weights.wd
        leverageIn = requiredInput * pool.weights.wl
        
        newBalances = { r: balances.r + rateIn
                      , d: balances.d + durationIn  
                      , l: balances.l + leverageIn
                      }
        
      in Tuple newBalances amountIn

-- | Calculate output amount from invariant constraint
calculateOutputFromInvariant :: VirtualBalances -> Number -> Number -> DimensionWeights -> Boolean -> Number
calculateOutputFromInvariant balances amountIn k_target weights isRateDimension =
  -- Use Newton's method to solve for output preserving invariant
  -- Simplified closed-form for single dimension swaps
  if isRateDimension then
    -- When swapping rate, solve: (R_new)^wr * D^wd * L^wl = K
    let constantPart = pow balances.d weights.wd * pow balances.l weights.wl
        requiredR = pow (k_target / constantPart) (1.0 / weights.wr)
        deltaR = balances.r - requiredR
      in abs deltaR
    else
      -- Similar for other dimensions
      let constantPart = pow balances.r weights.wr * pow balances.d weights.wd
          requiredL = pow (k_target / constantPart) (1.0 / weights.wl)
          deltaL = balances.l - requiredL  
      in abs deltaL

-- | Calculate input amount needed for desired output
calculateInputFromInvariant :: VirtualBalances -> Number -> Number -> DimensionWeights -> Boolean -> Number
calculateInputFromInvariant balances desiredOutput k_target weights isRateDimension =
  -- Reverse calculation of calculateOutputFromInvariant
  desiredOutput * 1.1  -- Add 10% buffer for now, would use precise calculation

-- | Get liquidity cubes affected by a swap
getAffectedCubes :: Pool -> TickCoordinate -> TickCoordinate -> Array LiquidityCube3D
getAffectedCubes pool fromTick toTick =
  let
    -- Check if cube overlaps with swap path in all dimensions
    isAffected cube =
      -- Rate dimension overlap
      let rateOverlap = cube.tickLower.rateTick <= max fromTick.rateTick toTick.rateTick &&
                        cube.tickUpper.rateTick >= min fromTick.rateTick toTick.rateTick
          
          -- Duration dimension overlap  
          durationOverlap = cube.tickLower.durationTick <= max fromTick.durationTick toTick.durationTick &&
                           cube.tickUpper.durationTick >= min fromTick.durationTick toTick.durationTick
          
          -- Leverage dimension overlap
          leverageOverlap = cube.tickLower.leverageTick <= max fromTick.leverageTick toTick.leverageTick &&
                           cube.tickUpper.leverageTick >= min fromTick.leverageTick toTick.leverageTick
      
      -- Cube is affected if it overlaps in all dimensions that are changing
      in if fromTick.rateTick /= toTick.rateTick then rateOverlap
         else if fromTick.durationTick /= toTick.durationTick then durationOverlap  
         else if fromTick.leverageTick /= toTick.leverageTick then leverageOverlap
         else -- Multi-dimensional swap
           rateOverlap && durationOverlap && leverageOverlap
    
  in filter isAffected (values pool.liquidityCubes)

--------------------------------------------------------------------------------
-- REDENOMINATION SYSTEM
--------------------------------------------------------------------------------

-- | Redenominate pool positions based on pool performance and yield
redenominatePoolPositions :: Pool -> BlockNumber -> Pool
redenominatePositions pool currentBlock =
  let
    -- Redenominate all liquidity cubes with risk waterfall
    redenominatedCubes = Map.map (redenominateCubeWithWaterfall pool currentBlock) pool.liquidityCubes
    
    -- Then redenominate pool positions based on their cubes
    redenominatePos pos = 
      case Map.lookup pos.liquidityCubeId redenominatedCubes of
        Nothing -> pos
        Just cube -> redenominatePoolPositionWithCube pos cube currentBlock
    
    positions' = Map.map redenominatePos pool.positions
    
    -- Update global state
    globalState' = pool.globalState { lastRedenomination = currentBlock }
    
  in pool { positions = positions'
          , liquidityCubes = redenominatedCubes
          , globalState = globalState'
          }

-- | Redenominate a single liquidity cube
redenominateCube :: Pool -> BlockNumber -> LiquidityCube3D -> LiquidityCube3D
redenominateCube pool currentBlock cube =
  let
    blocksSince = currentBlock - cube.lastRedenomination
    
    -- Calculate yield from different sources
    rateYield = pool.globalState.yieldRates.rateYield * toNumber blocksSince
    durationYield = calculateDurationYield cube blocksSince
    leverageYield = calculateLeverageYield cube blocksSince pool.globalState.yieldRates.leverageYield
    
    -- Total yield (compounded)
    totalYield = rateYield + durationYield + leverageYield
    growthFactor = pow (1.0 + totalYield) (toNumber blocksSince / blocksPerYear)
    
    -- Apply redenomination
    newLiquidity = cube.liquidity * growthFactor
    
  in cube { liquidity = newLiquidity
          , lastRedenomination = currentBlock
          }

-- | Redenominate a pool position based on its cube
redenominatePoolPositionWithCube :: PoolPosition -> LiquidityCube3D -> BlockNumber -> PoolPosition
redenominatePoolPositionWithCube pos cube currentBlock =
  let
    -- Pool position value grows proportionally with cube growth
    cubeGrowth = cube.liquidity / cube.liquidityBase
    newValue = pos.principal * cubeGrowth
    newLiquidity = pos.liquidity * cubeGrowth
    
  in pos { liquidity = newLiquidity
         , currentValue = newValue
         , lastRedenomination = currentBlock
         }

-- | Redenominate a single pool position (for use in removeLiquidity)
redenominatePoolPosition :: Pool -> PoolPosition -> BlockNumber -> PoolPosition
redenominatePoolPosition pool pos currentBlock =
  case Map.lookup pos.liquidityCubeId pool.liquidityCubes of
    Nothing -> pos
    Just cube -> 
      let redenominatedCube = redenominateCube pool currentBlock cube
      in redenominatePoolPositionWithCube pos redenominatedCube currentBlock

-- | Calculate duration-based yield using time convergence
calculateDurationYield :: LiquidityCube3D -> Int -> Number
calculateDurationYield cube blocksSince =
  case cube.tickLower.durationTick of
    1 -> -- Monthly: yield from time decay
      let
        -- Calculate progress towards maturity
        blocksIntoMonth = blocksSince `mod` blocksPerMonth
        progressToMaturity = toNumber blocksIntoMonth / toNumber blocksPerMonth
        
        -- Time-based yield: value converges to par as maturity approaches
        -- LPs earn from this convergence
        convergenceYield = progressToMaturity * monthlyConvergenceRate
        
      in convergenceYield * toNumber blocksSince / toNumber blocksPerMonth
    _ -> 0.0  -- No time value for Flash/Spot

-- | Calculate leverage-based yield
calculateLeverageYield :: LiquidityCube3D -> Int -> Number -> Number
calculateLeverageYield cube blocksSince baseYield =
  case cube.tickLower.leverageTick of
    0 -> baseYield * toNumber blocksSince  -- Senior: base yield
    1 -> baseYield * 1.5 * toNumber blocksSince  -- Junior: 50% higher yield
    _ -> 0.0

-- | Calculate total pool value from virtual balances
calculateTotalPoolValue :: Pool -> Number
calculateTotalPoolValue pool =
  let balances = pool.globalState.virtualBalances
      weights = pool.weights
  in balances.r * weights.wr + balances.d * weights.wd + balances.l * weights.wl

--------------------------------------------------------------------------------
-- TICK MATH
--------------------------------------------------------------------------------

-- | Get sqrt rate at a specific tick (Q64.96 format)
getSqrtRateAtTick :: Int -> Number
getSqrtRateAtTick tick =
  let
    tickNumber = toNumber tick
    rate = pow 1.0001 (tickNumber / 100.0)  -- 1% = 100 ticks
    sqrtRate = pow rate 0.5
  in toQ96 sqrtRate

-- | Get tick at a specific sqrt rate
getTickAtSqrtRate :: Number -> Int
getTickAtSqrtRate sqrtRateX96 =
  let
    sqrtRate = fromQ96 sqrtRateX96
    rate = sqrtRate * sqrtRate
    tick = log rate / log 1.0001 * 100.0
  in round tick

-- | Calculate amount0 for 3D liquidity
getAmount0For3DLiquidity :: Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getAmount0For3DLiquidity liquidity tickLower tickUpper sqrtRateX96Current =
  let
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    q96 = pow 2.0 96.0
    
    -- Standard concentrated liquidity formula
    amount = if sqrtRateX96Current <= sqrtRateLower
             then liquidity * (1.0 / sqrtRateLower - 1.0 / sqrtRateUpper) * q96
             else if sqrtRateX96Current < sqrtRateUpper
             then liquidity * (1.0 / sqrtRateX96Current - 1.0 / sqrtRateUpper) * q96
             else 0.0
    
    -- Apply 3D multipliers
    durationMult = getDurationMultiplier tickLower.durationTick tickUpper.durationTick
    leverageMult = getLeverageMultiplier tickLower.leverageTick tickUpper.leverageTick
    
  in amount * durationMult * leverageMult

-- | Calculate amount1 for 3D liquidity
getAmount1For3DLiquidity :: Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getAmount1For3DLiquidity liquidity tickLower tickUpper sqrtRateX96Current =
  let
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    q96 = pow 2.0 96.0
    
    -- Standard concentrated liquidity formula
    amount = if sqrtRateX96Current <= sqrtRateLower
             then 0.0
             else if sqrtRateX96Current < sqrtRateUpper
             then liquidity * (sqrtRateX96Current - sqrtRateLower) / q96
             else liquidity * (sqrtRateUpper - sqrtRateLower) / q96
    
    -- Apply 3D multipliers
    durationMult = getDurationMultiplier tickLower.durationTick tickUpper.durationTick
    leverageMult = getLeverageMultiplier tickLower.leverageTick tickUpper.leverageTick
    
  in amount * durationMult * leverageMult

-- | Calculate liquidity from amounts in 3D
getLiquidityFor3DAmounts :: Number -> Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getLiquidityFor3DAmounts amount0 amount1 tickLower tickUpper sqrtRateX96Current =
  let
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    q96 = pow 2.0 96.0
    
    -- Calculate base liquidity using concentrated liquidity formulas
    liquidity = if sqrtRateX96Current <= sqrtRateLower
                then amount0 * sqrtRateLower * sqrtRateUpper / (sqrtRateUpper - sqrtRateLower) / q96
                else if sqrtRateX96Current < sqrtRateUpper
                then min
                  (amount0 * sqrtRateX96Current * sqrtRateUpper / (sqrtRateUpper - sqrtRateX96Current) / q96)
                  (amount1 * q96 / (sqrtRateX96Current - sqrtRateLower))
                else amount1 * q96 / (sqrtRateUpper - sqrtRateLower)
    
    -- Apply dimension divisors
    durationDiv = getDurationMultiplier tickLower.durationTick tickUpper.durationTick
    leverageDiv = getLeverageMultiplier tickLower.leverageTick tickUpper.leverageTick
    
  in liquidity / (durationDiv * leverageDiv)

--------------------------------------------------------------------------------
-- BALANCER MATH
--------------------------------------------------------------------------------

-- | Calculate spot price between two dimensions using weighted formula
calculateSpotPrice3D :: VirtualBalances -> DimensionWeights -> TickCoordinate -> TickCoordinate -> Number
calculateSpotPrice3D balances weights fromTick toTick =
  let
    -- Determine which dimensions are involved in the swap
    rateChange = fromTick.rateTick /= toTick.rateTick
    durationChange = fromTick.durationTick /= toTick.durationTick
    leverageChange = fromTick.leverageTick /= toTick.leverageTick
    
  in
    -- Calculate spot price based on dimension changes
    if rateChange && not durationChange && not leverageChange then
      -- Rate dimension swap: price of rate in terms of base
      let tickDiff = toNumber (toTick.rateTick - fromTick.rateTick) / 10000.0
          basePrice = pow 1.0001 tickDiff  -- Standard tick to price conversion
      in basePrice
      
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension swap: price reflects time value
      let fromDuration = tickToDuration fromTick.durationTick
          toDuration = tickToDuration toTick.durationTick
          -- Flash = 0.8x, Monthly = 1.0x, Spot = 1.2x multiplier
          fromMult = getDurationMultiplier fromTick.durationTick fromTick.durationTick
          toMult = getDurationMultiplier toTick.durationTick toTick.durationTick
          balanceRatio = balances.d / balances.d  -- Would use actual reserves
          weightRatio = weights.wd / weights.wd
      in toMult / fromMult * balanceRatio * weightRatio
      
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap: price reflects risk premium
      let fromLeverage = if fromTick.leverageTick == 0 then 1.0 else 3.0
          toLeverage = if toTick.leverageTick == 0 then 1.0 else 3.0
          balanceRatio = balances.l / balances.l  -- Would use actual reserves
          weightRatio = weights.wl / weights.wl
      in toLeverage / fromLeverage * balanceRatio * weightRatio
      
    else
      -- Multi-dimensional: use composite pricing
      1.0  -- Would calculate path-dependent price

-- | Calculate spot price with risk adjustment
calculateSpotPrice3DWithRisk :: Pool -> VirtualBalances -> TickCoordinate -> TickCoordinate -> BlockNumber -> Number
calculateSpotPrice3DWithRisk pool balances fromTick toTick currentBlock =
  let
    -- Get risk-adjusted weights for both ticks
    fromWeights = calculateEffectiveWeights pool.weights pool.riskParams fromTick
    toWeights = calculateEffectiveWeights pool.weights pool.riskParams toTick
    
    -- Apply time adjustment for duration dimension
    fromWd = calculateTimeAdjustedWeight fromTick.durationTick pool.creationBlock currentBlock fromWeights.wd
    toWd = calculateTimeAdjustedWeight toTick.durationTick pool.creationBlock currentBlock toWeights.wd
    
    -- Determine dimension changes
    rateChange = fromTick.rateTick /= toTick.rateTick
    durationChange = fromTick.durationTick /= toTick.durationTick
    leverageChange = fromTick.leverageTick /= toTick.leverageTick
    
  in
    -- Calculate dimension-specific price with risk adjustments
    if rateChange && not durationChange && not leverageChange then
      -- Rate swap with risk-adjusted weights
      let tickDiff = toNumber (toTick.rateTick - fromTick.rateTick)
          ratePrice = getSqrtRateAtTick toTick.rateTick / getSqrtRateAtTick fromTick.rateTick
          weightAdjustment = fromWeights.wr / toWeights.wr
      in ratePrice * ratePrice * weightAdjustment  -- Squared because using sqrt rates
      
    else if durationChange && not rateChange && not leverageChange then
      -- Duration swap with time decay
      let durationPrice = if fromWd > 0.0 && toWd > 0.0 
                         then toWd / fromWd
                         else 1.0
          balanceRatio = if balances.d > 0.0 then balances.d / balances.d else 1.0
      in durationPrice * balanceRatio
      
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage swap with risk multipliers
      let fromRisk = if fromTick.leverageTick == 0 then pool.riskParams.alphaSenior else pool.riskParams.alphaJunior
          toRisk = if toTick.leverageTick == 0 then pool.riskParams.alphaSenior else pool.riskParams.alphaJunior
          leveragePrice = toRisk / fromRisk
          weightAdjustment = fromWeights.wl / toWeights.wl
      in leveragePrice * weightAdjustment
      
    else
      -- Multi-dimensional swap: use base calculation
      calculateSpotPrice3D balances pool.weights fromTick toTick

-- | Calculate AMM invariant K = R^wr * D^wd * L^wl
calculateInvariant3D :: VirtualBalances -> DimensionWeights -> Number
calculateInvariant3D balances weights =
  pow balances.r weights.wr * pow balances.d weights.wd * pow balances.l weights.wl

-- | Calculate risk-adjusted effective weights
calculateEffectiveWeights :: DimensionWeights -> RiskParameters -> TickCoordinate -> DimensionWeights
calculateEffectiveWeights baseWeights riskParams tick =
  let
    -- Apply risk multiplier based on leverage
    leverageMultiplier = case tick.leverageTick of
      0 -> riskParams.alphaSenior   -- Senior: 1x
      1 -> riskParams.alphaJunior   -- Junior: 3x
      _ -> 1.0
    
    -- Adjust leverage weight by risk multiplier
    adjustedWl = baseWeights.wl * leverageMultiplier
    totalWeight = baseWeights.wr + baseWeights.wd + adjustedWl
    
  in { wr: baseWeights.wr / totalWeight
     , wd: baseWeights.wd / totalWeight
     , wl: adjustedWl / totalWeight
     }

-- | Calculate time-adjusted weight for duration dimension
calculateTimeAdjustedWeight :: Int -> BlockNumber -> BlockNumber -> Number -> Number
calculateTimeAdjustedWeight durationTick startBlock currentBlock baseWeight =
  case durationTick of
    1 -> -- Monthly duration
      let
        blocksElapsed = currentBlock - startBlock
        timeToMaturity = max 0.0 (1.0 - toNumber blocksElapsed / toNumber blocksPerMonth)
      in baseWeight * timeToMaturity
    _ -> baseWeight  -- No time adjustment for Flash/Spot

-- | Calculate invariant with risk and time adjustments
calculateInvariant3DWithAdjustments :: Pool -> VirtualBalances -> TickCoordinate -> BlockNumber -> Number
calculateInvariant3DWithAdjustments pool balances tick currentBlock =
  let
    -- Apply risk adjustment
    riskAdjustedWeights = calculateEffectiveWeights pool.weights pool.riskParams tick
    
    -- Apply time adjustment to duration weight
    timeAdjustedWd = calculateTimeAdjustedWeight tick.durationTick pool.creationBlock currentBlock riskAdjustedWeights.wd
    
    -- Renormalize weights after time adjustment
    totalW = riskAdjustedWeights.wr + timeAdjustedWd + riskAdjustedWeights.wl
    finalWeights = { wr: riskAdjustedWeights.wr / totalW
                   , wd: timeAdjustedWd / totalW
                   , wl: riskAdjustedWeights.wl / totalW
                   }
    
  in pow balances.r finalWeights.wr * 
     pow balances.d finalWeights.wd * 
     pow balances.l finalWeights.wl

-- | Get virtual balances for a tick coordinate
getVirtualBalances :: Pool -> TickCoordinate -> VirtualBalances
getVirtualBalances pool tick =
  let
    -- Aggregate liquidity from all cubes that contain this tick
    relevantCubes = filter (cubeContainsTick tick) (values pool.liquidityCubes)
    
    -- Calculate total liquidity at this tick
    totalLiquidityAtTick = sum (map _.liquidity relevantCubes)
    
  in
    -- If no liquidity at tick, use global balances
    if totalLiquidityAtTick == 0.0 then
      pool.globalState.virtualBalances
    else
      let
        -- Calculate dimension-specific liquidity
        rateLiquidity = sum $ map (\cube -> 
          if cube.tickLower.rateTick <= tick.rateTick && 
             cube.tickUpper.rateTick >= tick.rateTick
          then cube.liquidity else 0.0) relevantCubes
          
        durationLiquidity = sum $ map (\cube ->
          if cube.tickLower.durationTick <= tick.durationTick && 
             cube.tickUpper.durationTick >= tick.durationTick
          then cube.liquidity else 0.0) relevantCubes
          
        leverageLiquidity = sum $ map (\cube ->
          if cube.tickLower.leverageTick <= tick.leverageTick && 
             cube.tickUpper.leverageTick >= tick.leverageTick
          then cube.liquidity else 0.0) relevantCubes
        
        -- Convert to virtual balances using sqrt pricing
        sqrtRate = getSqrtRateAtTick tick.rateTick
        
        -- Virtual balances proportional to liquidity and price
        virtualR = rateLiquidity * fromQ96 sqrtRate
        virtualD = durationLiquidity * getDurationMultiplier tick.durationTick tick.durationTick
        virtualL = leverageLiquidity * getLeverageMultiplier tick.leverageTick tick.leverageTick
        
      in { r: virtualR, d: virtualD, l: virtualL }

-- | Check if a cube contains a specific tick coordinate
cubeContainsTick :: TickCoordinate -> LiquidityCube3D -> Boolean
cubeContainsTick tick cube =
  cube.tickLower.rateTick <= tick.rateTick && 
  cube.tickUpper.rateTick >= tick.rateTick &&
  cube.tickLower.durationTick <= tick.durationTick && 
  cube.tickUpper.durationTick >= tick.durationTick &&
  cube.tickLower.leverageTick <= tick.leverageTick && 
  cube.tickUpper.leverageTick >= tick.leverageTick

-- | Calculate new sqrt rate after tick movement
calculateNewSqrtRate :: TickCoordinate -> TickCoordinate -> Number -> Number
calculateNewSqrtRate fromTick toTick currentSqrtRate =
  let
    -- Calculate rate change based on tick difference
    tickDiff = toTick.rateTick - fromTick.rateTick
    newRate = fromQ96 currentSqrtRate * pow 1.0001 (toNumber tickDiff / 10000.0)
  in toQ96 (pow newRate 0.5)

-- | Calculate liquidity from amount for 3D ranges
calculateLiquidityFromAmount :: Pool -> Number -> TickCoordinate -> TickCoordinate -> Number
calculateLiquidityFromAmount pool amount tickLower tickUpper =
  let
    -- Get current tick position
    currentTick = pool.globalState.currentTick
    
    -- Calculate tick ranges for each dimension
    rateInRange = currentTick.rateTick >= tickLower.rateTick && 
                  currentTick.rateTick <= tickUpper.rateTick
    durationInRange = currentTick.durationTick >= tickLower.durationTick && 
                      currentTick.durationTick <= tickUpper.durationTick
    leverageInRange = currentTick.leverageTick >= tickLower.leverageTick && 
                      currentTick.leverageTick <= tickUpper.leverageTick
    
    -- Get sqrt rates at boundaries
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    sqrtRateCurrent = pool.globalState.sqrtRateX96
    
    -- Calculate base liquidity using concentrated liquidity formula
    baseLiquidity = if rateInRange then
      -- Current tick is within rate range
      let deltaInvSqrt = 1.0 / fromQ96 sqrtRateCurrent - 1.0 / fromQ96 sqrtRateUpper
          deltaL = fromQ96 sqrtRateCurrent - fromQ96 sqrtRateLower
      in if deltaInvSqrt > 0.0 && deltaL > 0.0
         then amount / (deltaInvSqrt + deltaL)
         else amount / fromQ96 sqrtRateCurrent
    else if currentTick.rateTick < tickLower.rateTick then
      -- Current tick below range
      amount * fromQ96 sqrtRateLower * fromQ96 sqrtRateUpper / 
        (fromQ96 sqrtRateUpper - fromQ96 sqrtRateLower)
    else
      -- Current tick above range  
      amount / (fromQ96 sqrtRateUpper - fromQ96 sqrtRateLower)
    
    -- Apply dimension multipliers
    durationMult = getDurationMultiplier tickLower.durationTick tickUpper.durationTick
    leverageMult = getLeverageMultiplier tickLower.leverageTick tickUpper.leverageTick
    
    -- Adjust for dimensions not in range
    dimensionAdjustment = (if durationInRange then 1.0 else 0.8) *
                         (if leverageInRange then 1.0 else 0.9)
    
  in baseLiquidity * dimensionAdjustment / (durationMult * leverageMult)

--------------------------------------------------------------------------------
-- RISK WATERFALL AND LOSS DISTRIBUTION
--------------------------------------------------------------------------------

-- | Get total liquidity by leverage type
getTotalLiquidityByLeverage :: Pool -> Int -> Number
getTotalLiquidityByLeverage pool leverageTick =
  let
    matchingCubes = filter (\cube -> cube.tickLower.leverageTick == leverageTick) (values pool.liquidityCubes)
  in sum (map _.liquidity matchingCubes)

-- | Calculate pool performance ratio
calculatePoolPerformance :: Pool -> Number
calculatePoolPerformance pool =
  if pool.totalDeposited > 0.0
  then pool.globalState.totalLiquidity / pool.totalDeposited
  else 1.0

-- | Calculate loss distribution with risk waterfall
calculateLossDistribution :: Pool -> Number -> LossDistribution
calculateLossDistribution pool totalLoss =
  let
    -- Get total liquidity by tranche
    juniorLiquidity = getTotalLiquidityByLeverage pool 1  -- Junior
    seniorLiquidity = getTotalLiquidityByLeverage pool 0  -- Senior
    
    -- Junior absorbs losses first (up to its total liquidity)
    juniorLoss = min totalLoss juniorLiquidity
    remainingLoss = max 0.0 (totalLoss - juniorLoss)
    
    -- Senior only loses if Junior is exhausted
    seniorLoss = min remainingLoss seniorLiquidity
    
  in { totalLoss: totalLoss
     , juniorLoss: juniorLoss
     , seniorLoss: seniorLoss
     }

-- | Redenominate cube with risk waterfall
redenominateCubeWithWaterfall :: Pool -> BlockNumber -> LiquidityCube3D -> LiquidityCube3D
redenominateCubeWithWaterfall pool currentBlock cube =
  let
    -- First apply normal yield-based redenomination
    baseRedenomination = redenominateCube pool currentBlock cube
    
    -- Check pool performance to determine if losses need to be applied
    poolPerformance = calculatePoolPerformance pool
  in
    if poolPerformance < 1.0 then
      let
        -- Calculate total loss
        totalLoss = (1.0 - poolPerformance) * pool.totalDeposited
        
        -- Get loss distribution according to waterfall
        lossDistribution = calculateLossDistribution pool totalLoss
        
        -- Calculate loss factor based on leverage tier
        lossFactor = case cube.tickLower.leverageTick of
          1 -> -- Junior tranche
            let juniorTVL = getTotalLiquidityByLeverage pool 1
            in if juniorTVL > 0.0
               then max 0.0 (1.0 - lossDistribution.juniorLoss / juniorTVL)
               else 1.0
          0 -> -- Senior tranche
            let seniorTVL = getTotalLiquidityByLeverage pool 0
            in if seniorTVL > 0.0 && lossDistribution.seniorLoss > 0.0
               then max 0.0 (1.0 - lossDistribution.seniorLoss / seniorTVL)
               else 1.0
          _ -> 1.0
      
      in baseRedenomination { liquidity = baseRedenomination.liquidity * lossFactor }
    else
      baseRedenomination

--------------------------------------------------------------------------------
-- COMPOSITE BALANCE VIEWS
--------------------------------------------------------------------------------

-- | Calculate composite balance for simplified trading views
calculateCompositeBalance :: Pool -> TickCoordinate -> BlockNumber -> CompositeBalance
calculateCompositeBalance pool tick currentBlock =
  let
    balances = pool.globalState.virtualBalances
    weights = pool.weights
    
    -- Rate component (unchanged)
    rateComponent = balances.r
    
    -- Time-value component with decay
    timeToMaturity = calculateTimeToMaturity tick.durationTick pool.creationBlock currentBlock
    timeAdjustedWd = calculateTimeAdjustedWeight tick.durationTick pool.creationBlock currentBlock weights.wd
    timeValueComponent = balances.d * pow timeToMaturity timeAdjustedWd
    
    -- Leverage component with risk adjustment
    riskMultiplier = case tick.leverageTick of
      0 -> pool.riskParams.alphaSenior
      1 -> pool.riskParams.alphaJunior
      _ -> 1.0
    leverageComponent = balances.l * riskMultiplier
    
    -- Total composite value (weighted sum)
    total = timeValueComponent * weights.wd + leverageComponent * weights.wl
    
  in { rateComponent: rateComponent
     , timeValueComponent: timeValueComponent
     , leverageComponent: leverageComponent
     , total: total
     }

-- | Get simplified composite price (yield asset price in terms of rate)
getCompositePrice :: Pool -> TickCoordinate -> BlockNumber -> Number
getCompositePrice pool tick currentBlock =
  let
    composite = calculateCompositeBalance pool tick currentBlock
  in composite.total / composite.rateComponent

-- | Calculate swap using composite balance (for trader UX)
calculateSwapWithComposite :: Pool -> Number -> Boolean -> TickCoordinate -> BlockNumber -> Tuple Number Number
calculateSwapWithComposite pool amountIn isRate targetTick currentBlock =
  let
    composite = calculateCompositeBalance pool targetTick currentBlock
  in
    if isRate then
      -- Swapping rate for composite yield asset (simplified constant product)
      let amountOut = composite.total * amountIn / (composite.rateComponent + amountIn)
      in Tuple amountIn amountOut
    else
      -- Swapping composite yield asset for rate
      let amountOut = composite.rateComponent * amountIn / (composite.total + amountIn)
      in Tuple amountIn amountOut

-- | Get maturity info for a duration tick
getMaturityInfo :: Int -> BlockNumber -> BlockNumber -> MaturityInfo
getMaturityInfo durationTick startBlock currentBlock =
  case durationTick of
    1 -> -- Monthly
      let 
        maturityBlock = startBlock + toNumber blocksPerMonth
        elapsed = currentBlock - startBlock
        timeToMaturity = max 0.0 (1.0 - toNumber elapsed / toNumber blocksPerMonth)
      in { maturityBlock: maturityBlock
         , timeToMaturity: timeToMaturity
         , isMatured: currentBlock >= maturityBlock
         }
    _ -> { maturityBlock: startBlock
         , timeToMaturity: 0.0
         , isMatured: true
         }

-- | Calculate time to maturity (0 to 1 scale)
calculateTimeToMaturity :: Int -> BlockNumber -> BlockNumber -> Number
calculateTimeToMaturity durationTick startBlock currentBlock =
  let info = getMaturityInfo durationTick startBlock currentBlock
  in info.timeToMaturity

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

-- | Generate unique key for tick coordinate
coordinateKey :: TickCoordinate -> String
coordinateKey coord = 
  show coord.rateTick <> ":" <> 
  show coord.durationTick <> ":" <> 
  show coord.leverageTick

-- | Add ticks to active list
addToActiveTicks :: Array TickCoordinate -> TickCoordinate -> TickCoordinate -> Array TickCoordinate
addToActiveTicks activeTicks tickLower tickUpper =
  let
    hasLower = any (\t -> t == tickLower) activeTicks
    hasUpper = any (\t -> t == tickUpper) activeTicks
    
    withLower = if hasLower then activeTicks else tickLower : activeTicks
    withBoth = if hasUpper then withLower else tickUpper : withLower
    
  in sortBy compareCoordinates withBoth

-- | Compare tick coordinates for sorting
compareCoordinates :: TickCoordinate -> TickCoordinate -> Ordering
compareCoordinates a b =
  case compare a.rateTick b.rateTick of
    EQ -> case compare a.durationTick b.durationTick of
      EQ -> compare a.leverageTick b.leverageTick
      other -> other
    other -> other

-- | Get duration multiplier
getDurationMultiplier :: Int -> Int -> Number
getDurationMultiplier lower upper =
  let
    lowerMult = case lower of
      0 -> 0.8  -- Flash: 20% discount
      1 -> 1.0  -- Monthly: base rate  
      _ -> 1.2  -- Spot: 20% premium
    upperMult = case upper of
      0 -> 0.8
      1 -> 1.0
      _ -> 1.2
  in (lowerMult + upperMult) / 2.0

-- | Get leverage multiplier
getLeverageMultiplier :: Int -> Int -> Number
getLeverageMultiplier lower upper =
  let
    lowerMult = if lower == 0 then 1.0 else 3.0  -- Senior: 1x, Junior: 3x
    upperMult = if upper == 0 then 1.0 else 3.0
  in (lowerMult + upperMult) / 2.0

-- | Duration to tick mapping
durationToTick :: Duration -> Int
durationToTick Flash = 0
durationToTick Monthly = 1
durationToTick Spot = 2

-- | Tick to duration mapping
tickToDuration :: Int -> Duration
tickToDuration 0 = Flash
tickToDuration 1 = Monthly
tickToDuration _ = Spot

-- | Leverage to tick mapping
leverageToTick :: Leverage -> Int
leverageToTick Senior = 0
leverageToTick Junior = 1

-- | Tick to leverage mapping
tickToLeverage :: Int -> Leverage
tickToLeverage 0 = Senior
tickToLeverage _ = Junior

-- | Convert interest rate to tick (0% to 1000% APR)
rateToTick :: Number -> Int
rateToTick rate = round (rate * 100.0)  -- 1% = 100 ticks

-- | Convert tick to interest rate
tickToRate :: Int -> Number
tickToRate tick = toNumber tick / 100.0

--------------------------------------------------------------------------------
-- TICK DATA MANAGEMENT (SIMPLIFIED)
--------------------------------------------------------------------------------

-- | Tick data with liquidity information
type TickData =
  { coordinate :: TickCoordinate
  , liquidity :: Number
  , liquidityNet :: Number     -- Net liquidity change at this tick
  , liquidityGross :: Number   -- Total liquidity referencing this tick
  , initialized :: Boolean
  }

-- | Insert or update tick data
insertTick :: TickData -> Pool -> Pool
insertTick tickData pool =
  let tickKey = coordinateKey tickData.coordinate
      updatedTickData = Map.insert tickKey tickData pool.tickData
      updatedActiveTicks = if tickData.liquidity > 0.0 && not (any (\t -> t == tickData.coordinate) pool.activeTicks)
                          then sortBy compareCoordinates (tickData.coordinate : pool.activeTicks)
                          else pool.activeTicks
  in pool { tickData = updatedTickData, activeTicks = updatedActiveTicks }

-- | Remove tick data
removeTick :: TickCoordinate -> Pool -> Pool
removeTick coord pool =
  let tickKey = coordinateKey coord
      updatedTickData = Map.delete tickKey pool.tickData
      updatedActiveTicks = filter (\t -> t /= coord) pool.activeTicks
  in pool { tickData = updatedTickData, activeTicks = updatedActiveTicks }

-- | Lookup tick data
lookupTick :: TickCoordinate -> Pool -> Maybe TickData
lookupTick coord pool = Map.lookup (coordinateKey coord) pool.tickData

-- | Update tick data
updateTick :: (TickData -> TickData) -> TickCoordinate -> Pool -> Pool
updateTick updateFn coord pool =
  case lookupTick coord pool of
    Nothing -> pool
    Just tickData ->
      let newTickData = updateFn tickData
          tickKey = coordinateKey coord
          updatedTickData = Map.insert tickKey newTickData pool.tickData
          -- Update active ticks if liquidity changed
          wasActive = any (\t -> t == coord) pool.activeTicks
          isActive = newTickData.liquidity > 0.0
          updatedActiveTicks = if wasActive && not isActive
                              then filter (\t -> t /= coord) pool.activeTicks
                              else if not wasActive && isActive
                              then sortBy compareCoordinates (coord : pool.activeTicks)
                              else pool.activeTicks
      in pool { tickData = updatedTickData, activeTicks = updatedActiveTicks }

-- | Find ticks in a range (simplified)
findTicksInRange :: TickCoordinate -> TickCoordinate -> Pool -> Array TickData
findTicksInRange minCoord maxCoord pool =
  let isInRange tickData =
        let coord = tickData.coordinate
        in coord.rateTick >= minCoord.rateTick && coord.rateTick <= maxCoord.rateTick &&
           coord.durationTick >= minCoord.durationTick && coord.durationTick <= maxCoord.durationTick &&
           coord.leverageTick >= minCoord.leverageTick && coord.leverageTick <= maxCoord.leverageTick
  in filter isInRange (values pool.tickData)

-- | Get all active ticks with liquidity
getActiveTicks :: Pool -> Array TickData
getActiveTicks pool =
  mapMaybe (\coord -> lookupTick coord pool) pool.activeTicks
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
    , PoolEvent(..)
    , TickCoordinate
    , Liquidity3D
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
  , calculateSwapPrice3DWithRisk
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
  , calculateSwapPrice3D
  , calculateInvariant3D
  , getVirtualBalances
  -- Tick management
  , insertTick
  , removeTick
  , lookupTick
  , updateTick
  , findTicksInRange
  , getActiveTicks
  , durationToTick
  , leverageToTick
  -- Position types
  , Duration(..)
  , Leverage(..)
  , leverageMultiplier
  -- Helpers
  , toQ96
  , fromQ96
  ) where

import Prelude
import Data.Array (filter, sortBy, take, foldl, length, (:), any, mapMaybe, concat, concatMap, fromFoldable) as Array
import Data.Foldable (sum)
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Tuple (Tuple(..), uncurry)
import Data.Ord (min, max, abs)
import Effect (Effect)
import Data.Number (pow, log)
import Protocol.Token (TokenType)
import Protocol.Common (BlockNumber, PositionId)
import Protocol.Config (defaultProtocolConfig)

--------------------------------------------------------------------------------
-- CONSTANTS
--------------------------------------------------------------------------------

-- Time constants
blocksPerMonth :: Int  
blocksPerMonth = defaultProtocolConfig.time.blocksPerMonth

-- AMM constants
initialVirtualBalance :: Number
initialVirtualBalance = 1000.0

-- Initial yield rate constants
baseRateYield :: Number
baseRateYield = 0.0001

baseDurationYield :: Number
baseDurationYield = 0.00005

baseLeverageYield :: Number
baseLeverageYield = 0.00008

-- Swap constants
defaultSwapBuffer :: Number
defaultSwapBuffer = 1.1  -- 10% buffer for swap calculations

-- Tick math constants
tickBase :: Number
tickBase = 1.0001

ticksPerPercent :: Number
ticksPerPercent = 100.0

-- Duration multiplier constants
flashDurationMultiplier :: Number
flashDurationMultiplier = 0.8  -- 20% discount

monthlyDurationMultiplier :: Number
monthlyDurationMultiplier = 1.0  -- Base rate

swapDurationMultiplier :: Number
swapDurationMultiplier = 1.2  -- 20% premium

-- Leverage multiplier constants
seniorLeverageRiskMultiplier :: Number
seniorLeverageRiskMultiplier = 1.0

juniorLeverageRiskMultiplier :: Number
juniorLeverageRiskMultiplier = 3.0

-- Junior yield premium
juniorYieldPremium :: Number
juniorYieldPremium = 1.5  -- 50% higher yield for junior

-- Swap solver constants
swapSolverBufferRatio :: Number
swapSolverBufferRatio = 0.8

swapSolverPremiumRatio :: Number
swapSolverPremiumRatio = 0.9

--------------------------------------------------------------------------------
-- POOL POSITION TYPES
--------------------------------------------------------------------------------

-- | Duration options - discrete time commitments
data Duration
  = Flash     -- Single block loan, capital returned immediately
  | Monthly   -- 28-day term loan, capital returned at expiry
  | Swap      -- Cross the spread without return (conceptually equivalent to an indefinite loan)

derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

instance showDuration :: Show Duration where
  show Flash = "Flash"
  show Monthly = "Monthly"
  show Swap = "Swap"

-- | Leverage tiers - discrete exposure levels
data Leverage  
  = Senior    -- 1x exposure, protected
  | Junior    -- 3x exposure, higher risk/reward

derive instance eqLeverage :: Eq Leverage
derive instance ordLeverage :: Ord Leverage

instance showLeverage :: Show Leverage where
  show Senior = "Senior (1x)"
  show Junior = "Junior (3x)"

-- | Get numeric multiplier for leverage tier
leverageMultiplier :: Leverage -> Number
leverageMultiplier Senior = defaultProtocolConfig.pools.seniorLeverageMultiplier
leverageMultiplier Junior = defaultProtocolConfig.pools.juniorLeverageMultiplier

-- | Convert duration to tick
durationToTick :: Duration -> Int
durationToTick Flash = 0
durationToTick Monthly = 1
durationToTick Swap = 2

-- | Convert leverage to tick
leverageToTick :: Leverage -> Int
leverageToTick Senior = 0
leverageToTick Junior = 1

--------------------------------------------------------------------------------
-- CORE TYPES
--------------------------------------------------------------------------------

-- | 3D Pool state with weighted liquidity
type Pool =
  { id :: String
  , token :: TokenType                           -- Token being lent/borrowed
  , liquidity3Ds :: Map String Liquidity3D -- Concentrated liquidity positions (3D cubes)
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
  { rateTick :: Int       -- Rate/price tick: For Swap duration = exchange rate (0 = 1:1), For Monthly = lending APR (500 = 5%), For Flash = fee percentage
  , durationTick :: Int   -- 0 = Flash (single block), 1 = Monthly (28 days), 2 = Swap (immediate swap)
  , leverageTick :: Int   -- 0 = Senior (1x exposure), 1 = Junior (3x exposure)
  }

-- | Concentrated liquidity in 3D space (cube of rate × duration × leverage)
type Liquidity3D =
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
  , liquidity3DId :: String
  , liquidity :: Number               -- Current liquidity (redenominated)
  , principal :: Number               -- Original investment
  , currentValue :: Number            -- Current value after redenomination
  , lastRedenomination :: BlockNumber -- Last redenomination block
  }

-- | Global pool state for 3D AMM
type GlobalState3D =
  { currentTick :: TickCoordinate      -- Current position in 3D space
  , sqrtRateX96 :: Number              -- Current sqrt rate in Q64.96
  , virtualBalances :: VirtualBalances -- Current virtual balances
  , yieldRates :: YieldRates           -- Current yield rates per dimension
  , totalLiquidity :: Number           -- Total active liquidity
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
  , total :: Number               -- Combined composite value
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
minLiquidity = defaultProtocolConfig.pools.minLiquidity

-- | Blocks per year (assuming ~12 second blocks)
blocksPerYear :: Number
blocksPerYear = defaultProtocolConfig.time.blocksPerYear

-- | Monthly yield convergence rate
monthlyConvergenceRate :: Number
monthlyConvergenceRate = defaultProtocolConfig.pools.monthlyConvergenceRate

-- | Calculate liquidity growth factor
calculateGrowthFactor :: Number -> Number -> Number -> Number
calculateGrowthFactor currentLiquidity baseLiquidity minRequired =
  if baseLiquidity > 0.0 && currentLiquidity >= minRequired
  then currentLiquidity / baseLiquidity
  else 1.0

-- | Check if tick is in range (inclusive)
isTickInRange :: TickCoordinate -> TickCoordinate -> TickCoordinate -> Boolean
isTickInRange tick lower upper =
  tick.rateTick >= lower.rateTick && tick.rateTick <= upper.rateTick &&
  tick.durationTick >= lower.durationTick && tick.durationTick <= upper.durationTick &&
  tick.leverageTick >= lower.leverageTick && tick.leverageTick <= upper.leverageTick

-- | Create empty tick data
createEmptyTick :: TickCoordinate -> TickData
createEmptyTick coord =
  { coordinate: coord
  , liquidity: 0.0
  , liquidityNet: 0.0
  , liquidityGross: 0.0
  , initialized: false
  }

-- | Common error messages
positionNotFoundError :: String
positionNotFoundError = "Pool position not found"

cubeNotFoundError :: String
cubeNotFoundError = "Liquidity cube not found"

insufficientLiquidityError :: String
insufficientLiquidityError = "Insufficient liquidity"

withdrawalFailedError :: String
withdrawalFailedError = "Withdrawal failed"

-- | Check and return Either with error
requireFound :: forall a. Maybe a -> String -> Either String a
requireFound Nothing errorMsg = Left errorMsg
requireFound (Just value) _ = Right value

-- | Check condition and return Either with error
requireCondition :: Boolean -> String -> Either String Unit
requireCondition true _ = Right unit
requireCondition false errorMsg = Left errorMsg

-- | Simple fee calculation
calculateFee :: Number -> Number -> Number
calculateFee amount feeRate = amount * feeRate / 10000.0

-- | Get duration multiplier
getDurationMultiplier :: Duration -> Number
getDurationMultiplier Flash = flashDurationMultiplier
getDurationMultiplier Monthly = monthlyDurationMultiplier
getDurationMultiplier Swap = swapDurationMultiplier

-- | Get leverage risk multiplier
getLeverageRiskMultiplier :: Leverage -> Number
getLeverageRiskMultiplier Senior = seniorLeverageRiskMultiplier
getLeverageRiskMultiplier Junior = juniorLeverageRiskMultiplier

--------------------------------------------------------------------------------
-- POOL INITIALIZATION
--------------------------------------------------------------------------------

-- | Initialize a new 3D pool with weighted dimensions
initPool :: String -> TokenType -> Number -> DimensionWeights -> BlockNumber -> Pool
initPool poolId token feeRate weights currentBlock =
  { id: poolId
  , token: token
  , liquidity3Ds: Map.empty
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
  { alphaSenior: seniorLeverageRiskMultiplier
  , alphaJunior: juniorLeverageRiskMultiplier
  }

-- | Initialize global state
initGlobalState :: BlockNumber -> GlobalState3D
initGlobalState currentBlock =
  { currentTick: { rateTick: 0, durationTick: 1, leverageTick: 0 }  -- Start at 0% rate (1:1 price for Swap, 0% APR for Monthly), Monthly duration, Senior leverage
  , sqrtRateX96: toQ96 1.0  -- sqrt(1.0) = price 1.0 in Q64.96 format
  , virtualBalances: { r: initialVirtualBalance, d: initialVirtualBalance, l: initialVirtualBalance }  -- Initial balanced liquidity
  , yieldRates: { rateYield: baseRateYield, durationYield: baseDurationYield, leverageYield: baseLeverageYield }  -- Initial yield rates per block
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
    -- Create 3D liquidity position ID
    position3DId = owner <> "-" <> coordinateKey tickLower <> "-" <> coordinateKey tickUpper
    
    -- Calculate liquidity from amounts
    liquidity = getLiquidityFor3DAmounts amount0 amount1 tickLower tickUpper pool.globalState.sqrtRateX96
    
    -- Create 3D liquidity position (cube in rate × duration × leverage space)
    position3D = { id: position3DId
           , owner: owner
           , tickLower: tickLower
           , tickUpper: tickUpper
           , liquidity: liquidity
           , liquidityBase: liquidity  -- Track original liquidity
           , lastRedenomination: currentBlock
           , initialized: true
           }
    
    -- Create pool position with Int ID (using simple hash for model)
    positionId = hashString position3DId
    position = { id: positionId
               , owner: owner
               , liquidity3DId: position3DId
               , liquidity: liquidity
               , principal: amount0 + amount1
               , currentValue: amount0 + amount1
               , lastRedenomination: currentBlock
               }
    
    -- Update pool state
    updated3Ds = Map.insert position3DId position3D pool.liquidity3Ds
    updatedPositions = Map.insert positionId position pool.positions
    updatedActiveTicks = addToActiveTicks pool.activeTicks tickLower tickUpper
    updatedGlobalState = pool.globalState { totalLiquidity = pool.globalState.totalLiquidity + liquidity }
    
  in updatePoolState
       (pool { liquidity3Ds = updated3Ds
             , positions = updatedPositions
             , activeTicks = updatedActiveTicks
             , totalDeposited = pool.totalDeposited + amount0 + amount1  -- Track deposits
             })
       updatedGlobalState
       currentBlock

-- | Remove liquidity from a pool position
removeLiquidity3D :: Pool -> PositionId -> Number -> BlockNumber -> Either String Pool
removeLiquidity3D pool positionId liquidityAmount currentBlock = do
  poolPosition <- requireFound (Map.lookup positionId pool.positions) positionNotFoundError
    
  -- First redenominate the pool position
  let redenominatedPosition = redenominatePoolPosition pool poolPosition currentBlock
  
  _ <- requireCondition (redenominatedPosition.liquidity >= liquidityAmount) insufficientLiquidityError
  
  position3D <- requireFound (Map.lookup poolPosition.liquidity3DId pool.liquidity3Ds) "3D liquidity position not found"
  
  -- Redenominate the 3D position first
  let redenominated3D = redenominate3D pool currentBlock position3D
  
  let
    -- Calculate amounts to return based on redenominated liquidity
    shareof3D = liquidityAmount / redenominated3D.liquidity
    amount0 = getAmount0For3DLiquidity liquidityAmount redenominated3D.tickLower redenominated3D.tickUpper pool.globalState.sqrtRateX96
    amount1 = getAmount1For3DLiquidity liquidityAmount redenominated3D.tickLower redenominated3D.tickUpper pool.globalState.sqrtRateX96
    
    -- Update pool position
    remainingLiquidity = redenominatedPosition.liquidity - liquidityAmount
    poolPosition' = redenominatedPosition
      { liquidity = remainingLiquidity
      , currentValue = redenominatedPosition.currentValue * remainingLiquidity / redenominatedPosition.liquidity
      }
    
    -- Update 3D position
    position3D' = redenominated3D 
      { liquidity = redenominated3D.liquidity - liquidityAmount }
    
    -- Update pool state
    positions' = if remainingLiquidity > minLiquidity
                then Map.insert positionId poolPosition' pool.positions
                else Map.delete positionId pool.positions
    
    liquidity3Ds' = if position3D'.liquidity > minLiquidity
            then Map.insert position3D.id position3D' pool.liquidity3Ds
            else Map.delete position3D.id pool.liquidity3Ds
    
    globalState' = pool.globalState
      { totalLiquidity = pool.globalState.totalLiquidity - liquidityAmount }
    
    -- Update total deposited (subtract withdrawn amounts)
    totalDeposited' = pool.totalDeposited - (amount0 + amount1)
    
  pure $ pool
    { positions = positions'
    , liquidity3Ds = liquidity3Ds'
    , globalState = globalState'
    , totalDeposited = totalDeposited'
    , lastUpdateBlock = currentBlock
    }

--------------------------------------------------------------------------------
-- SWAP OPERATIONS
--------------------------------------------------------------------------------

-- | Distribute fees to affected 3D liquidity positions (cubes in rate × duration × leverage space)
distributeFees3D :: Array Liquidity3D -> Number -> BlockNumber -> Array Liquidity3D
distributeFees3D affected3DPositions feeAmount currentBlock =
  let
    feePer3D = if Array.length affected3DPositions > 0 
               then feeAmount / toNumber (Array.length affected3DPositions)
               else 0.0
    
    distributeFees3DPosition position3D =
      let
        feeYield = if position3D.liquidity > 0.0
                   then feePer3D / position3D.liquidity
                   else 0.0
        growthFactor = 1.0 + feeYield
      in position3D { liquidity = position3D.liquidity * growthFactor
                    , lastRedenomination = currentBlock
                    }
  in map distributeFees3DPosition affected3DPositions

-- | Update pool state after swap
updatePoolStateAfterSwap :: Pool -> TickCoordinate -> VirtualBalances -> Number -> Array Liquidity3D -> BlockNumber -> Pool
updatePoolStateAfterSwap pool targetTick newBalances newSqrtRate updated3Ds currentBlock =
  let
    liquidity3Ds' = Array.foldl (\m p -> Map.insert p.id p m) pool.liquidity3Ds updated3Ds
    globalState' = pool.globalState 
      { currentTick = targetTick
      , sqrtRateX96 = newSqrtRate
      , virtualBalances = newBalances
      }
  in pool { liquidity3Ds = liquidity3Ds'
          , globalState = globalState'
          , lastUpdateBlock = currentBlock
          }

-- | Create swap result
createSwapResult :: Number -> Number -> TickCoordinate -> Number -> Number -> SwapResult3D
createSwapResult amountIn amountOut executionTick feeAmount sqrtRateAfter =
  { amountIn: amountIn
  , amountOut: amountOut
  , executionTick: executionTick
  , feeAmount: feeAmount
  , sqrtRateAfter: sqrtRateAfter
  }

-- | Execute a swap through the 3D AMM with fee distribution via redenomination
-- The rate tick interpretation depends on the duration:
-- - Swap (duration=2): Direct token exchange at specified price tick
-- - Monthly (duration=1): Lending position at specified APR tick  
-- - Flash (duration=0): Single-block loan at specified fee tick
swap3D :: Pool -> TickCoordinate -> Number -> Boolean -> BlockNumber -> Tuple Pool SwapResult3D
swap3D pool targetTick amountIn isExactInput currentBlock =
  let
    -- Get current virtual balances
    currentBalances = pool.globalState.virtualBalances
    
    -- Calculate price impact using Balancer formula with risk adjustment
    swapPrice = calculateSwapPrice3DWithRisk pool currentBalances pool.globalState.currentTick targetTick currentBlock
    
    -- Calculate output amount using invariant preservation with adjustments
    k_before = calculateInvariant3DWithAdjustments pool currentBalances pool.globalState.currentTick currentBlock
    
    -- Update balances based on swap direction
    Tuple newBalances amountOut = executeSwapInvariant pool currentBalances targetTick amountIn isExactInput
    
    -- Apply fee
    feeAmount = amountIn * pool.feeRate / 10000.0
    finalAmountOut = amountOut * (1.0 - pool.feeRate / 10000.0)
    
    -- Calculate new sqrt rate
    newSqrtRate = calculateNewSqrtRate pool.globalState.currentTick targetTick pool.globalState.sqrtRateX96
    
    -- Distribute fees to affected 3D positions (cubes in the 3D space)
    affected3DPositions = getAffected3DPositions pool pool.globalState.currentTick targetTick
    updated3DPositions = distributeFees3D affected3DPositions feeAmount currentBlock
    
    -- Update pool state
    pool' = updatePoolStateAfterSwap pool targetTick newBalances newSqrtRate updated3DPositions currentBlock
    
    -- Create swap result
    swapResult = createSwapResult amountIn finalAmountOut targetTick feeAmount newSqrtRate
    
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
      solveDimensionSwap balances amountIn currentWeights targetWeights k_current isExactInput RateDimension currentWd targetWd
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension swap  
      solveDimensionSwap balances amountIn currentWeights targetWeights k_current isExactInput DurationDimension currentWd targetWd
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap
      solveDimensionSwap balances amountIn currentWeights targetWeights k_current isExactInput LeverageDimension currentWd targetWd
    else
      -- Multi-dimensional swap - use composite approach
      solveMultiDimensionalSwap pool balances amountIn currentTick targetTick k_current isExactInput
    
  in result

-- | Dimension type for unified swap solver
data SwapDimension = RateDimension | DurationDimension | LeverageDimension

-- | Unified dimension swap solver
solveDimensionSwap :: VirtualBalances -> Number -> DimensionWeights -> DimensionWeights -> Number -> Boolean -> SwapDimension -> Number -> Number -> Tuple VirtualBalances Number
solveDimensionSwap balances amountIn currentWeights targetWeights k_target isExactInput dimension currentWd targetWd =
  let
    -- Calculate weight ratio for duration swaps
    weightRatio = if currentWd > 0.0 then targetWd / currentWd else 1.0
    
    -- Determine which dimension to update and whether it's primary
    updateBalance = case dimension of
      RateDimension -> \b amt -> b { r = b.r + amt }
      DurationDimension -> \b amt -> b { d = b.d + amt * weightRatio }
      LeverageDimension -> \b amt -> b { l = b.l + amt }
    
    isPrimaryDimension = case dimension of
      RateDimension -> true
      _ -> false
  in
    if isExactInput then
      let
        amountToAdd = case dimension of
          DurationDimension -> amountIn * weightRatio
          _ -> amountIn
        newBalances = updateBalance balances amountToAdd
        amountOut = calculateOutputFromInvariant balances amountIn k_target currentWeights isPrimaryDimension
      in Tuple newBalances amountOut
    else
      let
        amountNeeded = calculateInputFromInvariant balances amountIn k_target currentWeights isPrimaryDimension
        amountToAdd = case dimension of
          DurationDimension -> amountNeeded * weightRatio
          _ -> amountNeeded
        newBalances = updateBalance balances amountToAdd
      in Tuple newBalances amountIn

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
  desiredOutput * defaultSwapBuffer  -- Add buffer for slippage protection

-- | Get 3D liquidity positions affected by a swap
getAffected3DPositions :: Pool -> TickCoordinate -> TickCoordinate -> Array Liquidity3D
getAffected3DPositions pool fromTick toTick =
  let
    -- Check if 3D position (cube) overlaps with swap path in all dimensions
    isAffected position3D =
      -- Rate dimension overlap
      let rateOverlap = position3D.tickLower.rateTick <= max fromTick.rateTick toTick.rateTick &&
                        position3D.tickUpper.rateTick >= min fromTick.rateTick toTick.rateTick
          
          -- Duration dimension overlap  
          durationOverlap = position3D.tickLower.durationTick <= max fromTick.durationTick toTick.durationTick &&
                           position3D.tickUpper.durationTick >= min fromTick.durationTick toTick.durationTick
          
          -- Leverage dimension overlap
          leverageOverlap = position3D.tickLower.leverageTick <= max fromTick.leverageTick toTick.leverageTick &&
                           position3D.tickUpper.leverageTick >= min fromTick.leverageTick toTick.leverageTick
      
      -- 3D position is affected if it overlaps in all dimensions that are changing
      in if fromTick.rateTick /= toTick.rateTick then rateOverlap
         else if fromTick.durationTick /= toTick.durationTick then durationOverlap  
         else if fromTick.leverageTick /= toTick.leverageTick then leverageOverlap
         else -- Multi-dimensional swap
           rateOverlap && durationOverlap && leverageOverlap
    
  in Array.filter isAffected (Array.fromFoldable (values pool.liquidity3Ds))

--------------------------------------------------------------------------------
-- REDENOMINATION SYSTEM
--------------------------------------------------------------------------------

-- | Redenominate pool positions based on pool performance and yield
redenominatePoolPositions :: Pool -> BlockNumber -> Pool
redenominatePoolPositions pool currentBlock =
  let
    -- Redenominate all 3D liquidity positions (cubes) with risk waterfall
    redenominated3Ds = map (redenominate3DWithWaterfall pool currentBlock) pool.liquidity3Ds
    
    -- Then redenominate pool positions based on their 3D positions
    redenominatePos pos = 
      case Map.lookup pos.liquidity3DId redenominated3Ds of
        Nothing -> pos
        Just position3D -> redenominatePoolPositionWith3D pos position3D currentBlock
    
    positions' = map redenominatePos pool.positions
    
    -- Update global state
    globalState' = pool.globalState { lastRedenomination = currentBlock }
    
  in pool { positions = positions'
          , liquidity3Ds = redenominated3Ds
          , globalState = globalState'
          }

-- | Calculate yield growth factor for redenomination
calculateYieldGrowthFactor :: Pool -> Liquidity3D -> Int -> Number
calculateYieldGrowthFactor pool position3D blocksSince =
  let
    -- Calculate yield from different sources
    rateYield = pool.globalState.yieldRates.rateYield * toNumber blocksSince
    durationYield = calculateDurationYield position3D blocksSince
    leverageYield = calculateLeverageYield position3D blocksSince pool.globalState.yieldRates.leverageYield
    
    -- Total yield (compounded)
    totalYield = rateYield + durationYield + leverageYield
  in pow (1.0 + totalYield) (toNumber blocksSince / blocksPerYear)

-- | Redenominate a single 3D liquidity position (cube in 3D space)
redenominate3D :: Pool -> BlockNumber -> Liquidity3D -> Liquidity3D
redenominate3D pool currentBlock position3D =
  let
    blocksSince = currentBlock - position3D.lastRedenomination
    growthFactor = calculateYieldGrowthFactor pool position3D blocksSince
    newLiquidity = position3D.liquidity * growthFactor
    
  in position3D { liquidity = newLiquidity
                , lastRedenomination = currentBlock
                }

-- | Redenominate a pool position based on its 3D position
redenominatePoolPositionWith3D :: PoolPosition -> Liquidity3D -> BlockNumber -> PoolPosition
redenominatePoolPositionWith3D pos position3D currentBlock =
  let
    position3DGrowth = calculateGrowthFactor position3D.liquidity position3D.liquidityBase minLiquidity
    newValue = pos.principal * position3DGrowth
    newLiquidity = pos.liquidity * position3DGrowth
    
  in pos { liquidity = newLiquidity
         , currentValue = newValue
         , lastRedenomination = currentBlock
         }

-- | Redenominate a single pool position (for use in removeLiquidity)
redenominatePoolPosition :: Pool -> PoolPosition -> BlockNumber -> PoolPosition
redenominatePoolPosition pool pos currentBlock =
  case Map.lookup pos.liquidity3DId pool.liquidity3Ds of
    Nothing -> pos
    Just position3D -> 
      let redenominated3D = redenominate3D pool currentBlock position3D
      in redenominatePoolPositionWith3D pos redenominated3D currentBlock

-- | Calculate duration-based yield using time convergence
calculateDurationYield :: Liquidity3D -> Int -> Number
calculateDurationYield position3D blocksSince =
  case position3D.tickLower.durationTick of
    1 -> -- Monthly: yield from time decay
      let
        -- Calculate progress towards maturity
        blocksIntoMonth = blocksSince `mod` blocksPerMonth
        progressToMaturity = toNumber blocksIntoMonth / toNumber blocksPerMonth
        
        -- Time-based yield: value converges to par as maturity approaches
        -- LPs earn from this convergence
        convergenceYield = progressToMaturity * monthlyConvergenceRate
        
      in convergenceYield * toNumber blocksSince / toNumber blocksPerMonth
    _ -> 0.0  -- No time value for Flash/Swap

-- | Calculate leverage-based yield
calculateLeverageYield :: Liquidity3D -> Int -> Number -> Number
calculateLeverageYield position3D blocksSince baseYield =
  case position3D.tickLower.leverageTick of
    0 -> baseYield * toNumber blocksSince  -- Senior: base yield
    1 -> baseYield * juniorYieldPremium * toNumber blocksSince  -- Junior: higher yield
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
-- For Swap: tick 0 = price 1.0, tick 100 = price ~1.01 (1% premium)
-- For Monthly: tick 500 = 5% APR lending rate
-- For Flash: tick 30 = 0.3% fee for single-block loan
getSqrtRateAtTick :: Int -> Number
getSqrtRateAtTick tick =
  let
    tickNumber = toNumber tick
    rate = pow tickBase (tickNumber / ticksPerPercent)  -- 1% = 100 ticks (applies to all durations)
    sqrtRate = pow rate 0.5
  in toQ96 sqrtRate

-- | Get tick at a specific sqrt rate
getTickAtSqrtRate :: Number -> Int
getTickAtSqrtRate sqrtRateX96 =
  let
    sqrtRate = fromQ96 sqrtRateX96
    rate = sqrtRate * sqrtRate
    tick = log rate / log tickBase * ticksPerPercent
  in round tick

-- | Token amount type for clarity
data TokenAmount = Amount0 | Amount1

-- | Get duration multiplier from tick
getDurationMultiplierFromTick :: Int -> Number
getDurationMultiplierFromTick 0 = flashDurationMultiplier
getDurationMultiplierFromTick 1 = monthlyDurationMultiplier
getDurationMultiplierFromTick 2 = swapDurationMultiplier
getDurationMultiplierFromTick _ = 1.0

-- | Get leverage multiplier from tick
getLeverageMultiplierFromTick :: Int -> Number
getLeverageMultiplierFromTick 0 = seniorLeverageRiskMultiplier
getLeverageMultiplierFromTick 1 = juniorLeverageRiskMultiplier  
getLeverageMultiplierFromTick _ = 1.0

-- | Calculate token amount for 3D liquidity (unified)
getAmountFor3DLiquidity :: TokenAmount -> Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getAmountFor3DLiquidity tokenAmount liquidity tickLower tickUpper sqrtRateX96Current =
  let
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    q96 = pow 2.0 96.0
    
    -- Calculate base amount using concentrated liquidity formula
    baseAmount = case tokenAmount of
      Amount0 ->
        if sqrtRateX96Current <= sqrtRateLower
        then liquidity * (1.0 / sqrtRateLower - 1.0 / sqrtRateUpper) * q96
        else if sqrtRateX96Current < sqrtRateUpper
        then liquidity * (1.0 / sqrtRateX96Current - 1.0 / sqrtRateUpper) * q96
        else 0.0
      Amount1 ->
        if sqrtRateX96Current <= sqrtRateLower
        then 0.0
        else if sqrtRateX96Current < sqrtRateUpper
        then liquidity * (sqrtRateX96Current - sqrtRateLower) / q96
        else liquidity * (sqrtRateUpper - sqrtRateLower) / q96
    
    -- Apply 3D multipliers (use average of lower and upper ticks)
    durationMult = (getDurationMultiplierFromTick tickLower.durationTick + getDurationMultiplierFromTick tickUpper.durationTick) / 2.0
    leverageMult = (getLeverageMultiplierFromTick tickLower.leverageTick + getLeverageMultiplierFromTick tickUpper.leverageTick) / 2.0
    
  in baseAmount * durationMult * leverageMult

-- | Calculate amount0 for 3D liquidity
getAmount0For3DLiquidity :: Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getAmount0For3DLiquidity = getAmountFor3DLiquidity Amount0

-- | Calculate amount1 for 3D liquidity
getAmount1For3DLiquidity :: Number -> TickCoordinate -> TickCoordinate -> Number -> Number
getAmount1For3DLiquidity = getAmountFor3DLiquidity Amount1

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
    
    -- Apply dimension divisors (use average of lower and upper ticks)
    durationDiv = (getDurationMultiplierFromTick tickLower.durationTick + getDurationMultiplierFromTick tickUpper.durationTick) / 2.0
    leverageDiv = (getLeverageMultiplierFromTick tickLower.leverageTick + getLeverageMultiplierFromTick tickUpper.leverageTick) / 2.0
    
  in liquidity / (durationDiv * leverageDiv)

--------------------------------------------------------------------------------
-- BALANCER MATH
--------------------------------------------------------------------------------

-- | Calculate swap price between two dimensions using weighted formula
calculateSwapPrice3D :: VirtualBalances -> DimensionWeights -> TickCoordinate -> TickCoordinate -> Number
calculateSwapPrice3D balances weights fromTick toTick =
  let
    -- Determine which dimensions are involved in the swap
    rateChange = fromTick.rateTick /= toTick.rateTick
    durationChange = fromTick.durationTick /= toTick.durationTick
    leverageChange = fromTick.leverageTick /= toTick.leverageTick
    
  in
    -- Calculate swap price based on dimension changes
    if rateChange && not durationChange && not leverageChange then
      -- Rate dimension swap: meaning depends on duration
      -- Swap: exchange rate between tokens (tick 100 = 1.01 price)
      -- Monthly: lending rate differential (tick 500 = 5% APR)
      -- Flash: fee percentage change (tick 30 = 0.3% fee)
      let tickDiff = toNumber (toTick.rateTick - fromTick.rateTick) / 10000.0
          basePrice = pow 1.0001 tickDiff  -- Standard tick to price conversion
      in basePrice
      
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension swap: price reflects time value
      let fromDuration = tickToDuration fromTick.durationTick
          toDuration = tickToDuration toTick.durationTick
          -- Flash = 0.8x, Monthly = 1.0x, Swap = 1.2x multiplier
          fromMult = getDurationMultiplierFromTick fromTick.durationTick
          toMult = getDurationMultiplierFromTick toTick.durationTick
          balanceRatio = balances.d / balances.d  -- Would use actual reserves
          weightRatio = weights.wd / weights.wd
      in toMult / fromMult * balanceRatio * weightRatio
      
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap: price reflects risk premium
      let fromLeverage = getLeverageMultiplierFromTick fromTick.leverageTick
          toLeverage = getLeverageMultiplierFromTick toTick.leverageTick
          balanceRatio = balances.l / balances.l  -- Would use actual reserves
          weightRatio = weights.wl / weights.wl
      in toLeverage / fromLeverage * balanceRatio * weightRatio
      
    else
      -- Multi-dimensional: use composite pricing
      1.0  -- Would calculate path-dependent price

-- | Calculate swap price with risk adjustment
calculateSwapPrice3DWithRisk :: Pool -> VirtualBalances -> TickCoordinate -> TickCoordinate -> BlockNumber -> Number
calculateSwapPrice3DWithRisk pool balances fromTick toTick currentBlock =
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
      calculateSwapPrice3D balances pool.weights fromTick toTick

-- | Calculate AMM invariant K = R^wr * D^wd * L^wl
calculateInvariant3D :: VirtualBalances -> DimensionWeights -> Number
calculateInvariant3D balances weights =
  pow balances.r weights.wr * pow balances.d weights.wd * pow balances.l weights.wl

-- | Calculate risk-adjusted effective weights
calculateEffectiveWeights :: DimensionWeights -> RiskParameters -> TickCoordinate -> DimensionWeights
calculateEffectiveWeights baseWeights riskParams tick =
  let
    -- Apply risk multiplier based on leverage
    leverageMultiplier = if tick.leverageTick == 0 
                        then riskParams.alphaSenior
                        else riskParams.alphaJunior
    
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
    _ -> baseWeight  -- No time adjustment for Flash/Swap

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

-- | Calculate dimension-specific liquidity
calculateDimensionLiquidity :: (Liquidity3D -> Boolean) -> Array Liquidity3D -> Number
calculateDimensionLiquidity predicate positions3D = sum $ map (\pos3D -> if predicate pos3D then pos3D.liquidity else 0.0) positions3D

-- | Convert liquidity to virtual balance for a dimension
liquidityToVirtualBalance :: Number -> Int -> Int -> Number
liquidityToVirtualBalance liquidity tickValue tickType =
  case tickType of
    0 -> liquidity * fromQ96 (getSqrtRateAtTick tickValue)  -- Rate dimension
    1 -> liquidity * getDurationMultiplierFromTick tickValue  -- Duration dimension
    _ -> liquidity * getLeverageMultiplierFromTick tickValue  -- Leverage dimension

-- | Get virtual balances for a tick coordinate
getVirtualBalances :: Pool -> TickCoordinate -> VirtualBalances
getVirtualBalances pool tick =
  let
    -- Aggregate liquidity from all 3D positions (cubes) that contain this tick
    relevant3DPositions = Array.filter (position3DContainsTick tick) (Array.fromFoldable (values pool.liquidity3Ds))
    
    -- Calculate total liquidity at this tick
    totalLiquidityAtTick = sum (map _.liquidity relevant3DPositions)
    
  in
    -- If no liquidity at tick, use global balances
    if totalLiquidityAtTick == 0.0 then
      pool.globalState.virtualBalances
    else
      let
        -- Calculate dimension-specific liquidity using helper
        rateLiquidity = calculateDimensionLiquidity 
          (\pos3D -> pos3D.tickLower.rateTick <= tick.rateTick && pos3D.tickUpper.rateTick >= tick.rateTick) 
          relevant3DPositions
          
        durationLiquidity = calculateDimensionLiquidity
          (\pos3D -> pos3D.tickLower.durationTick <= tick.durationTick && pos3D.tickUpper.durationTick >= tick.durationTick)
          relevant3DPositions
          
        leverageLiquidity = calculateDimensionLiquidity
          (\pos3D -> pos3D.tickLower.leverageTick <= tick.leverageTick && pos3D.tickUpper.leverageTick >= tick.leverageTick)
          relevant3DPositions
        
        -- Convert to virtual balances
        virtualR = liquidityToVirtualBalance rateLiquidity tick.rateTick 0
        virtualD = liquidityToVirtualBalance durationLiquidity tick.durationTick 1
        virtualL = liquidityToVirtualBalance leverageLiquidity tick.leverageTick 2
        
      in { r: virtualR, d: virtualD, l: virtualL }

-- | Check if a 3D position (cube) contains a specific tick coordinate
position3DContainsTick :: TickCoordinate -> Liquidity3D -> Boolean
position3DContainsTick tick position3D = isTickInRange tick position3D.tickLower position3D.tickUpper

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
    
    -- Apply dimension multipliers (use average of lower and upper ticks)
    durationMult = (getDurationMultiplierFromTick tickLower.durationTick + getDurationMultiplierFromTick tickUpper.durationTick) / 2.0
    leverageMult = (getLeverageMultiplierFromTick tickLower.leverageTick + getLeverageMultiplierFromTick tickUpper.leverageTick) / 2.0
    
    -- Adjust for dimensions not in range
    dimensionAdjustment = (if durationInRange then 1.0 else swapSolverBufferRatio) *
                         (if leverageInRange then 1.0 else swapSolverPremiumRatio)
    
  in baseLiquidity * dimensionAdjustment / (durationMult * leverageMult)

--------------------------------------------------------------------------------
-- RISK WATERFALL AND LOSS DISTRIBUTION
--------------------------------------------------------------------------------

-- | Get total liquidity by leverage type
getTotalLiquidityByLeverage :: Pool -> Int -> Number
getTotalLiquidityByLeverage pool leverageTick =
  let
    matching3DPositions = Array.filter (\pos3D -> pos3D.tickLower.leverageTick == leverageTick) (Array.fromFoldable (values pool.liquidity3Ds))
  in sum (map _.liquidity matching3DPositions)

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

-- | Redenominate 3D liquidity position with risk waterfall
redenominate3DWithWaterfall :: Pool -> BlockNumber -> Liquidity3D -> Liquidity3D
redenominate3DWithWaterfall pool currentBlock position3D =
  let
    -- First apply normal yield-based redenomination
    baseRedenomination = redenominate3D pool currentBlock position3D
    
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
        lossFactor = case position3D.tickLower.leverageTick of
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
        maturityBlock = startBlock + blocksPerMonth
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
-- POOL EVENTS
--------------------------------------------------------------------------------

-- | Events emitted by pool operations
data PoolEvent
  = SwapExecuted
    { pool :: String
    , amountIn :: Number
    , amountOut :: Number
    , sqrtPrice :: Number
    , tick :: Int
    }
  | LiquidityChanged
    { pool :: String
    , liquidityDelta :: Number
    }
  | POLDeployed
    { pool :: String
    , triggerPrice :: Number
    , liquidityDeployed :: Number
    , amount :: Number  -- Total amount deployed
    }

derive instance eqPoolEvent :: Eq PoolEvent

--------------------------------------------------------------------------------
-- HELPER FUNCTIONS
--------------------------------------------------------------------------------

-- | Simple string hash for generating position IDs (model implementation)
hashString :: String -> Int
hashString str = 
  -- Simple hash based on string length and first few characters
  let len = Array.length (Array.fromFoldable str)
      charSum = Array.foldl (\acc c -> acc + fromEnum c) 0 (Array.take 5 $ Array.fromFoldable str)
  in (len * 31 + charSum) `mod` 2147483647

-- | Create tick coordinate
createTickCoordinate :: Int -> Int -> Int -> TickCoordinate
createTickCoordinate rateTick durationTick leverageTick =
  { rateTick, durationTick, leverageTick }

-- | Update pool state helper
updatePoolState :: forall r. 
  { globalState :: GlobalState3D
  , lastUpdateBlock :: BlockNumber 
  | r } -> 
  GlobalState3D -> 
  BlockNumber -> 
  { globalState :: GlobalState3D
  , lastUpdateBlock :: BlockNumber 
  | r }
updatePoolState pool newGlobalState newBlock =
  pool { globalState = newGlobalState
       , lastUpdateBlock = newBlock
       }

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
    hasLower = Array.any (\t -> t == tickLower) activeTicks
    hasUpper = Array.any (\t -> t == tickUpper) activeTicks
    
    withLower = if hasLower then activeTicks else tickLower Array.: activeTicks
    withBoth = if hasUpper then withLower else tickUpper Array.: withLower
    
  in Array.sortBy compareCoordinates withBoth

-- | Compare tick coordinates for sorting
compareCoordinates :: TickCoordinate -> TickCoordinate -> Ordering
compareCoordinates a b =
  case compare a.rateTick b.rateTick of
    EQ -> case compare a.durationTick b.durationTick of
      EQ -> compare a.leverageTick b.leverageTick
      other -> other
    other -> other

-- | Tick to duration mapping
tickToDuration :: Int -> Duration
tickToDuration 0 = Flash
tickToDuration 1 = Monthly
tickToDuration _ = Swap

-- | Tick to leverage mapping
tickToLeverage :: Int -> Leverage
tickToLeverage 0 = Senior
tickToLeverage _ = Junior

-- | Convert rate/price to tick
-- For Swap: price 1.05 → tick 500 (5% premium)
-- For Monthly: 5% APR → tick 500
-- For Flash: 0.3% fee → tick 30
rateToTick :: Number -> Int
rateToTick rate = round (rate * ticksPerPercent)

-- | Convert tick to rate/price
-- Interpretation depends on duration context
tickToRate :: Int -> Number
tickToRate tick = toNumber tick / ticksPerPercent

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
      updatedActiveTicks = if tickData.liquidity > 0.0 && not (Array.any (\t -> t == tickData.coordinate) pool.activeTicks)
                          then Array.sortBy compareCoordinates (tickData.coordinate Array.: pool.activeTicks)
                          else pool.activeTicks
  in pool { tickData = updatedTickData, activeTicks = updatedActiveTicks }

-- | Remove tick data
removeTick :: TickCoordinate -> Pool -> Pool
removeTick coord pool =
  let tickKey = coordinateKey coord
      updatedTickData = Map.delete tickKey pool.tickData
      updatedActiveTicks = Array.filter (\t -> t /= coord) pool.activeTicks
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
      in insertTick newTickData pool

-- | Find ticks in a range (simplified)
findTicksInRange :: TickCoordinate -> TickCoordinate -> Pool -> Array TickData
findTicksInRange minCoord maxCoord pool =
  let isInRange tickData = isTickInRange tickData.coordinate minCoord maxCoord
  in Array.filter isInRange (Array.fromFoldable (values pool.tickData))

-- | Get all active ticks with liquidity
getActiveTicks :: Pool -> Array TickData
getActiveTicks pool =
  Array.mapMaybe (\coord -> lookupTick coord pool) pool.activeTicks
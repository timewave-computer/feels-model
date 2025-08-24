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
-- |
-- | The pool serves as the core liquidity engine for the Feels Protocol,
-- | enabling multi-dimensional markets where users can trade between
-- | interest rates, time commitments, and leverage exposure simultaneously
module Protocol.Pool
  ( -- Core types
    Pool
    , PoolEvent(..)
    , TickCoordinate
    , TickDirection(..)
    , Liquidity3D
    , PoolPosition
    , PoolState3D
    , VirtualBalances
    , DimensionWeights
    , SwapResult3D
    , YieldRates
    , RiskParameters
    , ProtectionCurve(..)
    , RiskProfile
    , CompositeBalance
    , LossDistribution
    , MaturityInfo
    , TickData
    -- Pool operations
  , initPool
  , addLiquidity3D
  , removeLiquidity3D
  , swap3D
  , rebalancePool
  -- Position NFT management
  , mintPositionNFT
  , transferPositionNFT
  , burnPositionNFT
  , lockPosition
  -- Redenomination
  , redenominatePoolPositions
  , redenominatePoolPosition
  -- Risk and adjustments
  , calculateEffectiveWeights
  , calculateTimeAdjustedWeight
  , calculateInvariant3DWithAdjustments
  , calculateSwapPrice3DWithRisk
  , calculateLossDistribution
  , calculateProtection
  , createRiskProfile
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
import Data.Array (filter, sortBy, take, length, (:), any, mapMaybe, concat, concatMap, fromFoldable, elem, nub, range, head, reverse, tail) as Array
import Data.Foldable (sum, traverse_, foldl)
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Data.Int (round, toNumber)
import Data.Tuple (Tuple(..), uncurry)
import Data.Ord (min, max, abs)
import Effect (Effect)
import Effect.Ref (Ref)
import Data.Number (pow, log)
import Data.Char (fromCharCode, toCharCode)
import Data.Enum (fromEnum)
import Data.String.CodeUnits (toCharArray)
import Data.String as String
import Data.String.CodePoints (length) as StringCP
import Protocol.Token (TokenType)
import Protocol.Common (BlockNumber, PositionId, EventEmitter, createEventEmitter, emit)
import Protocol.Config (defaultProtocolConfig, PoolParameters)
import Protocol.Oracle (Oracle, getCurrentPrice)

-- All constants in Protocol.Config

--------------------------------------------------------------------------------
-- POOL POSITION TYPES
--------------------------------------------------------------------------------

-- | Duration options - represents time commitment for liquidity
-- | Based on the fixed duration types from the 3D protocol specification
data Duration
  = Flash      -- 1 block duration
  | Swap       -- Immediate (spot)
  | Weekly     -- 7 days  
  | Monthly    -- 28 days
  | Quarterly  -- 90 days
  | Annual     -- 365 days

derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

instance showDuration :: Show Duration where
  show Flash = "Flash"
  show Swap = "Swap"
  show Weekly = "Weekly"
  show Monthly = "Monthly"
  show Quarterly = "Quarterly"
  show Annual = "Annual"

-- | Leverage represents continuous scaling from 1.0x to pool-specific maximums
-- | The actual leverage value is stored separately as a Number
-- | This type is used for categorization and backwards compatibility
data Leverage  
  = Leverage Number  -- Continuous leverage value (1.0 to max)

instance eqLeverage :: Eq Leverage where
  eq (Leverage a) (Leverage b) = a == b

instance ordLeverage :: Ord Leverage where
  compare (Leverage a) (Leverage b) = compare a b

instance showLeverage :: Show Leverage where
  show (Leverage x) = "Leverage " <> show x <> "x"

-- | Get numeric multiplier for leverage - now just returns the contained value
leverageMultiplier :: Leverage -> Number
leverageMultiplier (Leverage x) = x

-- | Convert duration to tick - maps time commitment to discrete tick space
-- | Based on Duration enum from spec: Flash=0, Swap=1, Weekly=2, Monthly=3, Quarterly=4, Annual=5
durationToTick :: Duration -> Int
durationToTick Flash = 0
durationToTick Swap = 1
durationToTick Weekly = 2
durationToTick Monthly = 3
durationToTick Quarterly = 4
durationToTick Annual = 5

-- | Convert leverage to tick - maps continuous leverage to discrete tick space
-- | Uses 6 bits for 64 discrete levels as per spec
leverageToTick :: Leverage -> Int
leverageToTick (Leverage x) = round ((x - 1.0) * 10.0)  -- Maps 1.0-6.4x to 0-63

--------------------------------------------------------------------------------
-- CORE TYPES
--------------------------------------------------------------------------------

-- | 3D Pool state with weighted liquidity
-- | Pool events for monitoring and indexing - enables UI updates and analytics
data PoolEvent
  = TickCrossed { tick :: TickCoordinate, direction :: TickDirection }  -- Price moved across tick boundary
  | LiquidityAdded { positionId :: String, amount :: Number }           -- LP added liquidity
  | LiquidityRemoved { positionId :: String, amount :: Number }         -- LP removed liquidity
  | SwapExecuted { pool :: String, amountIn :: Number, amountOut :: Number, fee :: Number, sqrtPrice :: Number }  -- Trade executed
  | FeesCollected { amount :: Number, recipients :: Int }               -- Fees distributed to LPs
  | PoolCreated { poolId :: String, token :: TokenType }                -- New pool deployed
  | OracleUpdated { oldPrice :: Number, newPrice :: Number }            -- Oracle price changed
  | PoolRebalanced { oldBalances :: VirtualBalances, newBalances :: VirtualBalances, deviation :: Number }  -- AMM rebalanced
  | PositionNFTMinted { positionId :: PositionId, nftId :: Int, owner :: String }  -- Position tokenized
  | PositionTransferred { positionId :: PositionId, from :: String, to :: String, nftId :: Maybe Int }  -- Position ownership changed
  | PositionNFTBurned { positionId :: PositionId, nftId :: Int }        -- Position NFT destroyed
  | POLDeployed { pool :: String, amount :: Number }                    -- Protocol-owned liquidity added
  | LiquidityChanged { pool :: String, liquidityDelta :: Number }       -- Total liquidity changed

-- | 3D Pool - the core liquidity structure managing multi-dimensional markets
type Pool =
  { id :: String
  , token :: TokenType                           -- Base asset for all positions
  , liquidity3Ds :: Map String Liquidity3D       -- 3D liquidity "cubes" in tick space
  , activeTicks :: Array TickCoordinate          -- Ticks with active liquidity for routing
  , tickData :: Map String TickData              -- Liquidity depth at each tick
  , positions :: Map PositionId PoolPosition     -- User positions with redenomination tracking
  , poolState :: PoolState3D                     -- Current AMM state and invariant
  , feeRate :: Number                            -- Swap fee in basis points
  , weights :: DimensionWeights                  -- Dimension importance in pricing
  , riskParams :: RiskParameters                 -- Continuous leverage risk parameters
  , creationBlock :: BlockNumber                 -- Genesis block for time calculations
  , totalDeposited :: Number                     -- Tracks deposits for loss socialization
  , lastUpdateBlock :: BlockNumber               -- Last state transition
  , oracle :: Oracle                             -- External price reference
  , eventEmitter :: EventEmitter PoolEvent       -- Event broadcasting system
  }

-- | Coordinate in 3D tick space - uniquely identifies a point in the market
type TickCoordinate =
  { rateTick :: Int       -- Price dimension: swap rate, lending APR, or flash fee
  , durationTick :: Int   -- Time dimension: 0=Flash, 1=Monthly, 2=Swap
  , leverageTick :: Int   -- Risk dimension: continuous leverage encoded as tick
  }

-- | Concentrated liquidity in 3D space - defines a liquidity "cube" in the market
type Liquidity3D =
  { id :: String
  , owner :: String
  , tickLower :: TickCoordinate        -- Lower corner of liquidity cube
  , tickUpper :: TickCoordinate        -- Upper corner of liquidity cube
  , liquidity :: Number                -- Current liquidity after yield accrual
  , liquidityBase :: Number            -- Initial liquidity for growth tracking
  , lastRedenomination :: BlockNumber  -- When position last earned yield
  , initialized :: Boolean             -- Whether position is active
  }

-- | User's liquidity position - tracks ownership and yield accumulation
type PoolPosition =
  { id :: PositionId
  , owner :: String
  , liquidity3DId :: String
  , liquidity :: Number               -- Current shares after compounding
  , principal :: Number               -- Initial deposit amount
  , currentValue :: Number            -- Market value including gains/losses
  , lastRedenomination :: BlockNumber -- Last yield calculation block
  , nftId :: Maybe Int                -- NFT for position transferability
  , transferable :: Boolean           -- Lock status for composability
  }

-- | Pool state for 3D AMM - tracks global market state and invariant
type PoolState3D =
  { currentTick :: TickCoordinate      -- Current market price point
  , sqrtRateX96 :: Number              -- Square root price for gas efficiency
  , virtualBalances :: VirtualBalances -- Virtual reserves for each dimension
  , yieldRates :: YieldRates           -- Dimension-specific yield rates
  , totalLiquidity :: Number           -- Sum of all active liquidity
  , lastRedenomination :: BlockNumber  -- Global yield distribution block
  }

-- | Virtual balances for AMM invariant - drives pricing across dimensions
type VirtualBalances =
  { r :: Number  -- Rate dimension reserve
  , d :: Number  -- Duration dimension reserve
  , l :: Number  -- Leverage dimension reserve
  }

-- | Dimension weights for AMM invariant - controls price sensitivity
type DimensionWeights =
  { wr :: Number  -- Rate weight (price impact)
  , wd :: Number  -- Duration weight (time value)  
  , wl :: Number  -- Leverage weight (risk premium)
  }

-- | Yield rates per dimension - determines position growth rates
type YieldRates =
  { rateYield :: Number      -- APR from lending/borrowing spreads
  , durationYield :: Number  -- Time decay towards maturity
  , leverageYield :: Number  -- Risk premium for junior tranches
  }

-- | Result of a swap through 3D space - complete trade execution details
type SwapResult3D =
  { amountIn :: Number               -- Input amount in base token
  , amountOut :: Number              -- Output amount after fees
  , executionTick :: TickCoordinate  -- Final tick after swap
  , feeAmount :: Number              -- Fees paid to LPs
  , sqrtRateAfter :: Number          -- New pool sqrt price
  }

-- | Risk parameters for leverage - controls protection curves and ceilings
type RiskParameters =
  { maxLeverage :: Number           -- Pool-specific maximum leverage
  , currentCeiling :: Number        -- Dynamic leverage ceiling based on conditions
  , protectionCurve :: ProtectionCurve  -- How protection scales with leverage
  , lastCeilingUpdate :: BlockNumber    -- When ceiling was last adjusted
  }

-- | Protection curve types for continuous leverage
data ProtectionCurve
  = Linear                          -- protection = 1 - (leverage - 1) / (max - 1)
  | Exponential { decayRate :: Number }  -- protection = e^(-k * (leverage - 1))
  | Piecewise { points :: Array (Tuple Number Number) }  -- Custom breakpoints

derive instance eqProtectionCurve :: Eq ProtectionCurve

instance showProtectionCurve :: Show ProtectionCurve where
  show Linear = "Linear"
  show (Exponential { decayRate }) = "Exponential(k=" <> show decayRate <> ")"
  show (Piecewise { points }) = "Piecewise(" <> show (Array.length points) <> " points)"

-- | Risk profile for a specific leverage level
type RiskProfile =
  { leverage :: Number              -- Input leverage (1.0 to max)
  , protectionFactor :: Number      -- Derived from curve (0.0 to 1.0)
  , feeMultiplier :: Number         -- Fee scaling (sqrt of leverage)
  , maxLossPercentage :: Number     -- Maximum loss during redenomination
  , requiredMarginRatio :: Number   -- Margin requirement
  }

-- | Composite balance view - aggregates 3D position into single value
type CompositeBalance = 
  { rateComponent :: Number        -- Interest rate value contribution
  , timeValueComponent :: Number   -- Time commitment value
  , leverageComponent :: Number    -- Risk premium value
  , total :: Number                -- Sum of all components
  }

-- | Loss distribution for continuous leverage model
type LossDistribution =
  { totalLoss :: Number              -- Total protocol loss
  , distributionByLeverage :: Map Number Number  -- Loss per leverage level
  }

-- | Maturity information for term positions - tracks time decay
type MaturityInfo =
  { maturityBlock :: BlockNumber  -- When position matures
  , timeToMaturity :: Number      -- Progress to maturity (0-1)
  , isMatured :: Boolean          -- Ready for settlement
  }

--------------------------------------------------------------------------------
-- CONSTANTS AND UTILITIES
--------------------------------------------------------------------------------

-- | Q96 fixed-point conversion - matches Uniswap V3 precision
toQ96 :: Number -> Number
toQ96 x = x * pow 2.0 96.0

-- | Convert from Q96 fixed-point to floating point
fromQ96 :: Number -> Number  
fromQ96 x = x / pow 2.0 96.0

-- All utility constants now use config parameters

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
getDurationMultiplier :: PoolParameters -> Duration -> Number
getDurationMultiplier poolConfig Flash = poolConfig.flashDurationMultiplier
getDurationMultiplier poolConfig Swap = poolConfig.swapDurationMultiplier
getDurationMultiplier poolConfig Weekly = poolConfig.monthlyDurationMultiplier * 0.25  -- Approximation
getDurationMultiplier poolConfig Monthly = poolConfig.monthlyDurationMultiplier
getDurationMultiplier poolConfig Quarterly = poolConfig.monthlyDurationMultiplier * 3.0
getDurationMultiplier poolConfig Annual = poolConfig.monthlyDurationMultiplier * 12.0

-- | Get leverage risk multiplier - now based on continuous leverage
getLeverageRiskMultiplier :: PoolParameters -> Leverage -> Number
getLeverageRiskMultiplier poolConfig (Leverage x) = x

-- | Calculate protection factor based on leverage and curve
calculateProtection :: Number -> ProtectionCurve -> Number
calculateProtection leverage Linear = 
  max 0.0 (1.0 - (leverage - 1.0) / 5.0)  -- Assuming max leverage of 6.0
calculateProtection leverage (Exponential { decayRate }) = 
  pow 2.71828 (-decayRate * (leverage - 1.0))
calculateProtection leverage (Piecewise { points }) =
  -- Find the appropriate segment and interpolate
  fromMaybe 0.0 $ interpolatePoints leverage points
  where
    interpolatePoints :: Number -> Array (Tuple Number Number) -> Maybe Number
    interpolatePoints _ [] = Nothing
    interpolatePoints lev pts = 
      -- Simple linear interpolation between points
      case Array.filter (\(Tuple l _) -> l <= lev) pts of
        [] -> map (\(Tuple _ p) -> p) (Array.head pts)
        filtered -> 
          case Array.head (Array.reverse filtered) of
            Nothing -> Nothing
            Just (Tuple l1 p1) ->
              case Array.head (Array.filter (\(Tuple l _) -> l > lev) pts) of
                Nothing -> Just p1
                Just (Tuple l2 p2) -> 
                  Just $ p1 + (p2 - p1) * (lev - l1) / (l2 - l1)

-- | Create risk profile for a given leverage
createRiskProfile :: Number -> RiskParameters -> RiskProfile
createRiskProfile leverage params =
  let protection = calculateProtection leverage params.protectionCurve
      feeMultiplier = pow leverage 0.5  -- Square root scaling
      maxLoss = 1.0 - protection
      marginRatio = 1.0 / leverage + 0.1  -- 10% buffer
  in { leverage: leverage
     , protectionFactor: protection
     , feeMultiplier: feeMultiplier
     , maxLossPercentage: maxLoss
     , requiredMarginRatio: marginRatio
     }

--------------------------------------------------------------------------------
-- POOL INITIALIZATION
--------------------------------------------------------------------------------

-- | Initialize a new 3D pool - creates empty market with initial parameters
initPool :: String -> TokenType -> Number -> DimensionWeights -> PoolParameters -> BlockNumber -> Oracle -> Effect Pool
initPool poolId token feeRate weights poolConfig currentBlock oracle = do
  eventEmitter <- createEventEmitter
  pure
    { id: poolId
    , token: token
    , liquidity3Ds: Map.empty
    , activeTicks: []
    , tickData: Map.empty
    , positions: Map.empty
    , poolState: initPoolState poolConfig currentBlock
    , feeRate: feeRate
    , weights: weights
    , riskParams: defaultRiskParameters poolConfig
    , creationBlock: currentBlock
    , totalDeposited: 0.0
    , lastUpdateBlock: currentBlock
    , oracle: oracle
    , eventEmitter: eventEmitter
    }

-- | Default risk parameters - sets up continuous leverage system
defaultRiskParameters :: PoolParameters -> RiskParameters
defaultRiskParameters poolConfig =
  { maxLeverage: 6.0  -- Default maximum leverage
  , currentCeiling: 3.0  -- Conservative starting ceiling
  , protectionCurve: Linear  -- Simple linear protection to start
  , lastCeilingUpdate: 0
  }

-- | Initialize pool state - sets starting point in 3D market space
initPoolState :: PoolParameters -> BlockNumber -> PoolState3D
initPoolState poolConfig currentBlock =
  { currentTick: { rateTick: 0, durationTick: 1, leverageTick: 0 }  -- Neutral rate, monthly duration, senior risk
  , sqrtRateX96: toQ96 1.0  -- Unity price in fixed-point
  , virtualBalances: { r: poolConfig.initialVirtualBalance, d: poolConfig.initialVirtualBalance, l: poolConfig.initialVirtualBalance }  -- Equal starting reserves
  , yieldRates: { rateYield: poolConfig.baseRateYield, durationYield: poolConfig.baseDurationYield, leverageYield: poolConfig.baseLeverageYield }  -- Base yields
  , totalLiquidity: 0.0
  , lastRedenomination: currentBlock
  }

--------------------------------------------------------------------------------
-- LIQUIDITY OPERATIONS
--------------------------------------------------------------------------------

-- | Add liquidity to a 3D range - creates concentrated liquidity position
addLiquidity3D :: AddLiquidity3DParams -> Effect Pool
addLiquidity3D params = do
  let
    -- Generate unique position identifier
    position3DId = params.owner <> "-" <> coordinateKey params.tickLower <> "-" <> coordinateKey params.tickUpper
    
    -- Convert token amounts to liquidity units
    liquidity = getLiquidityFor3DAmounts params.amount0 params.amount1 params.tickLower params.tickUpper params.pool.poolState.sqrtRateX96
    
    -- Define liquidity cube in 3D tick space
    position3D = { id: position3DId
           , owner: params.owner
           , tickLower: params.tickLower
           , tickUpper: params.tickUpper
           , liquidity: liquidity
           , liquidityBase: liquidity  -- Base for yield tracking
           , lastRedenomination: params.currentBlock
           , initialized: true
           }
    
    -- Create user's position record with NFT capability
    positionId = hashString position3DId
    position = { id: positionId
               , owner: params.owner
               , liquidity3DId: position3DId
               , liquidity: liquidity
               , principal: params.amount0 + params.amount1  -- Track initial investment
               , currentValue: params.amount0 + params.amount1  -- Will grow with yield
               , lastRedenomination: params.currentBlock
               , nftId: Nothing  -- Can mint NFT later
               , transferable: true  -- Enables secondary markets
               }
    
    -- Update pool state with new position
    updated3Ds = Map.insert position3DId position3D params.pool.liquidity3Ds
    updatedPositions = Map.insert positionId position params.pool.positions
    updatedActiveTicks = addToActiveTicks params.pool.activeTicks params.tickLower params.tickUpper
    updatedPoolState = params.pool.poolState { totalLiquidity = params.pool.poolState.totalLiquidity + liquidity }
    
    updatedPool = updatePoolState
       (params.pool { liquidity3Ds = updated3Ds
             , positions = updatedPositions
             , activeTicks = updatedActiveTicks
             , totalDeposited = params.pool.totalDeposited + params.amount0 + params.amount1  -- Track deposits
             })
       updatedPoolState
       params.currentBlock
  
  -- Emit liquidity added event
  emit updatedPool.eventEmitter (LiquidityAdded { positionId: position3DId, amount: liquidity })
  
  -- Mint position NFT if requested
  finalPool <- if params.mintNFT
               then mintPositionNFT updatedPool positionId params.owner
               else pure updatedPool
  
  pure finalPool

-- | Remove liquidity from a pool position
removeLiquidity3D :: Pool -> PositionId -> Number -> BlockNumber -> Effect (Either String Pool)
removeLiquidity3D pool positionId liquidityAmount currentBlock = do
  case removeLiquidity3DPure pool positionId liquidityAmount currentBlock of
    Left err -> pure $ Left err
    Right updatedPool -> do
      emit updatedPool.eventEmitter (LiquidityRemoved { positionId: show positionId, amount: liquidityAmount })
      pure $ Right updatedPool

-- | Pure liquidity removal logic - calculates state changes
removeLiquidity3DPure :: Pool -> PositionId -> Number -> BlockNumber -> Either String Pool
removeLiquidity3DPure pool positionId liquidityAmount currentBlock = do
  poolPosition <- requireFound (Map.lookup positionId pool.positions) positionNotFoundError
    
  -- Apply accumulated yield before withdrawal
  let redenominatedPosition = redenominatePoolPosition pool poolPosition currentBlock
  
  _ <- requireCondition (redenominatedPosition.liquidity >= liquidityAmount) insufficientLiquidityError
  
  position3D <- requireFound (Map.lookup poolPosition.liquidity3DId pool.liquidity3Ds) "3D liquidity position not found"
  
  -- Update 3D position with yield
  let redenominated3D = redenominate3D pool currentBlock position3D
  
  let
    -- Calculate token amounts to return
    shareof3D = liquidityAmount / redenominated3D.liquidity
    amount0 = getAmount0For3DLiquidity liquidityAmount redenominated3D.tickLower redenominated3D.tickUpper pool.poolState.sqrtRateX96
    amount1 = getAmount1For3DLiquidity liquidityAmount redenominated3D.tickLower redenominated3D.tickUpper pool.poolState.sqrtRateX96
    
    -- Reduce position size proportionally
    remainingLiquidity = redenominatedPosition.liquidity - liquidityAmount
    poolPosition' = redenominatedPosition
      { liquidity = remainingLiquidity
      , currentValue = redenominatedPosition.currentValue * remainingLiquidity / redenominatedPosition.liquidity
      }
    
    -- Update 3D position
    position3D' = redenominated3D 
      { liquidity = redenominated3D.liquidity - liquidityAmount }
    
    -- Clean up dust positions below minimum
    positions' = if remainingLiquidity > defaultProtocolConfig.pools.minLiquidity
                then Map.insert positionId poolPosition' pool.positions
                else Map.delete positionId pool.positions
    
    liquidity3Ds' = if position3D'.liquidity > defaultProtocolConfig.pools.minLiquidity
            then Map.insert position3D.id position3D' pool.liquidity3Ds
            else Map.delete position3D.id pool.liquidity3Ds
    
    poolState' = pool.poolState
      { totalLiquidity = pool.poolState.totalLiquidity - liquidityAmount }
    
    -- Update total deposited (subtract withdrawn amounts)
    totalDeposited' = pool.totalDeposited - (amount0 + amount1)
    
    updatedPool = pool
      { positions = positions'
      , liquidity3Ds = liquidity3Ds'
      , poolState = poolState'
      , totalDeposited = totalDeposited'
      , lastUpdateBlock = currentBlock
      }
    
  pure updatedPool

--------------------------------------------------------------------------------
-- SWAP OPERATIONS
--------------------------------------------------------------------------------

-- | Distribute fees to affected 3D liquidity positions weighted by contribution
distributeFees3D :: Array Liquidity3D -> Number -> BlockNumber -> Array Liquidity3D
distributeFees3D affected3DPositions feeAmount currentBlock =
  let
    -- Sum liquidity for pro-rata distribution
    totalLiquidity = sum $ map _.liquidity affected3DPositions
    
    -- Apply fees as yield to each position
    distributeFees3DPosition position3D =
      if totalLiquidity > 0.0 && position3D.liquidity > 0.0
      then
        let
          -- Pro-rata share based on liquidity
          liquidityWeight = position3D.liquidity / totalLiquidity
          
          -- Position's fee allocation
          positionFees = feeAmount * liquidityWeight
          
          -- Convert to yield percentage
          feeYield = positionFees / position3D.liquidity
          
          -- Compound into position
          growthFactor = 1.0 + feeYield
        in position3D { liquidity = position3D.liquidity * growthFactor
                      , lastRedenomination = currentBlock
                      }
      else position3D
      
  in map distributeFees3DPosition affected3DPositions

-- | Update pool state after swap - handles tick crossings and state transitions
updatePoolStateAfterSwap :: Pool -> TickCoordinate -> VirtualBalances -> Number -> Array Liquidity3D -> BlockNumber -> Pool
updatePoolStateAfterSwap pool targetTick newBalances newSqrtRate updated3Ds currentBlock =
  let
    -- Find ticks crossed during swap
    crossedTicks = getTicksCrossed pool.poolState.currentTick targetTick
    
    -- Update liquidity at crossed ticks
    updatedTickData = foldl (updateTickDataForCrossing pool.poolState.currentTick targetTick) pool.tickData crossedTicks
    
    -- Refresh active tick list
    newActiveTicks = updateActiveTicks pool.activeTicks crossedTicks pool.poolState.currentTick targetTick
    
    -- Update liquidity positions
    liquidity3Ds' = foldl (\m p -> Map.insert p.id p m) pool.liquidity3Ds updated3Ds
    
    -- Update pool state
    poolState' = pool.poolState 
      { currentTick = targetTick
      , sqrtRateX96 = newSqrtRate
      , virtualBalances = newBalances
      }
    
    -- Emit tick crossing events
    _ = traverse_ (\tick -> 
        emit pool.eventEmitter (TickCrossed { tick: tick, direction: getTickDirection pool.poolState.currentTick targetTick })
      ) crossedTicks
    
  in pool { liquidity3Ds = liquidity3Ds'
          , poolState = poolState'
          , tickData = updatedTickData
          , activeTicks = newActiveTicks
          , lastUpdateBlock = currentBlock
          }

-- | Get ticks crossed during a swap
getTicksCrossed :: TickCoordinate -> TickCoordinate -> Array TickCoordinate
getTicksCrossed fromTick toTick =
  let
    -- Calculate tick ranges for each dimension
    rateRange = if fromTick.rateTick < toTick.rateTick
                then Array.range (fromTick.rateTick + 1) toTick.rateTick
                else Array.range toTick.rateTick (fromTick.rateTick - 1)
    
    durationRange = if fromTick.durationTick < toTick.durationTick
                    then Array.range (fromTick.durationTick + 1) toTick.durationTick
                    else Array.range toTick.durationTick (fromTick.durationTick - 1)
    
    leverageRange = if fromTick.leverageTick < toTick.leverageTick
                    then Array.range (fromTick.leverageTick + 1) toTick.leverageTick
                    else Array.range toTick.leverageTick (fromTick.leverageTick - 1)
    
    -- Create tick coordinates for each crossed tick
    -- For simplicity, we track major tick crossings in the dominant dimension
    dominantDimension = getDominantDimension fromTick toTick
  in
    case dominantDimension of
      RateDimension -> map (\r -> { rateTick: r, durationTick: toTick.durationTick, leverageTick: toTick.leverageTick }) rateRange
      DurationDimension -> map (\d -> { rateTick: toTick.rateTick, durationTick: d, leverageTick: toTick.leverageTick }) durationRange
      LeverageDimension -> map (\l -> { rateTick: toTick.rateTick, durationTick: toTick.durationTick, leverageTick: l }) leverageRange

-- | Determine dominant dimension for tick crossing
getDominantDimension :: TickCoordinate -> TickCoordinate -> SwapDimension
getDominantDimension fromTick toTick =
  let
    rateDelta = abs (toTick.rateTick - fromTick.rateTick)
    durationDelta = abs (toTick.durationTick - fromTick.durationTick)
    leverageDelta = abs (toTick.leverageTick - fromTick.leverageTick)
  in
    if rateDelta >= durationDelta && rateDelta >= leverageDelta
    then RateDimension
    else if durationDelta >= leverageDelta
    then DurationDimension
    else LeverageDimension

-- | Update tick data when crossing a tick
updateTickDataForCrossing :: TickCoordinate -> TickCoordinate -> Map String TickData -> TickCoordinate -> Map String TickData
updateTickDataForCrossing fromTick toTick tickDataMap crossedTick =
  let
    key = show crossedTick.rateTick <> "-" <> show crossedTick.durationTick <> "-" <> show crossedTick.leverageTick
    direction = getTickDirection fromTick toTick
  in
    case Map.lookup key tickDataMap of
      Nothing -> tickDataMap
      Just tickData ->
        let
          -- Update liquidity based on crossing direction
          updatedLiquidity = case direction of
            UpwardCrossing -> tickData.liquidityGross + tickData.liquidityNet
            DownwardCrossing -> tickData.liquidityGross - tickData.liquidityNet
            _ -> tickData.liquidityGross
          
          updatedTickData = tickData { liquidityGross = updatedLiquidity }
        in Map.insert key updatedTickData tickDataMap

-- | Get tick crossing direction
getTickDirection :: TickCoordinate -> TickCoordinate -> TickDirection
getTickDirection fromTick toTick =
  let
    -- Compare overall tick values (simplified)
    fromValue = fromTick.rateTick + fromTick.durationTick + fromTick.leverageTick
    toValue = toTick.rateTick + toTick.durationTick + toTick.leverageTick
  in
    if toValue > fromValue then UpwardCrossing
    else if toValue < fromValue then DownwardCrossing
    else NoCrossing

-- | Update active ticks after crossing
updateActiveTicks :: Array TickCoordinate -> Array TickCoordinate -> TickCoordinate -> TickCoordinate -> Array TickCoordinate
updateActiveTicks currentActive crossedTicks fromTick toTick =
  let
    -- Remove ticks that are no longer active
    stillActive = Array.filter (\t -> not (Array.elem t crossedTicks)) currentActive
    
    -- Add newly active ticks (simplified - would need actual liquidity check)
    newlyActive = Array.filter (\t -> hasLiquidityAt t) crossedTicks
  in
    Array.nub (stillActive <> newlyActive <> [toTick])
  where
    -- Placeholder for liquidity check
    hasLiquidityAt _ = true

-- | Tick crossing direction
data TickDirection = UpwardCrossing | DownwardCrossing | NoCrossing

derive instance eqTickDirection :: Eq TickDirection

-- | Swap dimension
data SwapDimension = RateDimension | DurationDimension | LeverageDimension

derive instance eqSwapDimension :: Eq SwapDimension

-- | Create swap result
createSwapResult :: Number -> Number -> TickCoordinate -> Number -> Number -> SwapResult3D
createSwapResult amountIn amountOut executionTick feeAmount sqrtRateAfter =
  { amountIn: amountIn
  , amountOut: amountOut
  , executionTick: executionTick
  , feeAmount: feeAmount
  , sqrtRateAfter: sqrtRateAfter
  }

-- | Execute a swap through the 3D AMM - main trading entry point
-- Rate tick meaning varies by duration:
-- - Swap: Exchange rate between tokens
-- - Monthly: Annual lending rate
-- - Flash: Single-block borrow fee
swap3D :: Pool -> TickCoordinate -> Number -> Boolean -> BlockNumber -> Effect (Tuple Pool SwapResult3D)
swap3D pool targetTick amountIn isExactInput currentBlock = do
  -- Fetch external price reference
  oraclePrice <- getCurrentPrice pool.oracle
  
  let
    -- Current AMM reserves
    currentBalances = pool.poolState.virtualBalances
    
    -- Price calculation with risk adjustments
    swapPrice = calculateSwapPrice3DWithRisk pool currentBalances pool.poolState.currentTick targetTick currentBlock
    
    -- Oracle deviation check
    maxDeviation = defaultProtocolConfig.pools.maxOracleDeviation
    priceDeviation = abs (swapPrice - oraclePrice) / oraclePrice
    
  -- Handle excessive price deviation
  if priceDeviation > maxDeviation
  then do
    -- Alert on price divergence
    emit pool.eventEmitter (OracleUpdated { oldPrice: swapPrice, newPrice: oraclePrice })
    -- Continue with swap but flag the deviation
    pure $ executeSwapWithOracle pool targetTick amountIn isExactInput currentBlock oraclePrice true
  else
    pure $ executeSwapWithOracle pool targetTick amountIn isExactInput currentBlock oraclePrice false

-- | Execute swap with oracle price consideration
executeSwapWithOracle :: Pool -> TickCoordinate -> Number -> Boolean -> BlockNumber -> Number -> Boolean -> Tuple Pool SwapResult3D
executeSwapWithOracle pool targetTick amountIn isExactInput currentBlock oraclePrice hasDeviation =
  let
    -- Get current virtual balances
    currentBalances = pool.poolState.virtualBalances
    
    -- Calculate price impact using Balancer formula with risk adjustment
    swapPrice = calculateSwapPrice3DWithRisk pool currentBalances pool.poolState.currentTick targetTick currentBlock
    
    -- Calculate output amount using invariant preservation with adjustments
    k_before = calculateInvariant3DWithAdjustments pool currentBalances pool.poolState.currentTick currentBlock
    
    -- Update balances based on swap direction
    Tuple newBalances amountOut = executeSwapInvariant pool currentBalances targetTick amountIn isExactInput
    
    -- Apply fee
    feeAmount = amountIn * pool.feeRate / 10000.0
    finalAmountOut = amountOut * (1.0 - pool.feeRate / 10000.0)
    
    -- Calculate new sqrt rate
    newSqrtRate = calculateNewSqrtRate pool.poolState.currentTick targetTick pool.poolState.sqrtRateX96
    
    -- Distribute fees to affected 3D positions (cubes in the 3D space)
    affected3DPositions = getAffected3DPositions pool pool.poolState.currentTick targetTick
    updated3DPositions = distributeFees3D affected3DPositions feeAmount currentBlock
    
    -- Update pool state
    pool' = updatePoolStateAfterSwap pool targetTick newBalances newSqrtRate updated3DPositions currentBlock
    
    -- Create swap result
    swapResult = createSwapResult amountIn finalAmountOut targetTick feeAmount newSqrtRate
    
    -- Emit swap event
    _ = emit pool'.eventEmitter (SwapExecuted { pool: pool.id, amountIn: amountIn, amountOut: finalAmountOut, fee: feeAmount, sqrtPrice: newSqrtRate })
    
  in Tuple pool' swapResult

-- | Execute swap preserving Balancer invariant
executeSwapInvariant :: Pool -> VirtualBalances -> TickCoordinate -> Number -> Boolean -> Tuple VirtualBalances Number
executeSwapInvariant pool balances targetTick amountIn isExactInput =
  let
    -- Determine which dimensions are being swapped
    currentTick = pool.poolState.currentTick
    
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
      solveDimensionSwap { balances, amountIn, currentWeights, targetWeights, k_target: k_current, isExactInput, dimension: RateDimension, currentWd, targetWd }
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension swap  
      solveDimensionSwap { balances, amountIn, currentWeights, targetWeights, k_target: k_current, isExactInput, dimension: DurationDimension, currentWd, targetWd }
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap
      solveDimensionSwap { balances, amountIn, currentWeights, targetWeights, k_target: k_current, isExactInput, dimension: LeverageDimension, currentWd, targetWd }
    else
      -- Multi-dimensional swap - use composite approach
      solveMultiDimensionalSwap pool balances amountIn currentTick targetTick k_current isExactInput
    
  in result

-- | Parameters for dimension swap solver
type DimensionSwapParams =
  { balances :: VirtualBalances
  , amountIn :: Number
  , currentWeights :: DimensionWeights
  , targetWeights :: DimensionWeights
  , k_target :: Number
  , isExactInput :: Boolean
  , dimension :: SwapDimension
  , currentWd :: Number
  , targetWd :: Number
  }

-- | Parameters for adding liquidity in 3D
type AddLiquidity3DParams =
  { pool :: Pool
  , owner :: String
  , tickLower :: TickCoordinate
  , tickUpper :: TickCoordinate
  , amount0 :: Number
  , amount1 :: Number
  , currentBlock :: BlockNumber
  , mintNFT :: Boolean  -- Whether to mint position NFT
  }

-- | Unified dimension swap solver - handles single-dimension trades
solveDimensionSwap :: DimensionSwapParams -> Tuple VirtualBalances Number
solveDimensionSwap params =
  let
    -- Time decay adjustment for duration dimension
    weightRatio = if params.currentWd > 0.0 then params.targetWd / params.currentWd else 1.0
    
    -- Update function for target dimension
    updateBalance = case params.dimension of
      RateDimension -> \b amt -> b { r = b.r + amt }
      DurationDimension -> \b amt -> b { d = b.d + amt * weightRatio }
      LeverageDimension -> \b amt -> b { l = b.l + amt }
    
    isPrimaryDimension = case params.dimension of
      RateDimension -> true
      _ -> false
  in
    if params.isExactInput then
      let
        amountToAdd = case params.dimension of
          DurationDimension -> params.amountIn * weightRatio
          _ -> params.amountIn
        newBalances = updateBalance params.balances amountToAdd
        amountOut = calculateOutputFromInvariant params.balances params.amountIn params.k_target params.currentWeights isPrimaryDimension
      in Tuple newBalances amountOut
    else
      let
        amountNeeded = calculateInputFromInvariant params.balances params.amountIn params.k_target params.currentWeights isPrimaryDimension
        amountToAdd = case params.dimension of
          DurationDimension -> amountNeeded * weightRatio
          _ -> amountNeeded
        newBalances = updateBalance params.balances amountToAdd
      in Tuple newBalances params.amountIn

-- | Solve multi-dimensional swap - handles cross-dimension trades
solveMultiDimensionalSwap :: Pool -> VirtualBalances -> Number -> TickCoordinate -> TickCoordinate -> Number -> Boolean -> Tuple VirtualBalances Number
solveMultiDimensionalSwap pool balances amountIn fromTick toTick k_target isExactInput =
  let
    -- Complex pricing for multi-dimension moves
    pathPrice = calculatePathDependentPrice balances pool.weights fromTick toTick
    
    -- Risk-adjusted weights for target position
    effectiveWeights = calculateEffectiveWeights pool.weights pool.riskParams toTick
    
    -- Calculate dimension changes as ratios
    rateRatio = pow defaultProtocolConfig.pools.tickBase 
                   (toNumber (toTick.rateTick - fromTick.rateTick) / defaultProtocolConfig.pools.ticksPerPercent)
    durationRatio = getDurationMultiplierFromTick defaultProtocolConfig.pools toTick.durationTick / 
                   getDurationMultiplierFromTick defaultProtocolConfig.pools fromTick.durationTick
    leverageRatio = getLeverageMultiplierFromTick defaultProtocolConfig.pools toTick.leverageTick /
                   getLeverageMultiplierFromTick defaultProtocolConfig.pools fromTick.leverageTick
    
  in
    if isExactInput then
      let
        -- For exact input, solve for output using Newton-Raphson method
        -- Initial guess based on path price
        initialGuess = amountIn / pathPrice
        
        -- Iteratively solve for output that maintains invariant
        solveOutput :: Number -> Int -> Number
        solveOutput guess iteration =
          if iteration > 10 then guess  -- Max 10 iterations
          else
            let
              -- Calculate new balances with current guess
              deltaR = amountIn * effectiveWeights.wr * rateRatio
              deltaD = amountIn * effectiveWeights.wd * durationRatio
              deltaL = amountIn * effectiveWeights.wl * leverageRatio
              
              newBalances = { r: balances.r + deltaR - guess * effectiveWeights.wr
                            , d: balances.d + deltaD - guess * effectiveWeights.wd
                            , l: balances.l + deltaL - guess * effectiveWeights.wl
                            }
              
              -- Check invariant
              k_new = calculateInvariant3D newBalances pool.weights
              error = (k_new - k_target) / k_target
            in
              -- If close enough, return
              if abs error < 0.0001 then guess
              else
                -- Adjust guess based on error
                let adjustment = guess * error * 0.5  -- Damping factor
                in solveOutput (guess - adjustment) (iteration + 1)
            
        amountOut = solveOutput initialGuess 0
        
        -- Calculate final balances
        deltaR = amountIn * effectiveWeights.wr * rateRatio
        deltaD = amountIn * effectiveWeights.wd * durationRatio  
        deltaL = amountIn * effectiveWeights.wl * leverageRatio
        
        newBalances = { r: balances.r + deltaR - amountOut * effectiveWeights.wr
                      , d: balances.d + deltaD - amountOut * effectiveWeights.wd
                      , l: balances.l + deltaL - amountOut * effectiveWeights.wl
                      }
        
      in Tuple newBalances amountOut
    else
      -- For exact output, solve for required input
      let
        -- Initial guess based on path price
        initialGuess = amountIn * pathPrice
        
        -- Newton's method to find required input
        solveInput :: Number -> Int -> Number
        solveInput guess iteration =
          if iteration > 10 then guess  -- Max iterations
          else
            let
              -- Calculate new balances with current guess
              deltaR = guess * effectiveWeights.wr * rateRatio
              deltaD = guess * effectiveWeights.wd * durationRatio
              deltaL = guess * effectiveWeights.wl * leverageRatio
              
              newBalances = { r: balances.r + deltaR - amountIn * effectiveWeights.wr
                            , d: balances.d + deltaD - amountIn * effectiveWeights.wd
                            , l: balances.l + deltaL - amountIn * effectiveWeights.wl
                            }
              
              -- Verify invariant preservation
              k_new = calculateInvariant3D newBalances pool.weights
              error = (k_new - k_target) / k_target
            in
              if abs error < 0.0001 then guess  -- Converged
              else
                let adjustment = guess * error * 0.5  -- Damped Newton step
                in solveInput (guess - adjustment) (iteration + 1)
        
        requiredInput = solveInput initialGuess 0
        
        -- Calculate final balances
        deltaR = requiredInput * effectiveWeights.wr * rateRatio
        deltaD = requiredInput * effectiveWeights.wd * durationRatio
        deltaL = requiredInput * effectiveWeights.wl * leverageRatio
        
        newBalances = { r: balances.r + deltaR - amountIn * effectiveWeights.wr
                      , d: balances.d + deltaD - amountIn * effectiveWeights.wd
                      , l: balances.l + deltaL - amountIn * effectiveWeights.wl
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
  desiredOutput * defaultProtocolConfig.pools.defaultSwapBuffer  -- Add buffer for slippage protection

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

-- | Redenominate pool positions - apply accumulated yield to all positions
redenominatePoolPositions :: Pool -> BlockNumber -> Pool
redenominatePoolPositions pool currentBlock =
  let
    -- Update 3D positions with yield and loss distribution
    redenominated3Ds = map (redenominate3DWithWaterfall pool currentBlock) pool.liquidity3Ds
    
    -- Propagate changes to user positions
    redenominatePos pos = 
      case Map.lookup pos.liquidity3DId redenominated3Ds of
        Nothing -> pos
        Just position3D -> redenominatePoolPositionWith3D pos position3D currentBlock
    
    positions' = map redenominatePos pool.positions
    
    -- Update pool state
    poolState' = pool.poolState { lastRedenomination = currentBlock }
    
  in pool { positions = positions'
          , liquidity3Ds = redenominated3Ds
          , poolState = poolState'
          }

-- | Calculate yield growth factor - compounds multi-dimensional yields
calculateYieldGrowthFactor :: Pool -> Liquidity3D -> Int -> Number
calculateYieldGrowthFactor pool position3D blocksSince =
  let
    -- Yield from each dimension
    rateYield = pool.poolState.yieldRates.rateYield * toNumber blocksSince
    durationYield = calculateDurationYield position3D blocksSince
    leverageYield = calculateLeverageYield position3D blocksSince pool.poolState.yieldRates.leverageYield
    
    -- Compound over time period
    totalYield = rateYield + durationYield + leverageYield
  in pow (1.0 + totalYield) (toNumber blocksSince / defaultProtocolConfig.time.blocksPerYear)

-- | Redenominate a single 3D liquidity position - apply yield growth
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
    position3DGrowth = calculateGrowthFactor position3D.liquidity position3D.liquidityBase defaultProtocolConfig.pools.minLiquidity
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
        blocksIntoMonth = blocksSince `mod` defaultProtocolConfig.time.blocksPerMonth
        progressToMaturity = toNumber blocksIntoMonth / toNumber defaultProtocolConfig.time.blocksPerMonth
        
        -- Time-based yield: value converges to par as maturity approaches
        -- LPs earn from this convergence
        convergenceYield = progressToMaturity * defaultProtocolConfig.pools.monthlyConvergenceRate
        
      in convergenceYield * toNumber blocksSince / toNumber defaultProtocolConfig.time.blocksPerMonth
    _ -> 0.0  -- No time value for Flash/Swap

-- | Calculate leverage-based yield
calculateLeverageYield :: Liquidity3D -> Int -> Number -> Number
calculateLeverageYield position3D blocksSince baseYield =
  let
    -- Convert tick to leverage
    tickToLeverage t = 1.0 + (toNumber t) / 10.0
    leverage = tickToLeverage position3D.tickLower.leverageTick
    
    -- Yield scales with leverage (higher leverage = higher yield)
    leverageYieldMultiplier = pow leverage 0.5  -- Square root scaling
    
  in baseYield * leverageYieldMultiplier * toNumber blocksSince

-- | Calculate total pool value - weighted sum of virtual reserves
calculateTotalPoolValue :: Pool -> Number
calculateTotalPoolValue pool =
  let balances = pool.poolState.virtualBalances
      weights = pool.weights
  in balances.r * weights.wr + balances.d * weights.wd + balances.l * weights.wl

--------------------------------------------------------------------------------
-- TICK MATH
--------------------------------------------------------------------------------

-- | Get sqrt rate at tick - converts discrete tick to continuous price
-- Swap: tick 0 = 1:1 exchange rate
-- Monthly: tick 500 = 5% annual rate
-- Flash: tick 30 = 0.3% single-block fee
getSqrtRateAtTick :: Int -> Number
getSqrtRateAtTick tick =
  let
    tickNumber = toNumber tick
    rate = pow defaultProtocolConfig.pools.tickBase (tickNumber / defaultProtocolConfig.pools.ticksPerPercent)  -- Exponential pricing
    sqrtRate = pow rate 0.5
  in toQ96 sqrtRate

-- | Get tick at sqrt rate - inverse of getSqrtRateAtTick
getTickAtSqrtRate :: Number -> Int
getTickAtSqrtRate sqrtRateX96 =
  let
    sqrtRate = fromQ96 sqrtRateX96
    rate = sqrtRate * sqrtRate
    tick = log rate / log defaultProtocolConfig.pools.tickBase * defaultProtocolConfig.pools.ticksPerPercent
  in round tick

-- | Token amount type for clarity
data TokenAmount = Amount0 | Amount1

-- | Get duration multiplier from tick - maps time commitment to yield
getDurationMultiplierFromTick :: PoolParameters -> Int -> Number
getDurationMultiplierFromTick poolConfig 0 = poolConfig.flashDurationMultiplier      -- Flash (1 block)
getDurationMultiplierFromTick poolConfig 1 = poolConfig.swapDurationMultiplier       -- Swap (immediate)
getDurationMultiplierFromTick poolConfig 2 = poolConfig.monthlyDurationMultiplier * 0.25  -- Weekly
getDurationMultiplierFromTick poolConfig 3 = poolConfig.monthlyDurationMultiplier    -- Monthly
getDurationMultiplierFromTick poolConfig 4 = poolConfig.monthlyDurationMultiplier * 3.0   -- Quarterly
getDurationMultiplierFromTick poolConfig 5 = poolConfig.monthlyDurationMultiplier * 12.0  -- Annual
getDurationMultiplierFromTick _ _ = 1.0

-- | Get leverage multiplier from tick - maps continuous leverage
getLeverageMultiplierFromTick :: PoolParameters -> Int -> Number
getLeverageMultiplierFromTick _ tick = 1.0 + (toNumber tick) / 10.0  -- Maps 0-63 to 1.0-7.3x

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
    
    -- Apply 3D multipliers (use average of lower and upper ticks) - using default config for now
    poolConfig = defaultProtocolConfig.pools
    durationMult = (getDurationMultiplierFromTick poolConfig tickLower.durationTick + getDurationMultiplierFromTick poolConfig tickUpper.durationTick) / 2.0
    leverageMult = (getLeverageMultiplierFromTick poolConfig tickLower.leverageTick + getLeverageMultiplierFromTick poolConfig tickUpper.leverageTick) / 2.0
    
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
    
    -- Apply dimension divisors (use average of lower and upper ticks) - using default config for now
    poolConfig = defaultProtocolConfig.pools
    durationDiv = (getDurationMultiplierFromTick poolConfig tickLower.durationTick + getDurationMultiplierFromTick poolConfig tickUpper.durationTick) / 2.0
    leverageDiv = (getLeverageMultiplierFromTick poolConfig tickLower.leverageTick + getLeverageMultiplierFromTick poolConfig tickUpper.leverageTick) / 2.0
    
  in liquidity / (durationDiv * leverageDiv)

--------------------------------------------------------------------------------
-- BALANCER MATH
--------------------------------------------------------------------------------

-- | Calculate swap price in 3D space - core AMM pricing function
calculateSwapPrice3D :: VirtualBalances -> DimensionWeights -> TickCoordinate -> TickCoordinate -> Number
calculateSwapPrice3D balances weights fromTick toTick =
  let
    -- Identify dimension changes for pricing
    rateChange = fromTick.rateTick /= toTick.rateTick
    durationChange = fromTick.durationTick /= toTick.durationTick
    leverageChange = fromTick.leverageTick /= toTick.leverageTick
    
  in
    -- Single-dimension pricing
    if rateChange && not durationChange && not leverageChange then
      -- Rate dimension only - price/APR/fee change
      let tickDiff = toNumber (toTick.rateTick - fromTick.rateTick) / 10000.0
          basePrice = pow 1.0001 tickDiff  -- Exponential tick spacing
      in basePrice
      
    else if durationChange && not rateChange && not leverageChange then
      -- Duration dimension only - time value pricing
      let fromDuration = tickToDuration fromTick.durationTick
          toDuration = tickToDuration toTick.durationTick
          -- Get yield multipliers for time commitment
          poolConfig = defaultProtocolConfig.pools
          fromMult = getDurationMultiplierFromTick poolConfig fromTick.durationTick
          toMult = getDurationMultiplierFromTick poolConfig toTick.durationTick
          -- Balance-based price impact
          totalBalance = balances.r + balances.d + balances.l
          durationRatio = if totalBalance > 0.0 then balances.d / totalBalance else 0.333
          -- Single dimension weight
          weightRatio = 1.0
      in toMult / fromMult * (1.0 + (durationRatio - 0.333) * 0.1) * weightRatio
      
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage dimension swap: price reflects risk premium
      let poolConfig = defaultProtocolConfig.pools
          fromLeverage = getLeverageMultiplierFromTick poolConfig fromTick.leverageTick
          toLeverage = getLeverageMultiplierFromTick poolConfig toTick.leverageTick
          -- Calculate actual balance ratio from virtual balances
          totalBalance = balances.r + balances.d + balances.l
          leverageRatio = if totalBalance > 0.0 then balances.l / totalBalance else 0.333
          -- Weight ratio reflects relative importance
          totalWeight = weights.wr + weights.wd + weights.wl
          weightRatio = if totalWeight > 0.0 then weights.wl / totalWeight else 0.333
      in toLeverage / fromLeverage * (1.0 + (leverageRatio - 0.333) * 0.1) * weightRatio
      
    else
      -- Multi-dimensional: calculate path-dependent price
      calculatePathDependentPrice balances weights fromTick toTick

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
          -- Calculate actual balance ratio from virtual balances
          totalBalance = balances.r + balances.d + balances.l
          durationRatio = if totalBalance > 0.0 then balances.d / totalBalance else 0.333
      in durationPrice * (1.0 + (durationRatio - 0.333) * 0.1)
      
    else if leverageChange && not rateChange && not durationChange then
      -- Leverage swap with continuous risk profiles
      let tickToLeverage t = 1.0 + (toNumber t) / 10.0
          fromLeverage = tickToLeverage fromTick.leverageTick
          toLeverage = tickToLeverage toTick.leverageTick
          fromProfile = createRiskProfile fromLeverage pool.riskParams
          toProfile = createRiskProfile toLeverage pool.riskParams
          leveragePrice = toProfile.feeMultiplier / fromProfile.feeMultiplier
          weightAdjustment = fromWeights.wl / toWeights.wl
      in leveragePrice * weightAdjustment
      
    else
      -- Multi-dimensional swap: use base calculation
      calculateSwapPrice3D balances pool.weights fromTick toTick

-- | Calculate path-dependent price - handles multi-dimensional moves
calculatePathDependentPrice :: VirtualBalances -> DimensionWeights -> TickCoordinate -> TickCoordinate -> Number
calculatePathDependentPrice balances weights fromTick toTick =
  let
    -- Net change in each dimension
    rateChange = toTick.rateTick - fromTick.rateTick
    durationChange = toTick.durationTick - fromTick.durationTick
    leverageChange = toTick.leverageTick - fromTick.leverageTick
    
    -- Dimension-specific price impacts
    rateMultiplier = if rateChange == 0 then 1.0 
                     else pow defaultProtocolConfig.pools.tickBase (toNumber rateChange / defaultProtocolConfig.pools.ticksPerPercent)
    
    -- Time value adjustments
    poolConfig = defaultProtocolConfig.pools
    fromDurationMult = getDurationMultiplierFromTick poolConfig fromTick.durationTick
    toDurationMult = getDurationMultiplierFromTick poolConfig toTick.durationTick
    durationMultiplier = if durationChange == 0 then 1.0 
                        else toDurationMult / fromDurationMult
    
    -- Risk premium adjustments
    fromLeverageMult = getLeverageMultiplierFromTick poolConfig fromTick.leverageTick
    toLeverageMult = getLeverageMultiplierFromTick poolConfig toTick.leverageTick
    leverageMultiplier = if leverageChange == 0 then 1.0 
                        else toLeverageMult / fromLeverageMult
    
    -- Marginal prices via partial derivatives of invariant
    -- P_r = (wr * K) / (r * R^wr * D^wd * L^wl)
    invariant = calculateInvariant3D balances weights
    
    -- Rate dimension marginal price
    marginalRatePrice = if balances.r > 0.0 
                       then (weights.wr * invariant) / balances.r
                       else 1.0
    
    -- Duration dimension marginal price
    marginalDurationPrice = if balances.d > 0.0
                           then (weights.wd * invariant) / balances.d
                           else 1.0
    
    -- Marginal leverage price: ∂K/∂l
    marginalLeveragePrice = if balances.l > 0.0
                           then (weights.wl * invariant) / balances.l
                           else 1.0
    
    -- Path-dependent price is the weighted geometric mean of dimension prices
    -- This accounts for the interdependence between dimensions
    pathPrice = pow (rateMultiplier * marginalRatePrice) weights.wr *
                pow (durationMultiplier * marginalDurationPrice) weights.wd *
                pow (leverageMultiplier * marginalLeveragePrice) weights.wl
    
  in pathPrice

-- | Calculate AMM invariant K = R^wr * D^wd * L^wl
calculateInvariant3D :: VirtualBalances -> DimensionWeights -> Number
calculateInvariant3D balances weights =
  pow balances.r weights.wr * pow balances.d weights.wd * pow balances.l weights.wl

-- | Calculate risk-adjusted effective weights
calculateEffectiveWeights :: DimensionWeights -> RiskParameters -> TickCoordinate -> DimensionWeights
calculateEffectiveWeights baseWeights riskParams tick =
  let
    -- Convert tick to leverage and get risk profile
    tickToLeverage t = 1.0 + (toNumber t) / 10.0
    leverage = tickToLeverage tick.leverageTick
    riskProfile = createRiskProfile leverage riskParams
    
    -- Apply fee multiplier based on continuous leverage
    leverageMultiplier = riskProfile.feeMultiplier
    
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
        timeToMaturity = max 0.0 (1.0 - toNumber blocksElapsed / toNumber defaultProtocolConfig.time.blocksPerMonth)
      in baseWeight * timeToMaturity
    _ -> baseWeight  -- No time adjustment for Flash/Swap

-- | Calculate invariant with risk and time adjustments - dynamic AMM weights
calculateInvariant3DWithAdjustments :: Pool -> VirtualBalances -> TickCoordinate -> BlockNumber -> Number
calculateInvariant3DWithAdjustments pool balances tick currentBlock =
  let
    -- Risk-based weight adjustment
    riskAdjustedWeights = calculateEffectiveWeights pool.weights pool.riskParams tick
    
    -- Time decay for duration positions
    timeAdjustedWd = calculateTimeAdjustedWeight tick.durationTick pool.creationBlock currentBlock riskAdjustedWeights.wd
    
    -- Ensure weights sum to 1.0
    totalW = riskAdjustedWeights.wr + timeAdjustedWd + riskAdjustedWeights.wl
    finalWeights = { wr: riskAdjustedWeights.wr / totalW
                   , wd: timeAdjustedWd / totalW
                   , wl: riskAdjustedWeights.wl / totalW
                   }
    
  in pow balances.r finalWeights.wr * 
     pow balances.d finalWeights.wd * 
     pow balances.l finalWeights.wl

-- | Calculate dimension-specific liquidity - filters positions by predicate
calculateDimensionLiquidity :: (Liquidity3D -> Boolean) -> Array Liquidity3D -> Number
calculateDimensionLiquidity predicate positions3D = sum $ map (\pos3D -> if predicate pos3D then pos3D.liquidity else 0.0) positions3D

-- | Convert liquidity to virtual balance - dimension-specific scaling
liquidityToVirtualBalance :: Number -> Int -> Int -> Number
liquidityToVirtualBalance liquidity tickValue tickType =
  let poolConfig = defaultProtocolConfig.pools
  in case tickType of
    0 -> liquidity * fromQ96 (getSqrtRateAtTick tickValue)  -- Rate: price-based
    1 -> liquidity * getDurationMultiplierFromTick poolConfig tickValue  -- Duration: time-based
    _ -> liquidity * getLeverageMultiplierFromTick poolConfig tickValue  -- Leverage: risk-based

-- | Get virtual balances for a tick coordinate by aggregating 3D positions
getVirtualBalances :: Pool -> TickCoordinate -> VirtualBalances
getVirtualBalances pool tick =
  let
    -- Get all 3D positions that contain this tick
    relevant3DPositions = Array.filter (position3DContainsTick tick) (Array.fromFoldable (values pool.liquidity3Ds))
    
    -- Base virtual balances from protocol config
    baseVirtual = defaultProtocolConfig.pools.initialVirtualBalance
    
    -- Calculate contribution from each position
    calculatePositionContribution position3D =
      let
        -- Calculate how much of this position's liquidity contributes to each dimension
        -- based on how centered the current tick is within the position's range
        
        -- Rate dimension contribution
        rateRange = toNumber (position3D.tickUpper.rateTick - position3D.tickLower.rateTick)
        rateCenter = toNumber (position3D.tickLower.rateTick) + rateRange / 2.0
        rateDistance = abs (toNumber tick.rateTick - rateCenter)
        rateWeight = if rateRange > 0.0 
                     then max 0.0 (1.0 - rateDistance / (rateRange / 2.0))
                     else 1.0
        
        -- Duration dimension contribution
        durationRange = toNumber (position3D.tickUpper.durationTick - position3D.tickLower.durationTick)
        durationCenter = toNumber (position3D.tickLower.durationTick) + durationRange / 2.0
        durationDistance = abs (toNumber tick.durationTick - durationCenter)
        durationWeight = if durationRange > 0.0
                        then max 0.0 (1.0 - durationDistance / (durationRange / 2.0))
                        else 1.0
        
        -- Leverage dimension contribution
        leverageRange = toNumber (position3D.tickUpper.leverageTick - position3D.tickLower.leverageTick)
        leverageCenter = toNumber (position3D.tickLower.leverageTick) + leverageRange / 2.0
        leverageDistance = abs (toNumber tick.leverageTick - leverageCenter)
        leverageWeight = if leverageRange > 0.0
                        then max 0.0 (1.0 - leverageDistance / (leverageRange / 2.0))
                        else 1.0
        
        -- Total weight (product of dimension weights)
        totalWeight = rateWeight * durationWeight * leverageWeight
        
      in
        { r: position3D.liquidity * rateWeight * totalWeight
        , d: position3D.liquidity * durationWeight * totalWeight
        , l: position3D.liquidity * leverageWeight * totalWeight
        }
    
    -- Aggregate contributions from all positions
    contributions = map calculatePositionContribution relevant3DPositions
    aggregated = foldl 
      (\acc contrib -> { r: acc.r + contrib.r, d: acc.d + contrib.d, l: acc.l + contrib.l })
      { r: 0.0, d: 0.0, l: 0.0 }
      contributions
    
  in
    -- Add base virtual balances to aggregated liquidity
    { r: baseVirtual + aggregated.r
    , d: baseVirtual + aggregated.d
    , l: baseVirtual + aggregated.l
    }

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

--------------------------------------------------------------------------------
-- REBALANCING OPERATIONS
--------------------------------------------------------------------------------

-- | Rebalance pool liquidity across dimensions with slippage protection
rebalancePool :: Pool -> BlockNumber -> Effect Pool
rebalancePool pool currentBlock = do
  -- Get current oracle price
  oraclePrice <- getCurrentPrice pool.oracle
  
  let
    -- Get current and target virtual balances
    currentBalances = pool.poolState.virtualBalances
    targetBalances = calculateTargetBalances pool oraclePrice
    
    -- Calculate rebalancing needs
    rebalanceInfo = calculateRebalanceInfo currentBalances targetBalances pool.weights
    
    -- Check if rebalancing is needed (threshold check)
    needsRebalance = rebalanceInfo.maxDeviation > defaultProtocolConfig.pools.rebalanceThreshold
    
  if needsRebalance
  then do
    -- Execute rebalancing with slippage protection
    let rebalancedPool = executeRebalancing pool rebalanceInfo currentBlock
    
    -- Emit rebalancing event
    emit rebalancedPool.eventEmitter (PoolRebalanced 
      { oldBalances: currentBalances
      , newBalances: rebalancedPool.poolState.virtualBalances
      , deviation: rebalanceInfo.maxDeviation
      })
    
    pure rebalancedPool
  else
    pure pool

-- | Calculate target balances based on oracle price and pool parameters
calculateTargetBalances :: Pool -> Number -> VirtualBalances
calculateTargetBalances pool oraclePrice =
  let
    -- Total liquidity in the pool
    totalLiquidity = pool.poolState.totalLiquidity
    
    -- Calculate optimal distribution based on weights and oracle price
    -- Rate dimension should track oracle price
    rateTarget = totalLiquidity * pool.weights.wr * oraclePrice / pool.poolState.sqrtRateX96
    
    -- Duration dimension based on time decay
    durationTarget = totalLiquidity * pool.weights.wd
    
    -- Leverage dimension based on risk parameters
    leverageTarget = totalLiquidity * pool.weights.wl
    
  in { r: rateTarget, d: durationTarget, l: leverageTarget }

-- | Calculate rebalancing information
calculateRebalanceInfo :: VirtualBalances -> VirtualBalances -> DimensionWeights -> 
                         { maxDeviation :: Number
                         , rateAdjustment :: Number
                         , durationAdjustment :: Number
                         , leverageAdjustment :: Number
                         }
calculateRebalanceInfo current target weights =
  let
    -- Calculate deviations for each dimension
    rateDeviation = abs (current.r - target.r) / target.r
    durationDeviation = abs (current.d - target.d) / target.d
    leverageDeviation = abs (current.l - target.l) / target.l
    
    -- Maximum deviation across dimensions
    maxDeviation = max rateDeviation (max durationDeviation leverageDeviation)
    
    -- Calculate adjustments needed
    rateAdjustment = (target.r - current.r) * weights.wr
    durationAdjustment = (target.d - current.d) * weights.wd
    leverageAdjustment = (target.l - current.l) * weights.wl
    
  in { maxDeviation, rateAdjustment, durationAdjustment, leverageAdjustment }

-- | Execute rebalancing with slippage protection
executeRebalancing :: Pool -> 
                     { maxDeviation :: Number
                     , rateAdjustment :: Number
                     , durationAdjustment :: Number
                     , leverageAdjustment :: Number
                     } -> 
                     BlockNumber -> 
                     Pool
executeRebalancing pool rebalanceInfo currentBlock =
  let
    -- Apply adjustments with slippage limits
    maxSlippage = defaultProtocolConfig.pools.maxRebalanceSlippage
    
    -- Limit adjustments to prevent excessive slippage
    limitedRateAdj = min (abs rebalanceInfo.rateAdjustment) (pool.poolState.virtualBalances.r * maxSlippage) *
                     (if rebalanceInfo.rateAdjustment < 0.0 then -1.0 else 1.0)
    limitedDurationAdj = min (abs rebalanceInfo.durationAdjustment) (pool.poolState.virtualBalances.d * maxSlippage) *
                        (if rebalanceInfo.durationAdjustment < 0.0 then -1.0 else 1.0)
    limitedLeverageAdj = min (abs rebalanceInfo.leverageAdjustment) (pool.poolState.virtualBalances.l * maxSlippage) *
                        (if rebalanceInfo.leverageAdjustment < 0.0 then -1.0 else 1.0)
    
    -- Calculate new balances
    newBalances = 
      { r: max 0.1 (pool.poolState.virtualBalances.r + limitedRateAdj)  -- Minimum balance protection
      , d: max 0.1 (pool.poolState.virtualBalances.d + limitedDurationAdj)
      , l: max 0.1 (pool.poolState.virtualBalances.l + limitedLeverageAdj)
      }
    
    -- Update pool state
    newPoolState = pool.poolState { virtualBalances = newBalances }
    
  in pool 
    { poolState = newPoolState
    , lastUpdateBlock = currentBlock
    }

--------------------------------------------------------------------------------
-- POSITION NFT MANAGEMENT
--------------------------------------------------------------------------------

-- | Position NFT state tracking - enables secondary markets
type NFTRegistry = Map PositionId Int  -- Maps position ID to NFT token ID

-- | Mint NFT for a position - tokenizes liquidity for trading
mintPositionNFT :: Pool -> PositionId -> String -> Effect Pool
mintPositionNFT pool positionId owner = do
  case Map.lookup positionId pool.positions of
    Nothing -> pure pool  -- Position not found
    Just position ->
      let
        -- Generate unique NFT identifier
        nftId = hashString (owner <> "-" <> show positionId <> "-nft")
        
        -- Attach NFT to position
        updatedPosition = position { nftId = Just nftId }
        updatedPositions = Map.insert positionId updatedPosition pool.positions
        
        -- Broadcast minting event
        _ = emit pool.eventEmitter (PositionNFTMinted { positionId: positionId, nftId: nftId, owner: owner })
        
      in pure $ pool { positions = updatedPositions }

-- | Transfer position ownership via NFT
transferPositionNFT :: Pool -> PositionId -> String -> String -> Effect (Either String Pool)
transferPositionNFT pool positionId fromOwner toOwner = do
  case Map.lookup positionId pool.positions of
    Nothing -> pure $ Left "Position not found"
    Just position ->
      if position.owner /= fromOwner
      then pure $ Left "Not position owner"
      else if not position.transferable
      then pure $ Left "Position not transferable"
      else
        let
          -- Update position ownership
          updatedPosition = position { owner = toOwner }
          updatedPositions = Map.insert positionId updatedPosition pool.positions
          
          -- Also update 3D position ownership
          updated3Ds = case Map.lookup position.liquidity3DId pool.liquidity3Ds of
            Nothing -> pool.liquidity3Ds
            Just pos3D -> Map.insert position.liquidity3DId (pos3D { owner = toOwner }) pool.liquidity3Ds
          
          updatedPool = pool { positions = updatedPositions, liquidity3Ds = updated3Ds }
          
        in do
          -- Emit transfer event
          emit updatedPool.eventEmitter (PositionTransferred 
            { positionId: positionId
            , from: fromOwner
            , to: toOwner
            , nftId: position.nftId
            })
          
          pure $ Right updatedPool

-- | Burn position NFT (when closing position)
burnPositionNFT :: Pool -> PositionId -> Effect Pool
burnPositionNFT pool positionId = do
  case Map.lookup positionId pool.positions of
    Nothing -> pure pool
    Just position ->
      case position.nftId of
        Nothing -> pure pool
        Just nftId -> do
          -- Emit burn event
          emit pool.eventEmitter (PositionNFTBurned { positionId: positionId, nftId: nftId })
          pure pool

-- | Lock position - prevents NFT transfer for governance/staking
lockPosition :: Pool -> PositionId -> String -> Effect (Either String Pool)
lockPosition pool positionId owner = do
  case Map.lookup positionId pool.positions of
    Nothing -> pure $ Left "Position not found"
    Just position ->
      if position.owner /= owner
      then pure $ Left "Not position owner"
      else
        let
          updatedPosition = position { transferable = false }
          updatedPositions = Map.insert positionId updatedPosition pool.positions
        in pure $ Right $ pool { positions = updatedPositions }

-- | Calculate liquidity from amount - converts tokens to LP shares
calculateLiquidityFromAmount :: Pool -> Number -> TickCoordinate -> TickCoordinate -> Number
calculateLiquidityFromAmount pool amount tickLower tickUpper =
  let
    -- Current market position
    currentTick = pool.poolState.currentTick
    
    -- Check if current price is within each dimension's range
    rateInRange = currentTick.rateTick >= tickLower.rateTick && 
                  currentTick.rateTick <= tickUpper.rateTick
    durationInRange = currentTick.durationTick >= tickLower.durationTick && 
                      currentTick.durationTick <= tickUpper.durationTick
    leverageInRange = currentTick.leverageTick >= tickLower.leverageTick && 
                      currentTick.leverageTick <= tickUpper.leverageTick
    
    -- Get sqrt rates at boundaries
    sqrtRateLower = getSqrtRateAtTick tickLower.rateTick
    sqrtRateUpper = getSqrtRateAtTick tickUpper.rateTick
    sqrtRateCurrent = pool.poolState.sqrtRateX96
    
    -- Concentrated liquidity formula adapted for 3D
    baseLiquidity = if rateInRange then
      -- Active position - both tokens needed
      let deltaInvSqrt = 1.0 / fromQ96 sqrtRateCurrent - 1.0 / fromQ96 sqrtRateUpper
          deltaL = fromQ96 sqrtRateCurrent - fromQ96 sqrtRateLower
      in if deltaInvSqrt > 0.0 && deltaL > 0.0
         then amount / (deltaInvSqrt + deltaL)
         else amount / fromQ96 sqrtRateCurrent
    else if currentTick.rateTick < tickLower.rateTick then
      -- Below range - only token0 needed
      amount * fromQ96 sqrtRateLower * fromQ96 sqrtRateUpper / 
        (fromQ96 sqrtRateUpper - fromQ96 sqrtRateLower)
    else
      -- Above range - only token1 needed
      amount / (fromQ96 sqrtRateUpper - fromQ96 sqrtRateLower)
    
    -- Apply dimension multipliers (use average of lower and upper ticks)
    poolConfig = defaultProtocolConfig.pools
    durationMult = (getDurationMultiplierFromTick poolConfig tickLower.durationTick + getDurationMultiplierFromTick poolConfig tickUpper.durationTick) / 2.0
    leverageMult = (getLeverageMultiplierFromTick poolConfig tickLower.leverageTick + getLeverageMultiplierFromTick poolConfig tickUpper.leverageTick) / 2.0
    
    -- Adjust for dimensions not in range
    dimensionAdjustment = (if durationInRange then 1.0 else defaultProtocolConfig.pools.swapSolverBufferRatio) *
                         (if leverageInRange then 1.0 else defaultProtocolConfig.pools.swapSolverPremiumRatio)
    
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

-- | Group positions by their leverage level
groupPositionsByLeverage :: Pool -> Array { leverage :: Number, liquidity :: Number }
groupPositionsByLeverage pool =
  let
    -- Extract leverage from tick and accumulate liquidity
    positions3D = Array.fromFoldable (values pool.liquidity3Ds)
    
    -- Convert leverage tick to actual leverage value
    tickToLeverage :: Int -> Number
    tickToLeverage tick = 1.0 + (toNumber tick) / 10.0  -- Maps 0-63 to 1.0-7.3x
    
    -- Group by leverage value
    grouped = foldl (\acc pos -> 
      let lev = tickToLeverage pos.tickLower.leverageTick
          existing = fromMaybe 0.0 (Map.lookup lev acc)
      in Map.insert lev (existing + pos.liquidity) acc
    ) Map.empty positions3D
    
  in Array.fromFoldable $ map (\(Tuple lev liq) -> { leverage: lev, liquidity: liq }) (Map.toUnfoldable grouped :: Array (Tuple Number Number))

-- | Calculate pool performance ratio
calculatePoolPerformance :: Pool -> Number
calculatePoolPerformance pool =
  if pool.totalDeposited > 0.0
  then pool.poolState.totalLiquidity / pool.totalDeposited
  else 1.0

-- | Calculate loss distribution across continuous leverage levels
calculateLossDistribution :: Pool -> Number -> LossDistribution
calculateLossDistribution pool totalLoss =
  let
    -- Group positions by leverage level
    leverageGroups = groupPositionsByLeverage pool
    
    -- Sort by leverage (highest first absorbs losses)
    sortedGroups = Array.sortBy (\a b -> compare b.leverage a.leverage) leverageGroups
    
    -- Distribute losses starting from highest leverage
    distributeToGroups :: Number -> Array { leverage :: Number, liquidity :: Number } -> Map Number Number
    distributeToGroups remaining groups = 
      case Array.head groups of
        Nothing -> Map.empty
        Just group ->
          let profile = createRiskProfile group.leverage pool.riskParams
              maxGroupLoss = group.liquidity * profile.maxLossPercentage
              groupLoss = min remaining maxGroupLoss
              remainingLoss = remaining - groupLoss
              restDistribution = distributeToGroups remainingLoss (fromMaybe [] $ Array.tail groups)
          in Map.insert group.leverage groupLoss restDistribution
    
    distribution = distributeToGroups totalLoss sortedGroups
    
  in { totalLoss: totalLoss
     , distributionByLeverage: distribution
     }

-- | Redenominate 3D position with continuous leverage loss distribution
redenominate3DWithWaterfall :: Pool -> BlockNumber -> Liquidity3D -> Liquidity3D
redenominate3DWithWaterfall pool currentBlock position3D =
  let
    -- Apply yield growth first
    baseRedenomination = redenominate3D pool currentBlock position3D
    
    -- Check for losses to distribute
    poolPerformance = calculatePoolPerformance pool
  in
    if poolPerformance < 1.0 then
      let
        -- Calculate total loss
        totalLoss = (1.0 - poolPerformance) * pool.totalDeposited
        
        -- Get loss distribution across leverage levels
        lossDistribution = calculateLossDistribution pool totalLoss
        
        -- Convert position's leverage tick to actual leverage
        tickToLeverage tick = 1.0 + (toNumber tick) / 10.0
        positionLeverage = tickToLeverage position3D.tickLower.leverageTick
        
        -- Get loss for this leverage level
        leverageLoss = fromMaybe 0.0 (Map.lookup positionLeverage lossDistribution.distributionByLeverage)
        
        -- Get total liquidity at this leverage level
        leverageGroups = groupPositionsByLeverage pool
        leverageGroup = Array.head $ Array.filter (\g -> abs (g.leverage - positionLeverage) < 0.01) leverageGroups
        leverageTVL = maybe 0.0 _.liquidity leverageGroup
        
        -- Calculate loss factor
        lossFactor = if leverageTVL > 0.0 && leverageLoss > 0.0
                     then max 0.0 (1.0 - leverageLoss / leverageTVL)
                     else 1.0
      
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
    balances = pool.poolState.virtualBalances
    weights = pool.weights
    
    -- Rate component (unchanged)
    rateComponent = balances.r
    
    -- Time-value component with decay
    timeToMaturity = calculateTimeToMaturity tick.durationTick pool.creationBlock currentBlock
    timeAdjustedWd = calculateTimeAdjustedWeight tick.durationTick pool.creationBlock currentBlock weights.wd
    timeValueComponent = balances.d * pow timeToMaturity timeAdjustedWd
    
    -- Leverage component with risk adjustment
    tickToLeverage t = 1.0 + (toNumber t) / 10.0
    leverage = tickToLeverage tick.leverageTick
    riskProfile = createRiskProfile leverage pool.riskParams
    leverageComponent = balances.l * riskProfile.feeMultiplier
    
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
        maturityBlock = startBlock + defaultProtocolConfig.time.blocksPerMonth
        elapsed = currentBlock - startBlock
        timeToMaturity = max 0.0 (1.0 - toNumber elapsed / toNumber defaultProtocolConfig.time.blocksPerMonth)
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

-- | Better hash function for generating position IDs using djb2 algorithm
hashString :: String -> Int
hashString str = 
  let 
    -- DJB2 hash algorithm - simple but effective
    djb2Hash :: Int -> String -> Int
    djb2Hash hash "" = hash
    djb2Hash hash s = 
      case String.uncons s of
        Nothing -> hash
        Just { head, tail } -> 
          let newHash = ((hash * 33) + fromEnum head) `mod` 2147483647
          in djb2Hash newHash tail
  in abs (djb2Hash 5381 str)

-- | Create tick coordinate
createTickCoordinate :: Int -> Int -> Int -> TickCoordinate
createTickCoordinate rateTick durationTick leverageTick =
  { rateTick, durationTick, leverageTick }

-- | Update pool state helper
updatePoolState :: forall r. 
  { poolState :: PoolState3D
  , lastUpdateBlock :: BlockNumber 
  | r } -> 
  PoolState3D -> 
  BlockNumber -> 
  { poolState :: PoolState3D
  , lastUpdateBlock :: BlockNumber 
  | r }
updatePoolState pool newPoolState newBlock =
  pool { poolState = newPoolState
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

-- | Tick to leverage mapping - continuous leverage system
tickToLeverage :: Int -> Leverage
tickToLeverage tick = Leverage (1.0 + (toNumber tick) / 10.0)  -- Maps 0-63 to 1.0-7.3x

-- | Convert rate/price to tick
-- For Swap: price 1.05 → tick 500 (5% premium)
-- For Monthly: 5% APR → tick 500
-- For Flash: 0.3% fee → tick 30
rateToTick :: Number -> Int
rateToTick rate = round (rate * defaultProtocolConfig.pools.ticksPerPercent)

-- | Convert tick to rate/price
-- Interpretation depends on duration context
tickToRate :: Int -> Number
tickToRate tick = toNumber tick / defaultProtocolConfig.pools.ticksPerPercent

--------------------------------------------------------------------------------
-- TICK DATA MANAGEMENT
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
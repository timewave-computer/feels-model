module Position 
  ( Position
  , PositionId
  , PositionParams
  , TickIndex
  , createPosition
  , createPresetPosition
  , defaultParams
  , defaultParamsForTicker
  , describePosition
  , priceToTick
  , tickToPrice
  , tickSpacing
  , nearestValidTick
  ) where

import Prelude
import Token (TokenType(..), TokenAmount)
import Utils (filter, intercalate)
import Data.Array (uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, pow)
import Data.Int (toNumber, floor)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (log)
import FFI (currentTime, generateId, log10)

--------------------------------------------------------------------------------
-- Position Types
--------------------------------------------------------------------------------

-- Position ID for tracking
type PositionId = Int

-- Tick index representation - each position exists at a specific tick
-- Each tick represents a price level where liquidity can be provided
type TickIndex = Int

-- Position parameters
-- All positions are essentially "ticks" with specific parameters
type PositionParams =
  { amount :: Number         -- Size of position (liquidity)
  , pair :: TokenType        -- Must be Synthetic SOL ↔ Token pair
  , tickLower :: TickIndex   -- Lower tick boundary
  , tickUpper :: TickIndex   -- Upper tick boundary  
  , duration :: Int          -- 0 for spot/immediate, >0 for time-locked
  , leverageRate :: Number   -- Leverage rate (0.0 = no leverage)
  }

-- Position data structure
-- Unified type representing both user positions and pool liquidity positions
type Position =
  { id :: PositionId             -- Unique identifier for the position
  , owner :: String              -- Address/identifier of position owner
  , inputToken :: TokenAmount    -- Input tokens locked in the position
  , liquidity :: Number          -- Amount of liquidity (L in Uniswap V3 formula)
  , createdAt :: Number          -- Creation timestamp
  , params :: PositionParams     -- Position parameters including tick range
  , liquidityGross :: Number     -- Total liquidity at this position
  , liquidityNet :: Number       -- Net liquidity change when crossing
  , feeGrowthInside0 :: Number   -- Fee growth inside range (token0)
  , feeGrowthInside1 :: Number   -- Fee growth inside range (token1)
  , tokensOwed0 :: Number        -- Uncollected fees in token0
  , tokensOwed1 :: Number        -- Uncollected fees in token1
  }

--------------------------------------------------------------------------------
-- Tick Conversion Functions
--------------------------------------------------------------------------------

-- Convert price to tick index
-- price = 1.0001^tick, so tick = log(price) / log(1.0001)
priceToTick :: Number -> TickIndex
priceToTick price = floor (log10 price / log10 1.0001)

-- Convert tick index to price
-- price = 1.0001^tick
tickToPrice :: TickIndex -> Number
tickToPrice tick = pow 1.0001 (toNumber tick)

-- Get tick spacing for a given fee tier
-- Lower fee tiers have tighter tick spacing
tickSpacing :: Number -> Int
tickSpacing fee
  | fee <= 0.0005 = 10    -- 0.05% fee -> 10 tick spacing
  | fee <= 0.003 = 60     -- 0.3% fee -> 60 tick spacing  
  | otherwise = 200       -- 1% fee -> 200 tick spacing

-- Round tick to nearest valid tick for given spacing
nearestValidTick :: TickIndex -> Int -> TickIndex
nearestValidTick tick spacing =
  let quotient = tick / spacing
      rounded = floor (toNumber quotient + 0.5)
  in rounded * spacing

-- Create a tick range around a center price
-- width is the percentage range (e.g., 0.1 = 10% range)
priceRangeToTicks :: Number -> Number -> { lower :: TickIndex, upper :: TickIndex }
priceRangeToTicks centerPrice width =
  let lowerPrice = centerPrice * (1.0 - width / 2.0)
      upperPrice = centerPrice * (1.0 + width / 2.0)
  in { lower: priceToTick lowerPrice, upper: priceToTick upperPrice }

--------------------------------------------------------------------------------
-- Position Creation Functions
--------------------------------------------------------------------------------

-- Create a unified position with specified parameters
-- Single function handles all position types based on parameters
-- Locked Synthetic SOL ticks create feels assets at 75% collateral ratio
createPosition :: forall r. String -> TokenAmount -> PositionParams -> { collateralRatio :: Number | r } -> Effect { position :: Position, feelsAsset :: Maybe TokenAmount }
createPosition owner input params sysParams = do
  -- Describe position based on parameters
  let posDesc = describePosition params
  log $ "Creating position: " <> show input.amount <> " " <> show input.tokenType <> " - " <> posDesc
  
  timestamp <- currentTime
  let position = 
        { id: generateId timestamp
        , owner: owner
        , inputToken: input
        , liquidity: params.amount  -- Simplified: using amount as liquidity directly
        , createdAt: timestamp
        , params: params
        , liquidityGross: params.amount
        , liquidityNet: params.amount
        , feeGrowthInside0: 0.0
        , feeGrowthInside1: 0.0
        , tokensOwed0: 0.0
        , tokensOwed1: 0.0
        }
  
  -- Check if this creates feels assets (duration > 0 using Synthetic SOL)
  let createsAsset = input.tokenType == SyntheticSOL && params.duration > 0
  
  if createsAsset
    then do
      -- Effective collateral ratio = 0.75 × (1 / (1 + leverageRate))
      let effectiveRatio = sysParams.collateralRatio / (1.0 + params.leverageRate)
      let feelsAmount = input.amount * effectiveRatio
      log $ "Creating " <> show feelsAmount <> " " <> show params.pair <> " from locked position"
      log $ "  Effective collateral ratio: " <> show effectiveRatio <> " (base: " <> show sysParams.collateralRatio <> ", leverage: " <> show params.leverageRate <> ")"
      pure { position, feelsAsset: Just { tokenType: params.pair, amount: feelsAmount } }
    else
      pure { position, feelsAsset: Nothing }

-- Create positions with common presets
createPresetPosition :: forall r. String -> TokenAmount -> String -> { collateralRatio :: Number | r } -> Effect { position :: Position, feelsAsset :: Maybe TokenAmount }
createPresetPosition owner input preset sysParams = 
  let defaultPair = Token "DEFAULT"
      narrowRange = priceRangeToTicks 1.0 0.02    -- 2% range around current price
      wideRange = priceRangeToTicks 1.0 0.10      -- 10% range
      fullRange = { lower: -887272, upper: 887272 } -- Full range position
  in case preset of
    "spot" -> 
      -- Spot position: narrow range for concentrated liquidity
      createPosition owner input { amount: input.amount, pair: defaultPair, tickLower: narrowRange.lower, tickUpper: narrowRange.upper, duration: 0, leverageRate: 0.0 } sysParams
    "lending" ->
      -- Lending position: wide range, time-locked
      createPosition owner input { amount: input.amount, pair: defaultPair, tickLower: wideRange.lower, tickUpper: wideRange.upper, duration: 30, leverageRate: 0.0 } sysParams
    "leveraged" ->
      -- Leveraged position: concentrated liquidity with leverage
      createPosition owner input { amount: input.amount, pair: defaultPair, tickLower: narrowRange.lower, tickUpper: narrowRange.upper, duration: 0, leverageRate: 1.0 } sysParams
    "complex" ->
      -- Complex position: all features combined
      createPosition owner input { amount: input.amount, pair: defaultPair, tickLower: wideRange.lower, tickUpper: wideRange.upper, duration: 30, leverageRate: 1.0 } sysParams
    _ ->
      -- Default: full range position
      createPosition owner input { amount: input.amount, pair: defaultPair, tickLower: fullRange.lower, tickUpper: fullRange.upper, duration: 0, leverageRate: 0.0 } sysParams

-- Note: Position composition functions have been removed as they were unused
-- The system now focuses on direct position creation with parameters

--------------------------------------------------------------------------------
-- Position Helper Functions
--------------------------------------------------------------------------------

-- Helper to describe a position based on its parameters
describePosition :: PositionParams -> String
describePosition params =
  let priceLower = tickToPrice params.tickLower
      priceUpper = tickToPrice params.tickUpper
      rangeDesc = "Range: " <> formatPrice priceLower <> " - " <> formatPrice priceUpper
      durationDesc = if params.duration > 0 
                     then show params.duration <> " day lock" 
                     else "spot"
      leverageDesc = if params.leverageRate > 0.0 
                     then show (leverageRateToRatio params.leverageRate) <> "x leverage" 
                     else ""
      components = filter (_ /= "") [rangeDesc, durationDesc, leverageDesc]
  in case components of
    [] -> "base position"
    _ -> intercalate ", " components
  where
    formatPrice p = if p < 0.01 then show p else show (toNumber (floor (p * 100.0)) / 100.0)

--------------------------------------------------------------------------------
-- Leverage Functions
--------------------------------------------------------------------------------

-- Convert leverage rate to leverage ratio (multiplier)
-- Rate of 0.5 = 50% additional exposure = 1.5x ratio
leverageRateToRatio :: Number -> Number
leverageRateToRatio rate = 1.0 + rate

--------------------------------------------------------------------------------
-- Position Validation
--------------------------------------------------------------------------------

type ValidationResult = Either String Unit

-- Helper to combine validations
validate :: Boolean -> String -> ValidationResult
validate true _ = Right unit
validate false err = Left err

-- Validate position parameters
validatePositionParams :: PositionParams -> ValidationResult
validatePositionParams params = do
  _ <- validateAmount params.amount 0.0
  _ <- validateTickRange params.tickLower params.tickUpper
  _ <- validate (params.duration >= 0) "Duration must be non-negative"
  _ <- validateLeverageRate params.leverageRate
  Right unit

-- Check if parameters are valid (Boolean version for compatibility)
isValidParams :: PositionParams -> Boolean
isValidParams params = case validatePositionParams params of
  Right _ -> true
  Left _ -> false

-- Validate amount
validateAmount :: Number -> Number -> ValidationResult
validateAmount amount minAmount = do
  _ <- validate (amount >= minAmount) $ "Amount must be at least " <> show minAmount
  _ <- validate (amount < 1000000000.0) "Amount too large"
  Right unit

-- Validate tick range
validateTickRange :: Int -> Int -> ValidationResult
validateTickRange lower upper = do
  _ <- validate (lower < upper) "Lower tick must be less than upper tick"
  _ <- validate (lower >= -887272) "Lower tick out of range"
  _ <- validate (upper <= 887272) "Upper tick out of range"
  Right unit

-- Validate leverage rate (used by position params)
validateLeverageRate :: Number -> ValidationResult
validateLeverageRate rate = 
  validate (rate >= 0.0 && rate <= 10.0) "Leverage rate must be between 0 and 10"

-- Default parameters for new positions
defaultParams :: TokenType -> PositionParams
defaultParams pair =
  let fullRange = { lower: -887272, upper: 887272 }  -- Full range position
  in { amount: 1000.0
     , pair: pair
     , tickLower: fullRange.lower
     , tickUpper: fullRange.upper
     , duration: 0        -- Spot by default
     , leverageRate: 0.0  -- No leverage by default
     }

-- Create default params for a token ticker
defaultParamsForTicker :: String -> PositionParams
defaultParamsForTicker ticker = defaultParams (Token ticker)

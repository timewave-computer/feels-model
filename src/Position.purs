-- | Position Module - Core position types for the Feels Protocol
-- |
-- | This module implements the three-dimensional position model with:
-- | - Price Strategy: Band-aligned, tick-specific, or hybrid positioning
-- | - Term Commitment: Spot (perpetual), hourly, daily, or weekly terms
-- | - Leverage Config: Static or dynamic leverage with health-based adjustments
-- |
-- | Key features:
-- | - Adaptive bands that track market conditions
-- | - Synchronized global term expiries
-- | - Liquidation-free leverage through dynamic adjustment
-- | - "Everything is Lending" philosophy maintained
module Position
  ( Position
  , TokenPair
  , PriceStrategy(..)
  , TickRange
  , AdaptiveBand
  , BandTier(..)
  , TrackingMode(..)
  , TermCommitment(..)
  , LeverageConfig
  , LeverageMode(..)
  , HealthFactor
  , createPosition
  , getNextExpiry
  , isSpot
  , isTermPosition
  , isLeveraged
  , getBandWidth
  , priceToTick
  , tickToPrice
  , updateAdaptiveBand
  , createSpotPosition
  , createTermPosition
  , createLeveragedPosition
  , validatePosition
  , adjustLeverageForHealth
  , decayLeverageAfterExpiry
  , calculateEffectiveLeverage
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Token (TokenType)
import FFI (currentTime, log, exp, sqrt, unsafeToInt, unsafeToNumber)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- Token pair for a position
type TokenPair =
  { base :: TokenType
  , quote :: TokenType
  }

-- Core position structure
type Position =
  { -- Core parameters
    amount :: Number
  , tokenPair :: TokenPair
  
  -- Price dimension
  , priceStrategy :: PriceStrategy
  
  -- Time dimension  
  , term :: TermCommitment
  
  -- Leverage dimension
  , leverageConfig :: LeverageConfig
  
  -- Metadata
  , owner :: String
  , id :: Int
  , createdAt :: Number
  }

--------------------------------------------------------------------------------
-- Price Strategy Types
--------------------------------------------------------------------------------

-- Price positioning strategy
data PriceStrategy
  = BandAligned AdaptiveBand           -- Simplified, auto-adjusting
  | TickSpecific TickRange             -- Granular, fixed
  | BandConstrained AdaptiveBand TickRange  -- Hybrid approach

derive instance eqPriceStrategy :: Eq PriceStrategy

instance showPriceStrategy :: Show PriceStrategy where
  show (BandAligned band) = "BandAligned"
  show (TickSpecific range) = "TickSpecific"
  show (BandConstrained band range) = "BandConstrained"

-- Tick range for precise positioning
type TickRange =
  { lowerTick :: Int
  , upperTick :: Int
  }

-- Band tier for simplified positioning
data BandTier
  = TightBand    -- ±1%
  | MediumBand   -- ±5%
  | WideBand     -- ±10%

derive instance eqBandTier :: Eq BandTier
derive instance ordBandTier :: Ord BandTier

instance showBandTier :: Show BandTier where
  show TightBand = "Tight"
  show MediumBand = "Medium"
  show WideBand = "Wide"

-- Price tracking mode
data TrackingMode
  = TWAP         -- Time-weighted average
  | SpotPrice    -- Current spot price
  | Midpoint     -- Mid between bid/ask

derive instance eqTrackingMode :: Eq TrackingMode

instance showTrackingMode :: Show TrackingMode where
  show TWAP = "TWAP"
  show SpotPrice = "SpotPrice"
  show Midpoint = "Midpoint"

-- Adaptive band configuration
type AdaptiveBand =
  { tier :: BandTier                    -- Tight/Medium/Wide
  , centerTracking :: TrackingMode      -- TWAP/Spot/Midpoint
  , adaptiveWidth :: Boolean            -- Adjust to volatility
  , lastUpdate :: Number                -- Last update timestamp
  , cachedBounds :: Maybe TickRange     -- Cached tick bounds
  }

--------------------------------------------------------------------------------
-- Time Commitment Types
--------------------------------------------------------------------------------

-- Term commitment for positions
data TermCommitment
  = Spot                               -- No expiry (∞ duration)
  | Hourly Number                      -- Expires on the hour
  | Daily Number                       -- Expires at 00:00 UTC
  | Weekly Number                      -- Expires Sunday 00:00 UTC

derive instance eqTermCommitment :: Eq TermCommitment

instance showTermCommitment :: Show TermCommitment where
  show Spot = "Spot"
  show (Hourly expiry) = "Hourly (expires " <> show expiry <> ")"
  show (Daily expiry) = "Daily (expires " <> show expiry <> ")"
  show (Weekly expiry) = "Weekly (expires " <> show expiry <> ")"

--------------------------------------------------------------------------------
-- Leverage Types
--------------------------------------------------------------------------------

-- Health factor for dynamic leverage
type HealthFactor = Number

-- Leverage mode
data LeverageMode
  = Static                             -- Fixed leverage
  | Dynamic HealthFactor               -- Adjusts with pool health

derive instance eqLeverageMode :: Eq LeverageMode

instance showLeverageMode :: Show LeverageMode where
  show Static = "Static"
  show (Dynamic factor) = "Dynamic (health: " <> show factor <> ")"

-- Leverage configuration
type LeverageConfig =
  { targetLeverage :: Number            -- Desired leverage (1.0 - 10.0)
  , currentLeverage :: Number           -- Current effective leverage
  , mode :: LeverageMode
  , decayAfterTerm :: Boolean           -- Whether to decay post-expiry
  }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Create a new position
createPosition :: 
  Int -> 
  String -> 
  Number -> 
  TokenPair -> 
  PriceStrategy -> 
  TermCommitment -> 
  LeverageConfig -> 
  Number ->
  Position
createPosition id owner amount tokenPair priceStrategy term leverageConfig timestamp =
  { id
  , owner
  , amount
  , tokenPair
  , priceStrategy
  , term
  , leverageConfig
  , createdAt: timestamp
  }

-- Get next expiry timestamp for a term type
getNextExpiry :: TermCommitment -> Number -> Number
getNextExpiry term currentTime =
  case term of
    Spot -> 9999999999999.0           -- MaxTimestamp (never expires)
    Hourly _ -> nextHourBoundary currentTime
    Daily _ -> nextMidnightUTC currentTime  
    Weekly _ -> nextSundayMidnightUTC currentTime

-- Check if position is spot (perpetual)
isSpot :: Position -> Boolean
isSpot pos = case pos.term of
  Spot -> true
  _ -> false

-- Check if position has term commitment
isTermPosition :: Position -> Boolean
isTermPosition pos = not (isSpot pos)

-- Check if position is leveraged
isLeveraged :: Position -> Boolean
isLeveraged pos = pos.leverageConfig.targetLeverage > 1.0

--------------------------------------------------------------------------------
-- Time Boundary Calculations
--------------------------------------------------------------------------------

-- Get next hour boundary
nextHourBoundary :: Number -> Number
nextHourBoundary currentTime =
  let hourMs = 3600000.0  -- 60 * 60 * 1000
      currentHour = floor (currentTime / hourMs)
  in (currentHour + 1.0) * hourMs

-- Get next midnight UTC
nextMidnightUTC :: Number -> Number
nextMidnightUTC currentTime =
  let dayMs = 86400000.0  -- 24 * 60 * 60 * 1000
      currentDay = floor (currentTime / dayMs)
  in (currentDay + 1.0) * dayMs

-- Get next Sunday midnight UTC
nextSundayMidnightUTC :: Number -> Number
nextSundayMidnightUTC currentTime =
  let weekMs = 604800000.0  -- 7 * 24 * 60 * 60 * 1000
      -- Unix epoch was Thursday, so we need to adjust
      -- to get weeks starting on Sunday
      adjustedTime = currentTime + 345600000.0  -- 4 days in ms
      currentWeek = floor (adjustedTime / weekMs)
      nextSundayAdjusted = (currentWeek + 1.0) * weekMs
  in nextSundayAdjusted - 345600000.0

-- Helper function for floor
floor :: Number -> Number
floor x = if x >= 0.0 
          then floorPositive x
          else -floorPositive (-x)
  where
    floorPositive n = 
      let intPart = toInt n
      in toNumber intPart
    
    toInt :: Number -> Int
    toInt n = unsafeToInt n
    
    toNumber :: Int -> Number
    toNumber i = unsafeToNumber i

--------------------------------------------------------------------------------
-- Band and Tick Calculations
--------------------------------------------------------------------------------

-- Get band width for a tier
getBandWidth :: BandTier -> Number
getBandWidth tier = case tier of
  TightBand -> 0.01    -- ±1%
  MediumBand -> 0.05   -- ±5%
  WideBand -> 0.10     -- ±10%

-- Convert price to tick (basis point precision)
-- Tick 0 = 1.0, Tick 100 = 1.01, Tick -100 = 0.99
priceToTick :: Number -> Int
priceToTick price = 
  let logPrice = log price
      -- Each tick represents 1 basis point (0.01%)
      tickSpacing = 0.0001  -- log(1.0001) ≈ 0.0001
  in unsafeToInt (logPrice / tickSpacing)

-- Convert tick to price
tickToPrice :: Int -> Number
tickToPrice tick =
  let tickSpacing = 0.0001
  in exp (unsafeToNumber tick * tickSpacing)

-- Update adaptive band based on market conditions
updateAdaptiveBand :: 
  { currentPrice :: Number
  , volatility :: Number
  , baselineVol :: Number
  , stressMultiplier :: Number  -- Added for stress-based widening
  } -> 
  AdaptiveBand -> 
  AdaptiveBand
updateAdaptiveBand params band =
  let baseWidth = getBandWidth band.tier
      
      -- Adjust for volatility if enabled
      volatilityAdjustment = if band.adaptiveWidth
        then sqrt (params.volatility / params.baselineVol)
        else 1.0
      
      -- Apply both volatility and stress adjustments
      effectiveWidth = baseWidth * volatilityAdjustment * params.stressMultiplier
      
      -- Calculate new tick bounds
      lowerPrice = params.currentPrice * (1.0 - effectiveWidth)
      upperPrice = params.currentPrice * (1.0 + effectiveWidth)
      
      newBounds = 
        { lowerTick: priceToTick lowerPrice
        , upperTick: priceToTick upperPrice
        }
      
  in band 
    { cachedBounds = Just newBounds
    , lastUpdate = 0.0  -- Would be set by caller with current time
    }

--------------------------------------------------------------------------------
-- Math Functions
--------------------------------------------------------------------------------
-- Position Creation Helpers
--------------------------------------------------------------------------------

-- Create a spot trading position (price only)
createSpotPosition ::
  Int ->
  String ->
  Number ->
  TokenPair ->
  BandTier ->
  Number ->
  Position
createSpotPosition id owner amount tokenPair bandTier timestamp =
  let band = 
        { tier: bandTier
        , centerTracking: SpotPrice
        , adaptiveWidth: true
        , lastUpdate: timestamp
        , cachedBounds: Nothing
        }
      leverage = 
        { targetLeverage: 1.0
        , currentLeverage: 1.0
        , mode: Static
        , decayAfterTerm: false
        }
  in createPosition id owner amount tokenPair (BandAligned band) Spot leverage timestamp

-- Create a term lending position (time only)
createTermPosition ::
  Int ->
  String ->
  Number ->
  TokenPair ->
  TermCommitment ->
  Number ->
  Position
createTermPosition id owner amount tokenPair term timestamp =
  let band = 
        { tier: MediumBand  -- Default to medium band for term positions
        , centerTracking: TWAP
        , adaptiveWidth: true
        , lastUpdate: timestamp
        , cachedBounds: Nothing
        }
      leverage = 
        { targetLeverage: 1.0
        , currentLeverage: 1.0
        , mode: Static
        , decayAfterTerm: false
        }
  in createPosition id owner amount tokenPair (BandAligned band) term leverage timestamp

-- Create a leveraged position (leverage only)
createLeveragedPosition ::
  Int ->
  String ->
  Number ->
  TokenPair ->
  Number ->
  Boolean ->
  Number ->
  Position
createLeveragedPosition id owner amount tokenPair targetLeverage useDynamicMode timestamp =
  let band = 
        { tier: WideBand  -- Wide bands for leveraged positions
        , centerTracking: SpotPrice
        , adaptiveWidth: true
        , lastUpdate: timestamp
        , cachedBounds: Nothing
        }
      leverage = 
        { targetLeverage: targetLeverage
        , currentLeverage: targetLeverage
        , mode: if useDynamicMode then Dynamic 1.0 else Static
        , decayAfterTerm: true
        }
  in createPosition id owner amount tokenPair (BandAligned band) Spot leverage timestamp

-- Validate position parameters
validatePosition :: Position -> Either String Position
validatePosition pos =
  if pos.amount <= 0.0
    then Left "Position amount must be positive"
    else if pos.leverageConfig.targetLeverage < 1.0
    then Left "Leverage must be at least 1.0"
    else if pos.leverageConfig.targetLeverage > 10.0
    then Left "Leverage cannot exceed 10.0"
    else Right pos

--------------------------------------------------------------------------------
-- Leverage Management
--------------------------------------------------------------------------------

-- Adjust leverage based on pool health
adjustLeverageForHealth :: HealthFactor -> Position -> Position
adjustLeverageForHealth health pos =
  case pos.leverageConfig.mode of
    Static -> pos  -- No adjustment for static leverage
    Dynamic _ ->
      let -- Stress adjustment: reduce leverage when health < 1.2
          stressAdjustment = max 0.5 (min 1.0 health)
          effectiveLeverage = pos.leverageConfig.targetLeverage * stressAdjustment
      in pos { leverageConfig = pos.leverageConfig 
        { currentLeverage = effectiveLeverage
        , mode = Dynamic health
        }}

-- Decay leverage after term expiry
decayLeverageAfterExpiry :: Position -> Position
decayLeverageAfterExpiry pos =
  if pos.leverageConfig.decayAfterTerm && isSpot pos
  then 
    -- Gradually reduce leverage to 1.0
    let decayedLeverage = max 1.0 (pos.leverageConfig.currentLeverage * 0.95)
    in pos { leverageConfig = pos.leverageConfig 
      { currentLeverage = decayedLeverage }}
  else pos

-- Calculate effective leverage considering all factors
calculateEffectiveLeverage :: Position -> Number
calculateEffectiveLeverage pos = pos.leverageConfig.currentLeverage

-- Calculate pool health factor
calculatePoolHealth :: 
  { totalValue :: Number
  , obligations :: Number
  } -> HealthFactor
calculatePoolHealth params =
  if params.obligations > 0.0
  then params.totalValue / params.obligations
  else 2.0  -- Maximum health when no obligations

--------------------------------------------------------------------------------
-- Foreign Imports
--------------------------------------------------------------------------------

-- Number conversion functions now imported from FFI
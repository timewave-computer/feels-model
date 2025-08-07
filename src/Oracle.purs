module Oracle
  ( Oracle
  , OracleState
  , BandPricing
  , TWAPWindow(..)
  , VolatilityMetrics
  , PriceUpdate
  , PriceObservation
  , MarketMetrics
  , initOracle
  , updatePrice
  , getBandPrice
  , getTWAP
  , getVolatility
  , getBaselineVolatility
  , setPriceTrackingFrequency
  , takeMarketSnapshot
  , observeMarket
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:), take, drop, filter, length, foldl, sortBy, find, uncons, reverse, zip)
import Data.Number (sqrt, abs)
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Data.Ord (max)
import Data.Int as Int
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Utils (standardDeviation)
import Token (TokenType(..))
import Position (TrackingMode(..), BandTier(..))
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Oracle Types
--------------------------------------------------------------------------------

-- Time window for TWAP calculation
data TWAPWindow
  = FiveMinutes
  | FifteenMinutes
  | OneHour
  | FourHours

derive instance eqTWAPWindow :: Eq TWAPWindow

instance showTWAPWindow :: Show TWAPWindow where
  show FiveMinutes = "5m"
  show FifteenMinutes = "15m"
  show OneHour = "1h"
  show FourHours = "4h"

-- Price update from external source
type PriceUpdate =
  { tokenPair :: { base :: TokenType, quote :: TokenType }
  , price :: Number
  , volume :: Number
  , timestamp :: Number
  , source :: String  -- "market", "pol", "aggregate"
  }

-- Price observation for analysis
type PriceObservation = PriceUpdate

-- Band-specific pricing
type BandPricing =
  { tier :: BandTier
  , centerPrice :: Number      -- Current center price
  , twapPrice :: Number        -- TWAP for the band
  , spotPrice :: Number        -- Latest spot price
  , midpointPrice :: Number    -- Bid/ask midpoint
  , lastUpdate :: Number
  }

-- Volatility metrics
type VolatilityMetrics =
  { currentVolatility :: Number     -- Current volatility
  , baselineVolatility :: Number    -- Long-term baseline
  , rollingVolatility :: Number     -- Rolling window volatility
  , volatilityTrend :: Number       -- Rate of change
  , lastCalculation :: Number
  }

-- Oracle state
type OracleState =
  { priceHistory :: Array PriceUpdate        -- All price updates
  , bandPricing :: Array BandPricing         -- Per-band pricing
  , volatilityMetrics :: VolatilityMetrics   -- Volatility tracking
  , updateFrequency :: Number                -- Min ms between updates
  , lastUpdate :: Number
  , twapWindows :: Array TWAPWindow         -- Active TWAP windows
  }

type Oracle = Ref OracleState

-- Market metrics for incentives
type MarketMetrics =
  { currentPrice :: Number
  , volatility :: Number
  , volume24h :: Number
  , priceChange24h :: Number
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize oracle
initOracle :: Effect Oracle
initOracle = do
  now <- currentTime
  let initialVolatility =
        { currentVolatility: 0.01      -- 1% baseline
        , baselineVolatility: 0.01
        , rollingVolatility: 0.01
        , volatilityTrend: 0.0
        , lastCalculation: now
        }
      
      initialState =
        { priceHistory: []
        , bandPricing: []
        , volatilityMetrics: initialVolatility
        , updateFrequency: 1000.0      -- 1 second minimum
        , lastUpdate: now
        , twapWindows: [FiveMinutes, FifteenMinutes, OneHour]
        }
  
  new initialState

--------------------------------------------------------------------------------
-- Price Updates
--------------------------------------------------------------------------------

-- Update price from market
updatePrice :: PriceUpdate -> Oracle -> Effect Unit
updatePrice update oracleRef = do
  now <- currentTime
  state <- read oracleRef
  
  -- Check update frequency
  if now - state.lastUpdate < state.updateFrequency
    then pure unit
    else do
      -- Add to history (keep last 10000 updates)
      let newHistory = take 10000 (update : state.priceHistory)
      
      -- Recalculate volatility
      let newVolatility = calculateVolatilityMetrics newHistory state.volatilityMetrics
      
      -- Update band pricing
      let newBandPricing = updateBandPricing update newHistory newVolatility
      
      modify_ (\s -> s 
        { priceHistory = newHistory
        , bandPricing = newBandPricing
        , volatilityMetrics = newVolatility
        , lastUpdate = now
        }) oracleRef

--------------------------------------------------------------------------------
-- Band Pricing
--------------------------------------------------------------------------------

-- Get price for specific band and tracking mode
getBandPrice :: 
  { tokenPair :: { base :: TokenType, quote :: TokenType }
  , bandTier :: BandTier
  , trackingMode :: TrackingMode
  } -> Oracle -> Effect (Maybe Number)
getBandPrice params oracleRef = do
  state <- read oracleRef
  let bandPrice = find (matchesBand params.bandTier) state.bandPricing
  
  pure $ bandPrice >>= \bp ->
    case params.trackingMode of
      TWAP -> Just bp.twapPrice
      SpotPrice -> Just bp.spotPrice
      Midpoint -> Just bp.midpointPrice
  where
    matchesBand tier bp = bp.tier == tier

-- Update band pricing based on new price
updateBandPricing :: PriceUpdate -> Array PriceUpdate -> VolatilityMetrics -> Array BandPricing
updateBandPricing latest history volatility =
  -- For now, all bands use same price (could differentiate later)
  let spot = latest.price
      twap5m = calculateTWAP FiveMinutes history
      twap15m = calculateTWAP FifteenMinutes history
      twap1h = calculateTWAP OneHour history
      
      -- Use different TWAPs for different tracking modes
      createBandPrice tier =
        { tier
        , centerPrice: case tier of
            -- Tight bands track spot closely
            _ -> spot  -- Simplified for now
        , twapPrice: twap15m  -- 15m TWAP as default
        , spotPrice: spot
        , midpointPrice: spot  -- Would calculate from bid/ask
        , lastUpdate: latest.timestamp
        }
  
  in map createBandPrice [TightBand, MediumBand, WideBand]

--------------------------------------------------------------------------------
-- TWAP Calculation
--------------------------------------------------------------------------------

-- Get TWAP for a window
getTWAP :: 
  { tokenPair :: { base :: TokenType, quote :: TokenType }
  , window :: TWAPWindow
  } -> Oracle -> Effect Number
getTWAP params oracleRef = do
  state <- read oracleRef
  let relevantHistory = filter (matchesPair params.tokenPair) state.priceHistory
  pure $ calculateTWAP params.window relevantHistory
  where
    matchesPair pair update = 
      update.tokenPair.base == pair.base && 
      update.tokenPair.quote == pair.quote

-- Calculate TWAP from price history
calculateTWAP :: TWAPWindow -> Array PriceUpdate -> Number
calculateTWAP window history =
  if length history == 0
    then 0.0
    else
      let windowMs = twapWindowToMs window
          now = case uncons history of
            Just { head } -> head.timestamp
            Nothing -> 0.0
          cutoff = now - windowMs
          
          -- Filter to window and sort by time
          windowPrices = sortBy compareTime $ 
            filter (\p -> p.timestamp >= cutoff) history
          
          -- Calculate time-weighted average
          twap = calculateTimeWeighted windowPrices
      in twap
  where
    compareTime p1 p2 = compare p1.timestamp p2.timestamp

-- Convert TWAP window to milliseconds
twapWindowToMs :: TWAPWindow -> Number
twapWindowToMs window = case window of
  FiveMinutes -> 300000.0      -- 5 * 60 * 1000
  FifteenMinutes -> 900000.0   -- 15 * 60 * 1000
  OneHour -> 3600000.0         -- 60 * 60 * 1000
  FourHours -> 14400000.0      -- 4 * 60 * 60 * 1000

-- Calculate time-weighted average
calculateTimeWeighted :: Array PriceUpdate -> Number
calculateTimeWeighted prices =
  case uncons prices of
    Nothing -> 0.0
    Just { head: first, tail: [] } -> first.price
    Just { head: _, tail: rest } ->
      let pairs = zip prices rest
          weightedSum = foldl addWeighted 0.0 pairs
          totalTime = case Tuple (uncons prices) (uncons (reverse prices)) of
            Tuple (Just { head: first }) (Just { head: last }) ->
              last.timestamp - first.timestamp
            _ -> 1.0  -- Avoid division by zero
      in if totalTime > 0.0 
         then weightedSum / totalTime
         else 0.0
  where
    addWeighted acc (Tuple p1 p2) =
      let timeDiff = p2.timestamp - p1.timestamp
          avgPrice = (p1.price + p2.price) / 2.0
      in acc + (avgPrice * timeDiff)

--------------------------------------------------------------------------------
-- Volatility Calculation
--------------------------------------------------------------------------------

-- Get current volatility
getVolatility :: Oracle -> Effect Number
getVolatility oracleRef = do
  state <- read oracleRef
  pure state.volatilityMetrics.currentVolatility

-- Get baseline volatility
getBaselineVolatility :: Oracle -> Effect Number
getBaselineVolatility oracleRef = do
  state <- read oracleRef
  pure state.volatilityMetrics.baselineVolatility

-- Calculate volatility metrics from price history
calculateVolatilityMetrics :: Array PriceUpdate -> VolatilityMetrics -> VolatilityMetrics
calculateVolatilityMetrics history prevMetrics =
  if length history < 2
    then prevMetrics
    else
      let -- Calculate returns
          prices = map _.price (take 100 history)  -- Use last 100 prices
          returns = calculateReturns prices
          
          -- Current volatility (standard deviation of returns)
          currentVol = if length returns > 0
            then standardDeviation returns
            else prevMetrics.currentVolatility
          
          -- Rolling volatility (exponential moving average)
          alpha = 0.1  -- Smoothing factor
          rollingVol = alpha * currentVol + (1.0 - alpha) * prevMetrics.rollingVolatility
          
          -- Update baseline (very slow moving average)
          baselineAlpha = 0.01
          newBaseline = baselineAlpha * currentVol + (1.0 - baselineAlpha) * prevMetrics.baselineVolatility
          
          -- Volatility trend
          trend = (currentVol - prevMetrics.currentVolatility) / max 0.0001 prevMetrics.currentVolatility
          
      in { currentVolatility: currentVol
         , baselineVolatility: newBaseline
         , rollingVolatility: rollingVol
         , volatilityTrend: trend
         , lastCalculation: 0.0  -- Would be set by caller
         }

-- Calculate returns from prices
calculateReturns :: Array Number -> Array Number
calculateReturns prices =
  case uncons prices of
    Nothing -> []
    Just { tail: [] } -> []
    Just { head: _, tail: rest } ->
      let pairs = zip prices rest
          returns = map calculateReturn pairs
      in returns
  where
    calculateReturn (Tuple p1 p2) = 
      if p1 > 0.0 
      then (p2 - p1) / p1
      else 0.0


--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- Set price tracking update frequency
setPriceTrackingFrequency :: Number -> Oracle -> Effect Unit
setPriceTrackingFrequency frequencyMs oracleRef =
  modify_ (\s -> s { updateFrequency = max 100.0 frequencyMs }) oracleRef

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Take a market snapshot for analysis
takeMarketSnapshot :: Oracle -> Effect (Array PriceObservation)
takeMarketSnapshot oracleRef = do
  state <- read oracleRef
  pure state.priceHistory

-- Observe market metrics for incentives
observeMarket :: Oracle -> Effect MarketMetrics
observeMarket oracleRef = do
  state <- read oracleRef
  let latestPrice = case state.priceHistory of
        [] -> 1.0
        prices -> (head prices).price
      volume24h = sum $ map _.volume $ take 288 state.priceHistory  -- ~24h at 5min intervals
      priceChange24h = case state.priceHistory of
        [] -> 0.0
        prices -> 
          let dayAgoPrice = fromMaybe latestPrice $ map _.price $ find (\p -> p.timestamp < (head prices).timestamp - 86400000.0) prices
          in (latestPrice - dayAgoPrice) / dayAgoPrice
  
  pure { currentPrice: latestPrice
       , volatility: state.volatilityMetrics.currentVolatility
       , volume24h: volume24h
       , priceChange24h: priceChange24h
       }
  where
    head prices = case uncons prices of
      Nothing -> { price: 1.0, timestamp: 0.0, volume: 0.0, source: "", tokenPair: { base: FeelsSOL, quote: JitoSOL } }
      Just { head: x } -> x
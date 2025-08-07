module UI.PositionDisplay
  ( renderBandRanges
  , renderTermCountdown
  , renderEffectiveLeverage
  , renderPoolHealth
  , formatTimeRemaining
  , formatPriceRange
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Position
import RiskManagement (StressLevel(..), HealthFactor)
import Token (TokenType)
import FFI (currentTime, unsafeToInt)

--------------------------------------------------------------------------------
-- Band Range Display
--------------------------------------------------------------------------------

-- Show current band ranges and centers
renderBandRanges :: 
  { bandTier :: BandTier
  , centerPrice :: Number
  , lowerPrice :: Number
  , upperPrice :: Number
  , lastUpdate :: Number
  } -> String
renderBandRanges params =
  """
  <div class="band-ranges">
    <h3>""" <> show params.bandTier <> """ Band</h3>
    
    <div class="price-range">
      <div class="range-item">
        <span class="label">Lower:</span>
        <span class="value">""" <> formatPrice params.lowerPrice <> """</span>
      </div>
      
      <div class="range-item center">
        <span class="label">Center:</span>
        <span class="value">""" <> formatPrice params.centerPrice <> """</span>
      </div>
      
      <div class="range-item">
        <span class="label">Upper:</span>
        <span class="value">""" <> formatPrice params.upperPrice <> """</span>
      </div>
    </div>
    
    <div class="band-width">
      Width: """ <> formatPriceRange params.lowerPrice params.upperPrice params.centerPrice <> """
    </div>
    
    <div class="last-update">
      Updated: """ <> formatTimestamp params.lastUpdate <> """
    </div>
  </div>
  """

--------------------------------------------------------------------------------
-- Term Expiry Display
--------------------------------------------------------------------------------

-- Display term expiry countdown
renderTermCountdown :: Position -> Number -> String
renderTermCountdown position currentTime =
  case position.term of
    Spot -> 
      """<div class="term-display spot">
        <span class="term-type">Perpetual</span>
        <span class="no-expiry">No expiry</span>
      </div>"""
    
    term ->
      let expiry = getExpiryTime term
          remaining = expiry - currentTime
          Tuple timeStr urgency = formatTimeRemaining remaining
      in """
      <div class="term-display """ <> urgency <> """">
        <span class="term-type">""" <> formatTermType term <> """</span>
        <span class="countdown">""" <> timeStr <> """</span>
        <div class="expiry-time">
          Expires: """ <> formatTimestamp expiry <> """
        </div>
      </div>
      """

-- Get expiry time from term
getExpiryTime :: TermCommitment -> Number
getExpiryTime term = case term of
  Spot -> 9999999999999.0
  Hourly t -> t
  Daily t -> t
  Weekly t -> t

-- Format term type
formatTermType :: TermCommitment -> String
formatTermType term = case term of
  Spot -> "Spot"
  Hourly _ -> "Hourly Term"
  Daily _ -> "Daily Term"
  Weekly _ -> "Weekly Term"

--------------------------------------------------------------------------------
-- Leverage Display
--------------------------------------------------------------------------------

-- Present effective leverage after adjustments
renderEffectiveLeverage :: Position -> HealthFactor -> String
renderEffectiveLeverage position health =
  let config = position.leverageConfig
      effectiveLev = case config.mode of
        Static -> config.currentLeverage
        Dynamic _ -> adjustLeverageForHealth health position # _.leverageConfig.currentLeverage
      
      leverageClass = if effectiveLev < config.targetLeverage
                      then "reduced"
                      else "normal"
  in """
  <div class="leverage-display """ <> leverageClass <> """">
    <h3>Leverage</h3>
    
    <div class="leverage-metrics">
      <div class="metric">
        <span class="label">Target:</span>
        <span class="value">""" <> formatLeverage config.targetLeverage <> """</span>
      </div>
      
      <div class="metric effective">
        <span class="label">Effective:</span>
        <span class="value">""" <> formatLeverage effectiveLev <> """</span>
      </div>
      
      """ <> if config.mode /= Static
             then renderDynamicInfo health
             else "" <> """
    </div>
    
    """ <> if effectiveLev < config.targetLeverage
           then renderLeverageReduction config.targetLeverage effectiveLev
           else "" <> """
  </div>
  """

-- Render dynamic leverage info
renderDynamicInfo :: HealthFactor -> String
renderDynamicInfo health =
  """
  <div class="dynamic-info">
    <span class="mode-badge">Dynamic Mode</span>
    <span class="health">Health: """ <> formatHealth health <> """</span>
  </div>
  """

-- Render leverage reduction notice
renderLeverageReduction :: Number -> Number -> String
renderLeverageReduction target effective =
  let reduction = ((target - effective) / target) * 100.0
  in """
  <div class="leverage-warning">
    <strong>Leverage reduced """ <> formatPercent reduction <> """</strong>
    due to pool health conditions
  </div>
  """

--------------------------------------------------------------------------------
-- Pool Health Display
--------------------------------------------------------------------------------

-- Add pool health indicators
renderPoolHealth :: 
  { healthFactor :: HealthFactor
  , stressLevel :: StressLevel
  , totalValue :: Number
  , obligations :: Number
  } -> String
renderPoolHealth params =
  let healthClass = getHealthClass params.stressLevel
      healthIcon = getHealthIcon params.stressLevel
  in """
  <div class="pool-health """ <> healthClass <> """">
    <h3>Pool Health</h3>
    
    <div class="health-indicator">
      <span class="icon">""" <> healthIcon <> """</span>
      <span class="factor">""" <> formatHealth params.healthFactor <> """</span>
      <span class="level">""" <> show params.stressLevel <> """</span>
    </div>
    
    <div class="health-metrics">
      <div class="metric">
        <span class="label">Total Value:</span>
        <span class="value">""" <> formatAmount params.totalValue <> """</span>
      </div>
      
      <div class="metric">
        <span class="label">Obligations:</span>
        <span class="value">""" <> formatAmount params.obligations <> """</span>
      </div>
      
      <div class="metric">
        <span class="label">Coverage:</span>
        <span class="value">""" <> formatPercent (params.healthFactor * 100.0 - 100.0) <> """</span>
      </div>
    </div>
    
    """ <> renderStressEffects params.stressLevel <> """
  </div>
  """

-- Get health class for styling
getHealthClass :: StressLevel -> String
getHealthClass level = case level of
  Normal -> "healthy"
  Mild -> "mild-stress"
  Moderate -> "moderate-stress"
  Severe -> "severe-stress"
  Critical -> "critical-stress"

-- Get health icon
getHealthIcon :: StressLevel -> String
getHealthIcon level = case level of
  Normal -> "✓"
  Mild -> "!"
  Moderate -> "⚠"
  Severe -> "⚠⚠"
  Critical -> "🚨"

-- Render stress effects
renderStressEffects :: StressLevel -> String
renderStressEffects level = case level of
  Normal -> ""
  _ -> """
    <div class="stress-effects">
      <h4>Active Adjustments:</h4>
      <ul>
        """ <> renderStressAdjustments level <> """
      </ul>
    </div>
    """

-- Render stress adjustments list
renderStressAdjustments :: StressLevel -> String
renderStressAdjustments level = case level of
  Normal -> ""
  Mild -> "<li>Bands widened 20%</li><li>Max leverage reduced 10%</li>"
  Moderate -> "<li>Bands widened 50%</li><li>Max leverage reduced 25%</li><li>Fees increased 25%</li>"
  Severe -> "<li>Bands widened 100%</li><li>Max leverage reduced 50%</li><li>Fees increased 50%</li>"
  Critical -> "<li>Bands widened 200%</li><li>Max leverage reduced 80%</li><li>Fees doubled</li>"

--------------------------------------------------------------------------------
-- Formatting Helpers
--------------------------------------------------------------------------------

-- Format time remaining
formatTimeRemaining :: Number -> Tuple String String
formatTimeRemaining ms =
  if ms <= 0.0
    then Tuple "Expired" "expired"
  else if ms < 60000.0  -- Less than 1 minute
    then Tuple (show (floor (ms / 1000.0)) <> "s") "urgent"
  else if ms < 3600000.0  -- Less than 1 hour
    then Tuple (show (floor (ms / 60000.0)) <> "m") "soon"
  else if ms < 86400000.0  -- Less than 1 day
    then Tuple (show (floor (ms / 3600000.0)) <> "h") "normal"
  else
    Tuple (show (floor (ms / 86400000.0)) <> "d") "normal"

-- Format price range as percentage
formatPriceRange :: Number -> Number -> Number -> String
formatPriceRange lower upper center =
  let lowerPct = ((center - lower) / center) * 100.0
      upperPct = ((upper - center) / center) * 100.0
  in "±" <> formatPercent (max lowerPct upperPct)

-- Format price
formatPrice :: Number -> String
formatPrice price = "$" <> toStringWith (precision 4) price

-- Format leverage
formatLeverage :: Number -> String
formatLeverage lev = toStringWith (precision 1) lev <> "x"

-- Format health factor
formatHealth :: HealthFactor -> String
formatHealth health = toStringWith (precision 2) health

-- Format percentage
formatPercent :: Number -> String
formatPercent pct = toStringWith (precision 1) pct <> "%"

-- Format amount
formatAmount :: Number -> String
formatAmount amt = 
  if amt >= 1000000.0
  then "$" <> toStringWith (precision 1) (amt / 1000000.0) <> "M"
  else if amt >= 1000.0
  then "$" <> toStringWith (precision 1) (amt / 1000.0) <> "K"
  else "$" <> toStringWith (precision 0) amt

-- Format timestamp
formatTimestamp :: Number -> String
formatTimestamp ts = "Just now"  -- Simplified for example

-- Helpers
floor :: Number -> Int
floor = unsafeToInt

toStringWith :: { precision :: Int } -> Number -> String
toStringWith opts n = show n  -- Simplified

precision :: Int -> { precision :: Int }
precision p = { precision: p }

-- Number conversion function imported from FFI
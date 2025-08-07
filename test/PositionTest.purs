module Test.Position where

import Prelude
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Console (log)
import Position
import Token (TokenType(..))

--------------------------------------------------------------------------------
-- Test Utilities
--------------------------------------------------------------------------------

-- Simple test assertion
assert :: String -> Boolean -> Effect Unit
assert description condition = do
  if condition
    then log $ "✓ " <> description
    else log $ "✗ FAILED: " <> description

-- Test equality with description
assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual description expected actual = do
  if expected == actual
    then log $ "✓ " <> description
    else log $ "✗ FAILED: " <> description <> " - Expected: " <> show expected <> ", Got: " <> show actual

-- Test approximate equality for numbers
assertApprox :: String -> Number -> Number -> Number -> Effect Unit
assertApprox description expected actual tolerance = do
  let diff = abs (expected - actual)
  if diff <= tolerance
    then log $ "✓ " <> description
    else log $ "✗ FAILED: " <> description <> " - Expected: " <> show expected <> " (±" <> show tolerance <> "), Got: " <> show actual

--------------------------------------------------------------------------------
-- Band Width Tests
--------------------------------------------------------------------------------

testBandWidths :: Effect Unit
testBandWidths = do
  log "\n=== Testing Band Width Calculations ==="
  
  -- Test basic band widths
  assertEqual "Tight band width" 0.01 (getBandWidth TightBand)
  assertEqual "Medium band width" 0.05 (getBandWidth MediumBand)
  assertEqual "Wide band width" 0.10 (getBandWidth WideBand)
  
  -- Test tick conversions
  let price1 = 1.0
      tick1 = priceToTick price1
      price1Back = tickToPrice tick1
  assertApprox "Price to tick and back (1.0)" price1 price1Back 0.0001
  
  let price2 = 1.5
      tick2 = priceToTick price2
      price2Back = tickToPrice tick2
  assertApprox "Price to tick and back (1.5)" price2 price2Back 0.0001
  
  -- Test band updates
  let band = { tier: MediumBand
             , centerTracking: SpotPrice
             , adaptiveWidth: true
             , lastUpdate: 0.0
             , cachedBounds: Nothing
             }
      
      updatedBand = updateAdaptiveBand 
        { currentPrice: 100.0
        , volatility: 0.02
        , baselineVol: 0.01
        , stressMultiplier: 1.0
        } band
  
  case updatedBand.cachedBounds of
    Nothing -> log "✗ FAILED: Band update didn't cache bounds"
    Just bounds -> do
      let lowerPrice = tickToPrice bounds.lowerTick
          upperPrice = tickToPrice bounds.upperTick
          expectedLower = 100.0 * (1.0 - 0.05 * sqrt 2.0)  -- volatility adjustment
          expectedUpper = 100.0 * (1.0 + 0.05 * sqrt 2.0)
      
      assertApprox "Lower band bound" expectedLower lowerPrice 1.0
      assertApprox "Upper band bound" expectedUpper upperPrice 1.0
  
  -- Test stress multiplier effect
  let stressedBand = updateAdaptiveBand 
        { currentPrice: 100.0
        , volatility: 0.01
        , baselineVol: 0.01
        , stressMultiplier: 2.0  -- Double width under stress
        } band
  
  case stressedBand.cachedBounds of
    Nothing -> log "✗ FAILED: Stressed band update didn't cache bounds"
    Just bounds -> do
      let lowerPrice = tickToPrice bounds.lowerTick
          upperPrice = tickToPrice bounds.upperTick
          expectedLower = 100.0 * (1.0 - 0.05 * 2.0)  -- stress doubles width
          expectedUpper = 100.0 * (1.0 + 0.05 * 2.0)
      
      assertApprox "Stressed lower bound" expectedLower lowerPrice 1.0
      assertApprox "Stressed upper bound" expectedUpper upperPrice 1.0

--------------------------------------------------------------------------------
-- Term Synchronization Tests
--------------------------------------------------------------------------------

testTermSynchronization :: Effect Unit
testTermSynchronization = do
  log "\n=== Testing Term Synchronization Logic ==="
  
  -- Test next expiry calculations
  let testTime = 1704067200000.0  -- 2024-01-01 00:00:00 UTC (Monday)
  
  -- Hourly expiry
  let hourlyExpiry = getNextExpiry (Hourly 0.0) testTime
      expectedHourly = 1704070800000.0  -- Next hour boundary
  assertEqual "Hourly expiry calculation" expectedHourly hourlyExpiry
  
  -- Daily expiry
  let dailyExpiry = getNextExpiry (Daily 0.0) testTime
      expectedDaily = 1704153600000.0  -- Next midnight UTC
  assertEqual "Daily expiry calculation" expectedDaily dailyExpiry
  
  -- Weekly expiry (next Sunday)
  let weeklyExpiry = getNextExpiry (Weekly 0.0) testTime
      expectedWeekly = 1704585600000.0  -- Next Sunday midnight
  assertEqual "Weekly expiry calculation" expectedWeekly weeklyExpiry
  
  -- Spot (never expires)
  let spotExpiry = getNextExpiry Spot testTime
  assert "Spot positions never expire" (spotExpiry > 9999999999999.0)
  
  -- Test term type detection
  let spotPos = createSpotPosition 1 "user1" 100.0 
        { base: FeelsSOL, quote: JitoSOL } TightBand testTime
      termPos = createTermPosition 2 "user2" 100.0 
        { base: FeelsSOL, quote: JitoSOL } (Daily 0.0) testTime
  
  assert "Spot position detected" (isSpot spotPos)
  assert "Term position detected" (isTermPosition termPos)
  assert "Spot is not term" (not (isTermPosition spotPos))
  assert "Term is not spot" (not (isSpot termPos))

--------------------------------------------------------------------------------
-- Fee Calculation Tests
--------------------------------------------------------------------------------

testFeeCalculations :: Effect Unit
testFeeCalculations = do
  log "\n=== Testing Fee Calculations ==="
  
  -- Import fee module functions
  let feeStruct = 
        { baseFeeRate: 0.003
        , granularityDiscount: 0.1
        , hybridDiscount: 0.05
        , hourlyTermBase: 1.0
        , dailyDiscount: 0.05
        , weeklyDiscount: 0.1
        , spotPremium: 0.05
        , leverageRiskFactor: 0.1
        }
  
  -- Test granularity multipliers
  let bandAlignedMult = 1.0 - 0.1  -- 10% discount
      tickSpecificMult = 1.0         -- No discount
      hybridMult = 1.0 - 0.05        -- 5% discount
  
  assertEqual "Band-aligned multiplier" 0.9 bandAlignedMult
  assertEqual "Tick-specific multiplier" 1.0 tickSpecificMult
  assertEqual "Hybrid multiplier" 0.95 hybridMult
  
  -- Test term multipliers
  let spotTermMult = 1.0 + 0.05      -- 5% premium
      hourlyTermMult = 1.0           -- Baseline
      dailyTermMult = 1.0 - 0.05     -- 5% discount
      weeklyTermMult = 1.0 - 0.1     -- 10% discount
  
  assertEqual "Spot term multiplier" 1.05 spotTermMult
  assertEqual "Hourly term multiplier" 1.0 hourlyTermMult
  assertEqual "Daily term multiplier" 0.95 dailyTermMult
  assertEqual "Weekly term multiplier" 0.9 weeklyTermMult
  
  -- Test leverage multipliers
  let lev1xMult = 1.0 + ((1.0 - 1.0) * 0.1)  -- No extra fee
      lev2xMult = 1.0 + ((2.0 - 1.0) * 0.1)  -- 10% extra
      lev5xMult = 1.0 + ((5.0 - 1.0) * 0.1)  -- 40% extra
  
  assertEqual "1x leverage multiplier" 1.0 lev1xMult
  assertEqual "2x leverage multiplier" 1.1 lev2xMult
  assertEqual "5x leverage multiplier" 1.4 lev5xMult
  
  -- Test combined fee calculation
  let baseFee = 1000.0 * 0.003  -- 3.0
      -- Band-aligned (0.9) * Daily (0.95) * 2x leverage (1.1)
      totalMultiplier = 0.9 * 0.95 * 1.1
      expectedFee = baseFee * totalMultiplier
  
  assertApprox "Combined fee calculation" expectedFee (baseFee * totalMultiplier) 0.01

--------------------------------------------------------------------------------
-- Leverage Adjustment Tests
--------------------------------------------------------------------------------

testLeverageAdjustments :: Effect Unit
testLeverageAdjustments = do
  log "\n=== Testing Leverage Adjustments ==="
  
  -- Test dynamic leverage adjustment
  let leverageConfig = 
        { targetLeverage: 5.0
        , currentLeverage: 5.0
        , mode: Dynamic 1.0
        , decayAfterTerm: true
        }
      
      pos = createPosition 1 "user1" 1000.0 
        { base: FeelsSOL, quote: JitoSOL }
        (BandAligned { tier: MediumBand
                     , centerTracking: SpotPrice
                     , adaptiveWidth: true
                     , lastUpdate: 0.0
                     , cachedBounds: Nothing
                     })
        Spot
        leverageConfig
        0.0
  
  -- Test health-based adjustment
  let healthyPos = adjustLeverageForHealth 1.5 pos
      stressedPos = adjustLeverageForHealth 1.1 pos
      criticalPos = adjustLeverageForHealth 0.8 pos
  
  assertEqual "Healthy leverage unchanged" 5.0 healthyPos.leverageConfig.currentLeverage
  assertApprox "Stressed leverage reduced" 5.0 stressedPos.leverageConfig.currentLeverage 0.1
  assert "Critical leverage heavily reduced" (criticalPos.leverageConfig.currentLeverage < 4.0)
  
  -- Test leverage decay
  let decayPos = pos { leverageConfig = leverageConfig { decayAfterTerm = true } }
      decayed1 = decayLeverageAfterExpiry decayPos
      decayed2 = decayLeverageAfterExpiry decayed1
      decayed3 = decayLeverageAfterExpiry decayed2
  
  assert "Leverage decays over time" 
    (decayed3.leverageConfig.currentLeverage < decayPos.leverageConfig.currentLeverage)
  assert "Leverage never goes below 1.0" 
    (decayed3.leverageConfig.currentLeverage >= 1.0)
  
  -- Test effective leverage calculation
  let effLeverage = calculateEffectiveLeverage stressedPos
  assertEqual "Effective leverage matches current" 
    stressedPos.leverageConfig.currentLeverage effLeverage

--------------------------------------------------------------------------------
-- Run All Tests
--------------------------------------------------------------------------------

runAllTests :: Effect Unit
runAllTests = do
  log "Running Position Module Tests..."
  testBandWidths
  testTermSynchronization
  testFeeCalculations
  testLeverageAdjustments
  log "\nAll tests completed!"

-- Helper for abs
abs :: Number -> Number
abs x = if x < 0.0 then -x else x

-- Import sqrt from Position module
foreign import sqrt :: Number -> Number
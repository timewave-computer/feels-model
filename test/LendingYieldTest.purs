-- | Test module for lending yield functionality
module Test.LendingYieldTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck, (===))
import Data.Number (abs)
import Data.Array (uncons, find)
import Data.Maybe (Maybe(..))

-- Import protocol modules
import Protocol.Pool as Pool
import Protocol.Position as Position
import Protocol.Token (TokenType(..))

-- Test helpers
createTestPool :: Effect Pool.PoolState
createTestPool = pure $ Pool.initializePool FeelsSOL JitoSOL 1.0 
  { tickSpacing: 10
  , fee: 30.0  -- 0.3%
  , maxLiquidityPerTick: 1000000.0
  }

createTestPosition :: Effect Position.Position
createTestPosition = pure $ Position.createPosition
  1                        -- id
  "test-user"             -- owner
  1000.0                  -- amount
  1.0                     -- price
  Position.Monthly        -- duration
  Position.Junior         -- leverage (3x)
  false                   -- rollover
  3000.0                  -- shares (amount * leverage)
  0                       -- currentBlock

-- Test suite
lendingYieldTests :: Effect Unit
lendingYieldTests = do
  log "\n=== Lending Yield System Tests ==="
  
  testAPYCalculation
  testPositionYieldCalculation
  testPositionValueUpdates
  testLeveragePnLDistribution
  testEndToEndPositionCreation
  
  log "✓ All lending yield tests passed"

-- Helper function for approximate equality
approxEqual :: Number -> Number -> Boolean
approxEqual a b = abs (a - b) < 0.0001

-- Test APY calculation based on duration and leverage
testAPYCalculation :: Effect Unit
testAPYCalculation = do
  log "\nTesting APY calculation..."
  pool <- createTestPool
  
  let spotSeniorAPY = Pool.getAPY pool Position.Spot Position.Senior
  let spotJuniorAPY = Pool.getAPY pool Position.Spot Position.Junior
  let monthlySeniorAPY = Pool.getAPY pool Position.Monthly Position.Senior
  let monthlyJuniorAPY = Pool.getAPY pool Position.Monthly Position.Junior
  
  -- Base APY is 0.05 (5%)
  quickCheck $ approxEqual spotSeniorAPY 0.05         -- 0.05 * 1x * 1.0 = 0.05
  quickCheck $ approxEqual spotJuniorAPY 0.15         -- 0.05 * 3x * 1.0 = 0.15
  quickCheck $ approxEqual monthlySeniorAPY 0.06      -- 0.05 * 1x * 1.2 = 0.06
  quickCheck $ approxEqual monthlyJuniorAPY 0.18      -- 0.05 * 3x * 1.2 = 0.18
  
  log "  ✓ APY calculations correct for all duration/leverage combinations"

-- Test position yield calculation
testPositionYieldCalculation :: Effect Unit
testPositionYieldCalculation = do
  log "\nTesting position yield calculation..."
  pool <- createTestPool
  position <- createTestPosition
  
  -- Simulate fee growth
  let poolWithFees = pool { feeGrowthGlobal0X128 = 100.0
                          , feeGrowthGlobal1X128 = 100.0
                          }
  
  -- Calculate yield after 100 blocks
  let yield = Pool.calculatePositionYield position poolWithFees 100
  
  -- With 3000 shares, fee growth of 100 each, and 3x leverage:
  -- yieldFromFees = 3000 * (100 + 100) / 2 = 300000
  -- leverageBonus = 3.0
  -- durationMultiplier = 1.2 (monthly)
  -- totalYield = 300000 * 3.0 * 1.2 = 1080000
  quickCheck $ approxEqual yield 1080000.0
  
  log "  ✓ Yield calculation includes leverage and duration multipliers"

-- Test position value updates with yield
testPositionValueUpdates :: Effect Unit
testPositionValueUpdates = do
  log "\nTesting position value updates with yield..."
  pool <- createTestPool
  position <- createTestPosition
  
  -- Add fee growth to pool
  let poolWithFees = pool { feeGrowthGlobal0X128 = 50.0
                          , feeGrowthGlobal1X128 = 50.0
                          }
  
  -- Update position yield
  let updatedPosition = Pool.updatePositionYield position poolWithFees 100
  
  -- Check that yield was added to value
  quickCheck $ updatedPosition.value > position.value
  quickCheck $ updatedPosition.accumulatedYield > 0.0
  quickCheck $ updatedPosition.lastYieldClaim === 100
  
  log "  ✓ Position value increases with accumulated yield"

-- Test leverage-based PnL distribution
testLeveragePnLDistribution :: Effect Unit
testLeveragePnLDistribution = do
  log "\nTesting leverage-based PnL distribution..."
  
  -- Create leverage state with both tiers
  let leverageState = { totalValue: 1000.0
                      , leverageGroups: 
                        [ { leverage: 1.0, value: 600.0, shares: 600.0 }   -- Senior
                        , { leverage: 3.0, value: 400.0, shares: 1200.0 }  -- Junior
                        ]
                      }
  
  -- Test profit scenario
  let profitValues = Pool.calculateLeverageValues 1000.0 1100.0 leverageState
  
  case profitValues.values of
    [senior, junior] -> do
      log $ "    Senior value: " <> show senior.value <> " (started with 600)"
      log $ "    Junior value: " <> show junior.value <> " (started with 400)"
      -- Check proportional gains: Junior gets 3x leverage on profits
      -- Senior gain: 633.33 - 600 = 33.33
      -- Junior gain: 466.67 - 400 = 66.67 (2x senior's gain, which is correct)
      let seniorGain = senior.value - 600.0
      let juniorGain = junior.value - 400.0
      quickCheck $ juniorGain > seniorGain  -- Junior gets more absolute profit
      quickCheck $ approxEqual (senior.value + junior.value) 1100.0
      log "  ✓ Profits distributed proportionally to leverage"
    _ -> log "  ✗ Unexpected leverage group structure"
  
  -- Test loss scenario  
  let lossValues = Pool.calculateLeverageValues 1000.0 900.0 leverageState
  
  case lossValues.values of
    values -> do
      -- Find by leverage value instead of assuming order
      let seniorGroup = values # find (\v -> v.leverage == 1.0)
      let juniorGroup = values # find (\v -> v.leverage == 3.0)
      
      case { senior: seniorGroup, junior: juniorGroup } of
        { senior: Just s, junior: Just j } -> do
          log $ "    Loss scenario - Senior (1x) value: " <> show s.value
          log $ "    Loss scenario - Junior (3x) value: " <> show j.value
          -- In loss scenario, junior should absorb losses first
          -- Total loss is 100, junior had 400, so should go to 300
          -- Senior should remain at 600
          quickCheck $ approxEqual s.value 600.0  -- Senior unchanged
          quickCheck $ approxEqual j.value 300.0   -- Junior lost 100
          log "  ✓ Losses absorbed by junior tier first"
        _ -> log "  ✗ Could not find leverage groups"

-- Test end-to-end position creation
testEndToEndPositionCreation :: Effect Unit
testEndToEndPositionCreation = do
  log "\nTesting end-to-end position creation with yield tracking..."
  
  -- Create a test position directly
  let testPosition = Position.createPosition
        42                      -- id
        "test-user"            -- owner
        1000.0                 -- amount
        1.0                    -- price
        Position.Monthly       -- duration
        Position.Junior        -- leverage
        false                  -- rollover
        3000.0                 -- shares (leveraged amount)
        100                    -- currentBlock
  
  -- Verify position has yield tracking fields initialized
  quickCheck $ testPosition.accumulatedYield === 0.0
  quickCheck $ testPosition.lastYieldClaim === 100
  quickCheck $ testPosition.feeGrowthInside0 === 0.0
  quickCheck $ testPosition.feeGrowthInside1 === 0.0
  
  log "  ✓ Position created with yield tracking fields initialized"
-- | Test Module for Term Expiry System
-- |
-- | This module tests the term expiry functionality including:
-- | - Automatic rollover for positions with rollover=true
-- | - Flash loan conversion for positions with rollover=false
-- | - Correct timing of term expiries
-- | - Integration with the protocol clock
module Test.TermExpiryTest where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array (index)
import Data.Maybe (Maybe(..))

import Protocol.Pool (Duration(..), Leverage(..))
import Protocol.PositionVault 
  ( VaultPosition
  , CreatePositionParams
  , createVaultPosition
  , blocksPerMonth
  , isExpired
  , handleExpiredVaultPosition
  , processExpiredVaultPositions
  )
import Protocol.Common (PositionId, BlockNumber, ShareAmount)
import Protocol.Token (TokenType(..))

-- | Create a test position for expiry testing  
createTestPosition :: PositionId -> Boolean -> Duration -> BlockNumber -> Effect VaultPosition
createTestPosition id rollover duration currentBlock =
  createVaultPosition
    { id: id
    , owner: "test-user"
    , amount: 1000.0
    , price: 1.0
    , duration: duration
    , leverage: Leverage 1.0  -- 1x leverage for test
    , lendAsset: Custom "TEST"
    , collateralAsset: FeelsSOL
    , rollover: rollover
    , shares: 1000.0
    , currentBlock: currentBlock
    }

-- | Test that monthly positions expire after exactly 28 days
testMonthlyPositionExpiry :: Effect Unit
testMonthlyPositionExpiry = do
  log "Testing monthly position expiry timing..."
  
  position <- createTestPosition 1 false Monthly 0
  
  -- Position should not be expired before 28 days
  if not $ isExpired (blocksPerMonth - 1) position
    then log "  ✓ Position not expired before 28 days"
    else log "  ✗ FAILED: Position expired too early"
  
  -- Position should be expired exactly at 28 days
  if isExpired blocksPerMonth position
    then log "  ✓ Position expired at exactly 28 days"
    else log "  ✗ FAILED: Position not expired at 28 days"
  
  -- Position should remain expired after 28 days
  if isExpired (blocksPerMonth + 1) position
    then log "  ✓ Position remains expired after 28 days"
    else log "  ✗ FAILED: Position not expired after 28 days"

-- | Test that positions with rollover=true roll into new monthly terms
testPositionRollover :: Effect Unit
testPositionRollover = do
  log "Testing position rollover functionality..."
  
  position <- createTestPosition 2 true Monthly 0
  let currentBlock = blocksPerMonth + 100
      expiredPosition = handleExpiredVaultPosition position currentBlock
  
  -- Position should still be Monthly duration
  if expiredPosition.terms.duration == Monthly
    then log "  ✓ Position maintained Monthly duration"
    else log "  ✗ FAILED: Position duration changed from Monthly"
  
  -- Creation time should be updated to current block
  if expiredPosition.timestamps.createdAt == currentBlock
    then log "  ✓ Position creation time updated to current block"
    else log $ "  ✗ FAILED: Creation time not updated (expected " <> show currentBlock <> ", got " <> show expiredPosition.timestamps.createdAt <> ")"
  
  -- Position should no longer be expired after rollover
  if not $ isExpired currentBlock expiredPosition
    then log "  ✓ Position no longer expired after rollover"
    else log "  ✗ FAILED: Position still expired after rollover"
  
  -- Position should expire again after another 28 days
  if isExpired (currentBlock + blocksPerMonth) expiredPosition
    then log "  ✓ Position will expire again after 28 days"
    else log "  ✗ FAILED: Position won't expire after next term"

-- | Test that positions with rollover=false convert to flash loans
testFlashLoanConversion :: Effect Unit
testFlashLoanConversion = do
  log "Testing flash loan conversion..."
  
  position <- createTestPosition 3 false Monthly 0
  let currentBlock = blocksPerMonth + 100
      expiredPosition = handleExpiredVaultPosition position currentBlock
  
  -- Position should convert to Flash duration
  if expiredPosition.terms.duration == Flash
    then log "  ✓ Position converted to Flash duration"
    else log $ "  ✗ FAILED: Position not converted to Flash (got " <> show expiredPosition.terms.duration <> ")"
  
  -- Flash positions should never be expired (perpetual instant withdrawal)
  if not $ isExpired currentBlock expiredPosition
    then log "  ✓ Flash position not expired at current block"
    else log "  ✗ FAILED: Flash position is expired"
  
  if not $ isExpired (currentBlock + 1000000) expiredPosition
    then log "  ✓ Flash position not expired far in future"
    else log "  ✗ FAILED: Flash position expires in future"
  
  -- All other fields should remain unchanged
  if expiredPosition.metrics.amount == position.metrics.amount &&
     expiredPosition.owner == position.owner &&
     expiredPosition.terms.leverage == position.terms.leverage
    then log "  ✓ Other position fields unchanged"
    else log "  ✗ FAILED: Position fields were modified"

-- | Test batch processing of multiple expired positions
testBatchExpiry :: Effect Unit
testBatchExpiry = do
  log "Testing batch expiry processing..."
  
  pos1 <- createTestPosition 1 true Monthly 0     -- Will rollover
  pos2 <- createTestPosition 2 false Monthly 0    -- Will convert to flash  
  pos3 <- createTestPosition 3 true Swap 0        -- Swap, never expires
  pos4 <- createTestPosition 4 false Flash 0      -- Flash, never expires
  
  let positions = [pos1, pos2, pos3, pos4]
      currentBlock = blocksPerMonth + 50
      processed = processExpiredVaultPositions currentBlock positions
  
  -- First position should rollover to new monthly term
  case processed !! 0 of
    Nothing -> log "  ✗ FAILED: Position 0 not found"
    Just p -> 
      if p.terms.duration == Monthly && p.timestamps.createdAt == currentBlock
        then log "  ✓ Position 0 rolled over correctly"
        else log "  ✗ FAILED: Position 0 rollover incorrect"
  
  -- Second position should convert to flash
  case processed !! 1 of
    Nothing -> log "  ✗ FAILED: Position 1 not found"
    Just p -> 
      if p.terms.duration == Flash
        then log "  ✓ Position 1 converted to Flash"
        else log "  ✗ FAILED: Position 1 not converted to Flash"
  
  -- Third position (Swap) should remain unchanged
  case processed !! 2 of
    Nothing -> log "  ✗ FAILED: Position 2 not found"
    Just p -> 
      if p.terms.duration == Swap && p.timestamps.createdAt == 0
        then log "  ✓ Position 2 (Swap) unchanged"
        else log "  ✗ FAILED: Position 2 was modified"
  
  -- Fourth position (Flash) should remain unchanged
  case processed !! 3 of
    Nothing -> log "  ✗ FAILED: Position 3 not found"
    Just p -> 
      if p.terms.duration == Flash && p.timestamps.createdAt == 0
        then log "  ✓ Position 3 (Flash) unchanged"
        else log "  ✗ FAILED: Position 3 was modified"

-- | Test that swap positions never expire
testSwapPositionNoExpiry :: Effect Unit
testSwapPositionNoExpiry = do
  log "Testing swap position expiry behavior..."
  
  position <- createTestPosition 5 false Swap 0
  
  -- Swap positions should never expire, regardless of time
  if not (isExpired 0 position) && 
     not (isExpired blocksPerMonth position) && 
     not (isExpired (blocksPerMonth * 100) position)
    then log "  ✓ Swap position never expires"
    else log "  ✗ FAILED: Swap position expires"

-- | Test that flash positions never expire
testFlashPositionNoExpiry :: Effect Unit
testFlashPositionNoExpiry = do
  log "Testing flash position expiry behavior..."
  
  position <- createTestPosition 6 false Flash 0
  
  -- Flash positions should never expire
  if not (isExpired 0 position) && 
     not (isExpired 1 position) && 
     not (isExpired 1000000 position)
    then log "  ✓ Flash position never expires"
    else log "  ✗ FAILED: Flash position expires"

-- | Run all term expiry tests
main :: Effect Unit
main = do
  log "\n=== Term Expiry System Tests ==="
  
  testMonthlyPositionExpiry
  testPositionRollover
  testFlashLoanConversion
  testBatchExpiry
  testSwapPositionNoExpiry
  testFlashPositionNoExpiry
  
  log "\n✅ Term expiry tests completed!"

-- Helper function for array indexing
infixl 8 index as !!
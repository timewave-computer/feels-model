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
  , createVaultPosition
  , blocksPerMonth
  , isExpired
  , handleExpiredVaultPosition
  , processExpiredVaultPositions
  )
import Protocol.Token (TokenType(..))

-- | Create a test position for expiry testing
createTestPosition :: Int -> Boolean -> Duration -> VaultPosition
createTestPosition id rollover duration =
  createVaultPosition
    id                    -- Position ID
    "test-user"          -- Owner
    1000.0               -- Amount
    1.0                  -- Price
    duration             -- Duration
    Senior               -- Leverage
    (Token "TEST")       -- Lend asset
    FeelsSOL             -- Collateral asset
    rollover             -- Rollover setting
    1000.0               -- Shares
    0                    -- Created at block 0

-- | Test that monthly positions expire after exactly 28 days
testMonthlyPositionExpiry :: Effect Unit
testMonthlyPositionExpiry = do
  log "Testing monthly position expiry timing..."
  
  let position = createTestPosition 1 false Monthly
  
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
  
  let position = createTestPosition 2 true Monthly
      currentBlock = blocksPerMonth + 100
      expiredPosition = handleExpiredVaultPosition position currentBlock
  
  -- Position should still be Monthly duration
  if expiredPosition.duration == Monthly
    then log "  ✓ Position maintained Monthly duration"
    else log "  ✗ FAILED: Position duration changed from Monthly"
  
  -- Creation time should be updated to current block
  if expiredPosition.createdAt == currentBlock
    then log "  ✓ Position creation time updated to current block"
    else log $ "  ✗ FAILED: Creation time not updated (expected " <> show currentBlock <> ", got " <> show expiredPosition.createdAt <> ")"
  
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
  
  let position = createTestPosition 3 false Monthly
      currentBlock = blocksPerMonth + 100
      expiredPosition = handleExpiredVaultPosition position currentBlock
  
  -- Position should convert to Flash duration
  if expiredPosition.duration == Flash
    then log "  ✓ Position converted to Flash duration"
    else log $ "  ✗ FAILED: Position not converted to Flash (got " <> show expiredPosition.duration <> ")"
  
  -- Flash positions should never be expired (perpetual instant withdrawal)
  if not $ isExpired currentBlock expiredPosition
    then log "  ✓ Flash position not expired at current block"
    else log "  ✗ FAILED: Flash position is expired"
  
  if not $ isExpired (currentBlock + 1000000) expiredPosition
    then log "  ✓ Flash position not expired far in future"
    else log "  ✗ FAILED: Flash position expires in future"
  
  -- All other fields should remain unchanged
  if expiredPosition.amount == position.amount &&
     expiredPosition.owner == position.owner &&
     expiredPosition.leverage == position.leverage
    then log "  ✓ Other position fields unchanged"
    else log "  ✗ FAILED: Position fields were modified"

-- | Test batch processing of multiple expired positions
testBatchExpiry :: Effect Unit
testBatchExpiry = do
  log "Testing batch expiry processing..."
  
  let positions =
        [ createTestPosition 1 true Monthly    -- Will rollover
        , createTestPosition 2 false Monthly   -- Will convert to flash
        , createTestPosition 3 true Swap       -- Swap, never expires
        , createTestPosition 4 false Flash     -- Flash, never expires
        ]
      
      currentBlock = blocksPerMonth + 50
      processed = processExpiredVaultPositions currentBlock positions
  
  -- First position should rollover to new monthly term
  case processed !! 0 of
    Nothing -> log "  ✗ FAILED: Position 0 not found"
    Just p -> 
      if p.duration == Monthly && p.createdAt == currentBlock
        then log "  ✓ Position 0 rolled over correctly"
        else log "  ✗ FAILED: Position 0 rollover incorrect"
  
  -- Second position should convert to flash
  case processed !! 1 of
    Nothing -> log "  ✗ FAILED: Position 1 not found"
    Just p -> 
      if p.duration == Flash
        then log "  ✓ Position 1 converted to Flash"
        else log "  ✗ FAILED: Position 1 not converted to Flash"
  
  -- Third position (Swap) should remain unchanged
  case processed !! 2 of
    Nothing -> log "  ✗ FAILED: Position 2 not found"
    Just p -> 
      if p.duration == Swap && p.createdAt == 0
        then log "  ✓ Position 2 (Swap) unchanged"
        else log "  ✗ FAILED: Position 2 was modified"
  
  -- Fourth position (Flash) should remain unchanged
  case processed !! 3 of
    Nothing -> log "  ✗ FAILED: Position 3 not found"
    Just p -> 
      if p.duration == Flash && p.createdAt == 0
        then log "  ✓ Position 3 (Flash) unchanged"
        else log "  ✗ FAILED: Position 3 was modified"

-- | Test that swap positions never expire
testSwapPositionNoExpiry :: Effect Unit
testSwapPositionNoExpiry = do
  log "Testing swap position expiry behavior..."
  
  let position = createTestPosition 5 false Swap
  
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
  
  let position = createTestPosition 6 false Flash
  
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
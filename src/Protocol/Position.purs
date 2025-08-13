-- | Position Module - Core position system for Feels Protocol
-- |
-- | This module implements a unified position model with:
-- | - Price: Continuous parameter for specific tick levels
-- | - Duration: Discrete terms (Spot or Monthly)
-- | - Leverage: Discrete tiers (Senior 1x or Junior 3x)
-- | - Share-based accounting for pool ownership
-- | - No liquidations - positions adjust in value only
module Protocol.Position
  ( -- Position types
    Position
  , Duration(..)
  , Leverage(..)
  , leverageMultiplier
  -- Functions
  , createPosition
  , getPositionValue
  , isSpot
  , isTermPosition
  , getTermExpiry
  -- Term expiry functions
  , getNextExpiry
  , rollToSpot
  , isExpired
  , getNextMonthlyExpiry
  , blocksPerMonth
  , blocksUntilExpiry
  -- Duration constructors
  , spotDuration
  , monthlyDuration
  -- Term scheduling
  , TermSchedule
  , ExpiryBatch
  , getNextTermExpiries
  , groupPositionsByExpiry
  , processExpiredPositions
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber, round)
import Data.Array as Array
import Data.Array ((:), head, filter, groupBy, sortWith)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty (NonEmptyArray)
import Protocol.Token (TokenType)
import Protocol.Common (PoolId, PositionId, BlockNumber, ShareAmount)

--------------------------------------------------------------------------------
-- CORE TYPE DEFINITIONS
--------------------------------------------------------------------------------
-- The fundamental types that define positions, tranches, and term commitments

-- | Position structure - unified model with continuous price, discrete duration/leverage
type Position =
  { id :: PositionId
  , owner :: String
  , amount :: Number               -- Initial capital invested
  , price :: Number                -- Price level (tick rate) for liquidity
  , duration :: Duration           -- Time commitment (Spot or Monthly)
  , leverage :: Leverage           -- Exposure tier (Senior 1x or Junior 3x)
  , rollover :: Boolean            -- Whether position auto-rolls to next term
  , shares :: ShareAmount          -- Pool ownership shares
  , createdAt :: BlockNumber       -- Creation block number
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount locked below floor
  , accumulatedYield :: Number     -- Total yield earned
  , lastYieldClaim :: BlockNumber  -- Last block yield was claimed/updated
  , feeGrowthInside0 :: Number     -- Fee growth inside position's range (token0)
  , feeGrowthInside1 :: Number     -- Fee growth inside position's range (token1)
  }

-- | Duration options - discrete time commitments
data Duration
  = Spot      -- Perpetual, no expiry
  | Monthly   -- 28-day term commitment

derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

instance showDuration :: Show Duration where
  show Spot = "Spot"
  show Monthly = "Monthly"

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
leverageMultiplier Senior = 1.0
leverageMultiplier Junior = 3.0

-- | Get duration in blocks
durationBlocks :: Duration -> Int
durationBlocks Spot = 0
durationBlocks Monthly = blocksPerMonth


--------------------------------------------------------------------------------
-- SYSTEM CONSTANTS
--------------------------------------------------------------------------------
-- Block timing constants used throughout the term system

-- | Blocks per month (28 days, assuming 30-second blocks on Solana)
-- | 2 blocks per minute * 60 * 24 * 28 = 80640 blocks
blocksPerMonth :: Int
blocksPerMonth = 80640

--------------------------------------------------------------------------------
-- TERM COMMITMENT CONSTRUCTORS
--------------------------------------------------------------------------------
-- Validated ways to create term commitments

-- | Create a spot duration
spotDuration :: Duration
spotDuration = Spot

-- | Create a monthly duration
monthlyDuration :: Duration  
monthlyDuration = Monthly

--------------------------------------------------------------------------------
-- POSITION CREATION
--------------------------------------------------------------------------------
-- Functions for creating new positions

-- | Create a new position with unified parameters
createPosition :: 
  PositionId ->     -- Unique position identifier
  String ->         -- Position owner
  Number ->         -- Initial investment amount
  Number ->         -- Price level for liquidity
  Duration ->       -- Time commitment (Spot or Monthly)
  Leverage ->       -- Exposure tier (Senior or Junior)
  Boolean ->        -- Auto-rollover setting
  ShareAmount ->    -- Shares allocated by pool
  BlockNumber ->    -- Current block number
  Position
createPosition id owner amount price duration leverage rollover shares currentBlock =
  { id
  , owner
  , amount
  , price
  , duration
  , leverage
  , rollover
  , shares
  , createdAt: currentBlock
  , value: amount  -- Initial value equals amount invested
  , lockedAmount: 0.0
  , accumulatedYield: 0.0
  , lastYieldClaim: currentBlock
  , feeGrowthInside0: 0.0
  , feeGrowthInside1: 0.0
  }

--------------------------------------------------------------------------------
-- POSITION QUERIES AND INSPECTION
--------------------------------------------------------------------------------
-- Functions to inspect position properties and current state

-- | Get current position value (never liquidated, only adjusted)
getPositionValue :: Position -> Number
getPositionValue pos = pos.value

-- | Check if position is spot (perpetual swap, no expiry)
isSpot :: Position -> Boolean
isSpot pos = pos.duration == Spot

-- | Check if position has a term commitment (will expire)
isTermPosition :: Position -> Boolean
isTermPosition pos = not (isSpot pos)

-- | Get term expiry block number if position has duration
getTermExpiry :: Position -> Maybe Int
getTermExpiry pos = case pos.duration of
  Spot -> Nothing
  Monthly -> Just (pos.createdAt + blocksPerMonth)

-- | Check if position is senior (base exposure)
isSenior :: Position -> Boolean
isSenior pos = pos.leverage == Senior

-- | Check if position is junior (leveraged)
isJunior :: Position -> Boolean
isJunior pos = pos.leverage == Junior

-- | Get the numeric leverage multiplier for a position
getPositionLeverage :: Position -> Number
getPositionLeverage pos = leverageMultiplier pos.leverage

--------------------------------------------------------------------------------
-- TERM EXPIRY CALCULATION
--------------------------------------------------------------------------------
-- Core logic for calculating when positions expire and next expiry dates

-- | Get the next expiry block for a given duration
getNextExpiry :: Duration -> Int -> Int
getNextExpiry duration currentBlock = case duration of
  Spot -> 2147483647  -- MaxInt32 (effectively never expires)
  Monthly -> getNextMonthlyExpiry currentBlock

-- | Calculate the next monthly boundary block (28-day synchronized expiry)
getNextMonthlyExpiry :: Int -> Int
getNextMonthlyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerMonth
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerMonth

-- | Calculate blocks remaining until a position expires
blocksUntilExpiry :: Position -> Int -> Int
blocksUntilExpiry position currentBlock = case position.duration of
  Spot -> 2147483647  -- MaxInt32 (never expires)
  Monthly -> max 0 ((position.createdAt + blocksPerMonth) - currentBlock)

--------------------------------------------------------------------------------
-- TERM EXPIRY PROCESSING
--------------------------------------------------------------------------------
-- Functions for handling position expiry and rollover logic

-- | Check if a position has expired based on current block
isExpired :: Int -> Position -> Boolean
isExpired currentBlock position = case position.duration of
  Spot -> false  -- Spot positions never expire
  Monthly -> currentBlock >= (position.createdAt + blocksPerMonth)

-- | Handle an expired position based on its rollover setting
handleExpiredPosition :: Position -> Int -> Position
handleExpiredPosition position currentBlock =
  if position.rollover && position.duration == Monthly
    then -- Auto-roll to next monthly term
      position { createdAt = getNextMonthlyExpiry currentBlock - blocksPerMonth }
    else -- Convert to spot (no further expiry)
      rollToSpot position

-- | Convert a position to spot (remove duration)
rollToSpot :: Position -> Position
rollToSpot position = position { duration = Spot }

--------------------------------------------------------------------------------
-- BATCH TERM SCHEDULING
--------------------------------------------------------------------------------
-- Higher-level functions for managing multiple positions and synchronized expiries

-- | Track upcoming synchronized term expiry dates
type TermSchedule =
  { nextMonthly :: BlockNumber      -- Next monthly expiry block
  }

-- | A batch of positions that expire at the same synchronized block
type ExpiryBatch =
  { expiryBlock :: BlockNumber
  , positions :: Array Position
  }

-- | Get the next term expiry blocks for scheduling
getNextTermExpiries :: BlockNumber -> TermSchedule
getNextTermExpiries currentBlock =
  { nextMonthly: getNextMonthlyExpiry currentBlock
  }

-- | Group positions by their expiry block for batch processing
groupPositionsByExpiry :: Array Position -> Array ExpiryBatch
groupPositionsByExpiry positions =
  let
    -- Extract positions with expiries and their expiry blocks
    termPositionsWithExpiry = Array.mapMaybe getPositionWithExpiry positions
    
    -- Sort by expiry block for consistent ordering
    sorted = sortWith _.expiry termPositionsWithExpiry
    
    -- Group positions by identical expiry blocks
    grouped = groupBy (\a b -> a.expiry == b.expiry) sorted
    
    -- Convert grouped positions to ExpiryBatch format
    toBatch :: NonEmptyArray { position :: Position, expiry :: BlockNumber } -> ExpiryBatch
    toBatch group = 
      let positions' = NEA.toArray $ map _.position group
          expiry = (NEA.head group).expiry
      in { expiryBlock: expiry, positions: positions' }
  in
    map toBatch grouped
  where
    -- Extract position with its expiry block (if it has one)
    getPositionWithExpiry :: Position -> Maybe { position :: Position, expiry :: BlockNumber }
    getPositionWithExpiry pos = 
      getTermExpiry pos >>= \expiry -> 
        Just { position: pos, expiry: expiry }

-- | Process all expired positions in an array, handling rollover settings
processExpiredPositions :: BlockNumber -> Array Position -> Array Position
processExpiredPositions currentBlock = map processOne
  where
    processOne pos = 
      if isExpired currentBlock pos
      then handleExpiredPosition pos currentBlock
      else pos

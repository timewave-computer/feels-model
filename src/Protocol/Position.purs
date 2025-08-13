-- | Position Module - Unified position system for Feels Protocol
-- |
-- | This module implements a two-tranche position model:
-- | - Tranche Selection: Senior (1x) or Junior (3x) exposure
-- | - Term Commitment: Spot or Monthly (28-day) synchronized terms
-- | - Share-based accounting within tranches
-- | - No liquidations - positions adjust in value only
module Protocol.Position
  ( -- Position types
    Position
  , Tranche(..)
  , TermCommitment(..)
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
  -- Term commitment constructors
  , spotTerm
  , monthlyTerm
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

-- | Position structure - represents a user's lending position in the protocol
type Position =
  { id :: PositionId
  , poolId :: PoolId               -- Pool this position belongs to
  , owner :: String
  , amount :: Number               -- Initial capital invested
  , tranche :: Tranche             -- Senior (1x) or Junior (3x)
  , term :: TermCommitment         -- Synchronized term commitment
  , rollover :: Boolean            -- Whether position auto-rolls to next term
  , shares :: ShareAmount          -- Shares within their tranche
  , createdAt :: BlockNumber       -- Creation block number
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount locked below floor
  }

-- | Tranche selection - replaces traditional leverage with risk/reward tiers
data Tranche
  = Senior    -- 1x exposure, protected, lower yield
  | Junior    -- 3x exposure, first loss, higher yield

derive instance eqTranche :: Eq Tranche
derive instance ordTranche :: Ord Tranche

instance showTranche :: Show Tranche where
  show Senior = "Senior"
  show Junior = "Junior"

-- | Term commitment with synchronized block-based expiries
data TermCommitment
  = Spot                           -- Perpetual swap, no expiry
  | Monthly BlockNumber            -- 28-day term, expires at block boundary

derive instance eqTermCommitment :: Eq TermCommitment

instance showTermCommitment :: Show TermCommitment where
  show Spot = "Spot (Perpetual)"
  show (Monthly expiryBlock) = "Monthly (expires block " <> show expiryBlock <> ")"

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

-- | Create a spot term commitment (perpetual swap, no expiry)
spotTerm :: TermCommitment
spotTerm = Spot

-- | Create a monthly term commitment (28 days from current block)
monthlyTerm :: Int -> TermCommitment
monthlyTerm currentBlock = Monthly (getNextMonthlyExpiry currentBlock)

--------------------------------------------------------------------------------
-- POSITION CREATION
--------------------------------------------------------------------------------
-- Functions for creating new positions

-- | Create a new position with all required parameters
createPosition :: 
  PositionId ->     -- Unique position identifier
  PoolId ->         -- Pool this position belongs to
  String ->         -- Position owner
  Number ->         -- Initial investment amount
  Tranche ->        -- Senior (1x) or Junior (3x) tranche
  TermCommitment -> -- Term commitment (Spot or Monthly)
  Boolean ->        -- Auto-rollover setting
  ShareAmount ->    -- Shares allocated by pool (calculated elsewhere)
  BlockNumber ->    -- Current block number
  Position
createPosition id poolId owner amount tranche term rollover shares currentBlock =
  { id
  , poolId
  , owner
  , amount
  , tranche
  , term
  , rollover
  , shares
  , createdAt: currentBlock
  , value: amount  -- Initial value equals amount invested
  , lockedAmount: 0.0
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
isSpot pos = case pos.term of
  Spot -> true
  _ -> false

-- | Check if position has a term commitment (will expire)
isTermPosition :: Position -> Boolean
isTermPosition pos = not (isSpot pos)

-- | Get term expiry block number if position has term commitment
getTermExpiry :: Position -> Maybe Int
getTermExpiry pos = case pos.term of
  Spot -> Nothing
  Monthly expiryBlock -> Just expiryBlock

-- | Check if position is in senior tranche (1x exposure, protected)
isSenior :: Position -> Boolean
isSenior pos = pos.tranche == Senior

-- | Check if position is in junior tranche (3x exposure, first loss)
isJunior :: Position -> Boolean
isJunior pos = pos.tranche == Junior

-- | Get the exposure multiplier for a tranche
getTrancheMultiplier :: Tranche -> Number
getTrancheMultiplier tranche = case tranche of
  Senior -> 1.0  -- Protected exposure
  Junior -> 3.0  -- Fixed 3x exposure for MVP

--------------------------------------------------------------------------------
-- TERM EXPIRY CALCULATION
--------------------------------------------------------------------------------
-- Core logic for calculating when positions expire and next expiry dates

-- | Get the next synchronized expiry block for a given term type
getNextExpiry :: TermCommitment -> Int -> Int
getNextExpiry term currentBlock =
  case term of
    Spot -> 2147483647              -- MaxInt32 (effectively never expires)
    Monthly _ -> getNextMonthlyExpiry currentBlock

-- | Calculate the next monthly boundary block (28-day synchronized expiry)
getNextMonthlyExpiry :: Int -> Int
getNextMonthlyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerMonth
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerMonth

-- | Calculate blocks remaining until a term commitment expires
blocksUntilExpiry :: TermCommitment -> Int -> Int
blocksUntilExpiry term currentBlock =
  case term of
    Spot -> 2147483647  -- MaxInt32 (never expires)
    Monthly expiryBlock -> max 0 (expiryBlock - currentBlock)

--------------------------------------------------------------------------------
-- TERM EXPIRY PROCESSING
--------------------------------------------------------------------------------
-- Functions for handling position expiry and rollover logic

-- | Check if a position has expired based on current block
isExpired :: Int -> Position -> Boolean
isExpired currentBlock position =
  case position.term of
    Spot -> false  -- Spot positions never expire
    Monthly expiryBlock -> currentBlock >= expiryBlock

-- | Handle an expired position based on its rollover setting
handleExpiredPosition :: Position -> Int -> Position
handleExpiredPosition position currentBlock =
  if position.rollover
    then -- Auto-roll to next monthly term
      position { term = Monthly (getNextMonthlyExpiry currentBlock) }
    else -- Convert to spot (no further expiry)
      rollToSpot position

-- | Convert a position to spot (remove term commitment)
rollToSpot :: Position -> Position
rollToSpot position = position { term = Spot }

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

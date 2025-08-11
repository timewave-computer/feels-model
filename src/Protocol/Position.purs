-- | Position Module - Unified position system for Feels Protocol
-- |
-- | This module implements a two-tranche position model:
-- | - Tranche Selection: Senior (1x) or Junior (3x) exposure
-- | - Term Commitment: Spot, Hourly, Daily, or Weekly synchronized terms
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
  , getNextHourlyExpiry
  , getNextDailyExpiry
  , getNextWeeklyExpiry
  , blocksPerHour
  , blocksPerDay
  , blocksPerWeek
  , estimateTimeFromBlocks
  , blocksUntilExpiry
  -- Term commitment constructors
  , spotTerm
  , hourlyTerm
  , dailyTerm
  , weeklyTerm
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
import Data.Array ((:), head)
import Protocol.Token (TokenType)
import Protocol.Common (PoolId, PositionId, BlockNumber, ShareAmount)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Position structure
type Position =
  { id :: PositionId
  , poolId :: PoolId               -- Pool this position belongs to
  , owner :: String
  , amount :: Number               -- Initial capital invested
  , tranche :: Tranche             -- Senior (1x) or Junior (3x)
  , term :: TermCommitment         -- Synchronized term commitment
  , shares :: ShareAmount          -- Shares within their tranche
  , createdAt :: BlockNumber       -- Creation block number
  , value :: Number                -- Current value (never liquidated)
  , lockedAmount :: Number         -- Amount locked below floor
  }

-- | Tranche selection (replaces leverage system)
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
  = Spot                           -- Perpetual, no expiry
  | Hourly BlockNumber             -- Expires at hourly block boundary
  | Daily BlockNumber              -- Expires at daily block boundary
  | Weekly BlockNumber             -- Expires at weekly block boundary

derive instance eqTermCommitment :: Eq TermCommitment

instance showTermCommitment :: Show TermCommitment where
  show Spot = "Spot"
  show (Hourly expiryBlock) = "Hourly (expires block " <> show expiryBlock <> ")"
  show (Daily expiryBlock) = "Daily (expires block " <> show expiryBlock <> ")"
  show (Weekly expiryBlock) = "Weekly (expires block " <> show expiryBlock <> ")"


--------------------------------------------------------------------------------
-- Position Creation
--------------------------------------------------------------------------------

-- | Create a new position
createPosition :: 
  PositionId ->    -- ID
  PoolId ->        -- Pool ID
  String ->        -- Owner
  Number ->        -- Amount
  Tranche ->       -- Senior or Junior
  TermCommitment -> -- Term
  ShareAmount ->   -- Shares calculated by pool
  BlockNumber ->   -- Current block number
  Position
createPosition id poolId owner amount tranche term shares currentBlock =
  { id
  , poolId
  , owner
  , amount
  , tranche
  , term
  , shares
  , createdAt: currentBlock
  , value: amount  -- Initial value equals amount
  , lockedAmount: 0.0
  }

--------------------------------------------------------------------------------
-- Position Queries
--------------------------------------------------------------------------------

-- | Get current position value
getPositionValue :: Position -> Number
getPositionValue pos = pos.value

-- | Check if position is spot (perpetual)
isSpot :: Position -> Boolean
isSpot pos = case pos.term of
  Spot -> true
  _ -> false

-- | Check if position has term commitment
isTermPosition :: Position -> Boolean
isTermPosition pos = not (isSpot pos)

-- | Get term expiry block number
getTermExpiry :: Position -> Maybe Int
getTermExpiry pos = case pos.term of
  Spot -> Nothing
  Hourly expiryBlock -> Just expiryBlock
  Daily expiryBlock -> Just expiryBlock
  Weekly expiryBlock -> Just expiryBlock

--------------------------------------------------------------------------------
-- Tranche Helpers
--------------------------------------------------------------------------------

-- | Get tranche multiplier
getTrancheMultiplier :: Tranche -> Number
getTrancheMultiplier tranche = case tranche of
  Senior -> 1.0
  Junior -> 3.0  -- Fixed 3x for MVP

-- | Check if position is in junior tranche
isJunior :: Position -> Boolean
isJunior pos = pos.tranche == Junior

-- | Check if position is in senior tranche
isSenior :: Position -> Boolean
isSenior pos = pos.tranche == Senior

--------------------------------------------------------------------------------
-- Term System Constants and Functions
--------------------------------------------------------------------------------

-- | Blocks per hour (assuming 30-second blocks on Solana)
-- | 2 blocks per minute * 60 minutes = 120 blocks
blocksPerHour :: Int
blocksPerHour = 120

-- | Blocks per day
-- | 120 blocks per hour * 24 hours = 2880 blocks
blocksPerDay :: Int
blocksPerDay = 2880

-- | Blocks per week
-- | 2880 blocks per day * 7 days = 20160 blocks
blocksPerWeek :: Int
blocksPerWeek = 20160

--------------------------------------------------------------------------------
-- Block-based Term Expiry Calculation
--------------------------------------------------------------------------------

-- | Get next synchronized expiry block for a term type
getNextExpiry :: TermCommitment -> Int -> Int
getNextExpiry term currentBlock =
  case term of
    Spot -> 2147483647              -- MaxInt32 (effectively never expires)
    Hourly _ -> getNextHourlyExpiry currentBlock
    Daily _ -> getNextDailyExpiry currentBlock
    Weekly _ -> getNextWeeklyExpiry currentBlock

-- | Get next hourly boundary block
getNextHourlyExpiry :: Int -> Int
getNextHourlyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerHour
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerHour

-- | Get next daily boundary block
getNextDailyExpiry :: Int -> Int
getNextDailyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerDay
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerDay

-- | Get next weekly boundary block
getNextWeeklyExpiry :: Int -> Int
getNextWeeklyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerWeek
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerWeek

-- | Calculate blocks until expiry
blocksUntilExpiry :: TermCommitment -> Int -> Int
blocksUntilExpiry term currentBlock =
  case term of
    Spot -> 2147483647  -- MaxInt32
    Hourly expiryBlock -> max 0 (expiryBlock - currentBlock)
    Daily expiryBlock -> max 0 (expiryBlock - currentBlock)
    Weekly expiryBlock -> max 0 (expiryBlock - currentBlock)

--------------------------------------------------------------------------------
-- Term Expiry Processing
--------------------------------------------------------------------------------

-- | Check if position is expired based on current block
isExpired :: Int -> Position -> Boolean
isExpired currentBlock position =
  case position.term of
    Spot -> false  -- Spot positions never expire
    Hourly expiryBlock -> currentBlock >= expiryBlock
    Daily expiryBlock -> currentBlock >= expiryBlock
    Weekly expiryBlock -> currentBlock >= expiryBlock

-- | Roll expired position to spot
rollToSpot :: Position -> Position
rollToSpot position =
  position { term = Spot }
  -- Note: In full implementation, this would also:
  -- - Adjust shares based on any term completion bonuses
  -- - Update position value with accumulated fees
  -- - Trigger any necessary rebalancing

--------------------------------------------------------------------------------
-- UI Helper Functions
--------------------------------------------------------------------------------

-- | Estimate time remaining from blocks (for UI display only)
-- | Returns a human-readable string
estimateTimeFromBlocks :: Int -> String
estimateTimeFromBlocks blocks =
  if blocks <= 0 then
    "Expired"
  else if blocks < blocksPerHour then
    let minutes = round (toNumber blocks * 0.5)  -- 30-second blocks
    in show minutes <> " minutes"
  else if blocks < blocksPerDay then
    let hours = blocks / blocksPerHour
    in show hours <> " hours"
  else if blocks < blocksPerWeek then
    let days = blocks / blocksPerDay
    in show days <> " days"
  else
    let weeks = blocks / blocksPerWeek
    in show weeks <> " weeks"

--------------------------------------------------------------------------------
-- Term Commitment Smart Constructors
--------------------------------------------------------------------------------

-- | Create a spot term commitment (no expiry)
spotTerm :: TermCommitment
spotTerm = Spot

-- | Create an hourly term commitment with next expiry
hourlyTerm :: Int -> TermCommitment
hourlyTerm currentBlock = Hourly (getNextHourlyExpiry currentBlock)

-- | Create a daily term commitment with next expiry
dailyTerm :: Int -> TermCommitment
dailyTerm currentBlock = Daily (getNextDailyExpiry currentBlock)

-- | Create a weekly term commitment with next expiry
weeklyTerm :: Int -> TermCommitment
weeklyTerm currentBlock = Weekly (getNextWeeklyExpiry currentBlock)

--------------------------------------------------------------------------------
-- Term Scheduling
--------------------------------------------------------------------------------

-- | Track synchronized term expiries
type TermSchedule =
  { nextHourly :: BlockNumber      -- Next hourly expiry block
  , nextDaily :: BlockNumber       -- Next daily expiry block
  , nextWeekly :: BlockNumber      -- Next weekly expiry block
  }

-- | Batch of positions expiring at the same block
type ExpiryBatch =
  { expiryBlock :: BlockNumber
  , positions :: Array Position
  }

-- | Get the next term expiry blocks
getNextTermExpiries :: BlockNumber -> TermSchedule
getNextTermExpiries currentBlock =
  { nextHourly: getNextHourlyExpiry currentBlock
  , nextDaily: getNextDailyExpiry currentBlock
  , nextWeekly: getNextWeeklyExpiry currentBlock
  }

-- | Group positions by their expiry block (simplified implementation)
groupPositionsByExpiry :: Array Position -> Array ExpiryBatch
groupPositionsByExpiry positions =
  let
    -- Extract positions with expiries
    termPositions = Array.filter isTermPosition positions
    
    -- Simple implementation: create one batch for all term positions
    -- TODO: Implement proper grouping by expiry block
    createBatch :: Array Position -> Maybe ExpiryBatch  
    createBatch [] = Nothing
    createBatch ps = 
      Array.head ps >>= \p ->
        getTermExpiry p >>= \expiry ->
          Just { expiryBlock: expiry, positions: ps }
  in
    case createBatch termPositions of
      Just batch -> [batch]
      Nothing -> []

-- | Process expired positions by rolling them to spot
processExpiredPositions :: BlockNumber -> Array Position -> Array Position
processExpiredPositions currentBlock = map processOne
  where
    processOne pos = 
      if isExpired currentBlock pos
      then rollToSpot pos
      else pos

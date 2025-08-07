module PositionBook
  ( PositionBook
  , initPositionBook
  , addPosition
  , getPosition
  , updatePosition
  , getPositionsByOwner
  , getPositionsByTerm
  , getPositionsByBand
  , getAllPositions
  , getActivePositions
  , processTermExpiry
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), filter, find)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Position (Position, TermCommitment(..), PriceStrategy(..), BandTier, getNextExpiry)
import Token (TokenType)
import FFI (currentTime)
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- Position Book Type
--------------------------------------------------------------------------------

-- The new position book - organizes positions by band/term/leverage
type PositionBook =
  { positions :: Ref (Array Position)
  , nextId :: Ref Int
  , lastUpdate :: Ref Number
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize a new position book
initPositionBook :: Effect PositionBook
initPositionBook = do
  positions <- new []
  nextId <- new 1
  now <- currentTime
  lastUpdate <- new now
  pure { positions, nextId, lastUpdate }

--------------------------------------------------------------------------------
-- Core Operations
--------------------------------------------------------------------------------

-- Add a new position to the book
addPosition :: PositionBook -> Position -> Effect Unit
addPosition book position = do
  positions <- read book.positions
  write (position : positions) book.positions
  now <- currentTime
  write now book.lastUpdate

-- Get a position by ID
getPosition :: PositionBook -> Int -> Effect (Maybe Position)
getPosition book id = do
  positions <- read book.positions
  pure $ find (\p -> p.id == id) positions

-- Update an existing position
updatePosition :: PositionBook -> Position -> Effect Unit
updatePosition book updatedPos = do
  positions <- read book.positions
  let updated = map (\p -> if p.id == updatedPos.id then updatedPos else p) positions
  write updated book.positions
  now <- currentTime
  write now book.lastUpdate

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get all positions for an owner
getPositionsByOwner :: PositionBook -> String -> Effect (Array Position)
getPositionsByOwner book owner = do
  positions <- read book.positions
  pure $ filter (\p -> p.owner == owner) positions

-- Get positions by term commitment type
getPositionsByTerm :: PositionBook -> TermCommitment -> Effect (Array Position)
getPositionsByTerm book termType = do
  positions <- read book.positions
  pure $ filter (matchesTerm termType) positions
  where
    matchesTerm Spot pos = case pos.term of
      Spot -> true
      _ -> false
    matchesTerm (Hourly _) pos = case pos.term of
      Hourly _ -> true
      _ -> false
    matchesTerm (Daily _) pos = case pos.term of
      Daily _ -> true
      _ -> false
    matchesTerm (Weekly _) pos = case pos.term of
      Weekly _ -> true
      _ -> false

-- Get positions by band tier
getPositionsByBand :: PositionBook -> BandTier -> Effect (Array Position)
getPositionsByBand book tier = do
  positions <- read book.positions
  pure $ filter (hasBandTier tier) positions
  where
    hasBandTier targetTier pos = case pos.priceStrategy of
      BandAligned band -> band.tier == targetTier
      BandConstrained band _ -> band.tier == targetTier
      _ -> false

-- Get all positions
getAllPositions :: PositionBook -> Effect (Array Position)
getAllPositions book = read book.positions

-- Get active positions (not expired)
getActivePositions :: PositionBook -> Effect (Array Position)
getActivePositions book = do
  positions <- read book.positions
  now <- currentTime
  pure $ filter (isActive now) positions
  where
    isActive now pos = case pos.term of
      Spot -> true
      Hourly expiry -> now < expiry
      Daily expiry -> now < expiry
      Weekly expiry -> now < expiry

--------------------------------------------------------------------------------
-- Term Expiry Processing
--------------------------------------------------------------------------------

-- Process expired positions and roll them to spot
processTermExpiry :: PositionBook -> Effect (Array Position)
processTermExpiry book = do
  positions <- read book.positions
  now <- currentTime
  
  let { expired, active } = partition (isExpired now) positions
      rolledPositions = map rollToSpot expired
  
  -- Update the book with active positions and rolled positions
  write (active <> rolledPositions) book.positions
  write now book.lastUpdate
  
  pure rolledPositions
  where
    isExpired now pos = case pos.term of
      Spot -> false
      Hourly expiry -> now >= expiry
      Daily expiry -> now >= expiry
      Weekly expiry -> now >= expiry
    
    rollToSpot pos = pos { term = Spot }
    
    -- Simple partition function
    partition pred arr = 
      { expired: filter pred arr
      , active: filter (not <<< pred) arr
      }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Get next available ID
getNextId :: PositionBook -> Effect Int
getNextId book = do
  currentId <- read book.nextId
  _ <- modify_ (_ + 1) book.nextId
  pure currentId
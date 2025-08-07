module LendingBook
  ( LendingBook
  , LendingBookState
  , BookDimensions
  , BandBook
  , TermBook
  , LeverageBook
  , MatchResult(..)
  , initLendingBook
  , addPosition
  , removePosition
  , getPositionsByBand
  , getPositionsByTerm
  , getPositionsByLeverage
  , aggregateLiquidityByBand
  , aggregateLiquidityByTerm
  , matchPositions
  , allocatePOLAcrossBands
  , getTotalLiquidity
  , getUserPositions
  , getActivePositions
  , getNextId
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array ((:), filter, foldr, sortBy, groupBy, concat, find, null, length)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Foldable (sum)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Position (Position, PriceStrategy(..), TermCommitment(..), BandTier(..), 
                 TickRange, isSpot, isLeveraged)
import Token (TokenType)
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Organize positions by band tier
type BandBook = Map BandTier (Array Position)

-- Organize positions by term commitment
type TermBook = Map String (Array Position)  -- String key for term type

-- Organize positions by leverage range
type LeverageBook = Map String (Array Position)  -- String key for leverage range

-- Multi-dimensional position organization
type BookDimensions =
  { byBand :: BandBook
  , byTerm :: TermBook
  , byLeverage :: LeverageBook
  }

-- Main lending book with new position type
type LendingBookState =
  { positions :: Array Position          -- All positions
  , dimensions :: BookDimensions        -- Multi-dimensional indexing
  , nextId :: Int                        -- Next position ID
  , polAllocation :: Map BandTier Number -- POL allocation per band
  }

type LendingBook = Ref LendingBookState

-- Match result for position matching
data MatchResult
  = NoMatch String
  | PartialMatch Position Position Number  -- Lender, Borrower, Amount
  | FullMatch Position Position            -- Complete match
  | MatchPending Position Position         -- Match identified but not executed

derive instance eqMatchResult :: Eq MatchResult

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize empty lending book
initLendingBook :: Effect LendingBook
initLendingBook = do
  let emptyDimensions = 
        { byBand: Map.empty
        , byTerm: Map.empty
        , byLeverage: Map.empty
        }
      initialState =
        { positions: []
        , dimensions: emptyDimensions
        , nextId: 1
        , polAllocation: Map.empty
        }
  new initialState

--------------------------------------------------------------------------------
-- Position Management
--------------------------------------------------------------------------------

-- Add position to the book
addPosition :: Position -> LendingBook -> Effect Unit
addPosition pos bookRef = do
  modify_ (\book -> 
    let newPositions = pos : book.positions
        newDimensions = updateDimensions pos book.dimensions
    in book 
      { positions = newPositions
      , dimensions = newDimensions
      , nextId = book.nextId + 1
      }
  ) bookRef

-- Remove position from the book
removePosition :: Int -> LendingBook -> Effect (Maybe Position)
removePosition posId bookRef = do
  book <- read bookRef
  case find (\p -> p.id == posId) book.positions of
    Nothing -> pure Nothing
    Just pos -> do
      let newPositions = filter (\p -> p.id /= posId) book.positions
          newDimensions = removeDimensions pos book.dimensions
      modify_ (\b -> b 
        { positions = newPositions
        , dimensions = newDimensions
        }) bookRef
      pure (Just pos)

--------------------------------------------------------------------------------
-- Dimension Management
--------------------------------------------------------------------------------

-- Update dimensional indices when adding position
updateDimensions :: Position -> BookDimensions -> BookDimensions
updateDimensions pos dims =
  { byBand: updateBandBook pos dims.byBand
  , byTerm: updateTermBook pos dims.byTerm
  , byLeverage: updateLeverageBook pos dims.byLeverage
  }

-- Remove from dimensional indices
removeDimensions :: Position -> BookDimensions -> BookDimensions
removeDimensions pos dims =
  { byBand: removeBandBook pos dims.byBand
  , byTerm: removeTermBook pos dims.byTerm
  , byLeverage: removeLeverageBook pos dims.byLeverage
  }

-- Update band book
updateBandBook :: Position -> BandBook -> BandBook
updateBandBook pos bandBook =
  case getBandTier pos of
    Nothing -> bandBook
    Just tier -> Map.alter (addToArray pos) tier bandBook
  where
    addToArray p Nothing = Just [p]
    addToArray p (Just arr) = Just (p : arr)

-- Remove from band book
removeBandBook :: Position -> BandBook -> BandBook
removeBandBook pos bandBook =
  case getBandTier pos of
    Nothing -> bandBook
    Just tier -> Map.alter (removeFromArray pos.id) tier bandBook
  where
    removeFromArray pid Nothing = Nothing
    removeFromArray pid (Just arr) = 
      let filtered = filter (\p -> p.id /= pid) arr
      in if null filtered then Nothing else Just filtered

-- Update term book
updateTermBook :: Position -> TermBook -> TermBook
updateTermBook pos termBook =
  let key = termToKey pos.term
  in Map.alter (addToArray pos) key termBook
  where
    addToArray p Nothing = Just [p]
    addToArray p (Just arr) = Just (p : arr)

-- Remove from term book
removeTermBook :: Position -> TermBook -> TermBook
removeTermBook pos termBook =
  let key = termToKey pos.term
  in Map.alter (removeFromArray pos.id) key termBook
  where
    removeFromArray pid Nothing = Nothing
    removeFromArray pid (Just arr) = 
      let filtered = filter (\p -> p.id /= pid) arr
      in if null filtered then Nothing else Just filtered

-- Update leverage book
updateLeverageBook :: Position -> LeverageBook -> LeverageBook
updateLeverageBook pos levBook =
  let key = leverageToKey pos.leverageConfig.targetLeverage
  in Map.alter (addToArray pos) key levBook
  where
    addToArray p Nothing = Just [p]
    addToArray p (Just arr) = Just (p : arr)

-- Remove from leverage book
removeLeverageBook :: Position -> LeverageBook -> LeverageBook
removeLeverageBook pos levBook =
  let key = leverageToKey pos.leverageConfig.targetLeverage
  in Map.alter (removeFromArray pos.id) key levBook
  where
    removeFromArray pid Nothing = Nothing
    removeFromArray pid (Just arr) = 
      let filtered = filter (\p -> p.id /= pid) arr
      in if null filtered then Nothing else Just filtered

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get positions by band tier
getPositionsByBand :: BandTier -> LendingBook -> Effect (Array Position)
getPositionsByBand tier bookRef = do
  book <- read bookRef
  pure $ fromMaybe [] (Map.lookup tier book.dimensions.byBand)

-- Get positions by term
getPositionsByTerm :: TermCommitment -> LendingBook -> Effect (Array Position)
getPositionsByTerm term bookRef = do
  book <- read bookRef
  let key = termToKey term
  pure $ fromMaybe [] (Map.lookup key book.dimensions.byTerm)

-- Get positions by leverage range
getPositionsByLeverage :: Number -> Number -> LendingBook -> Effect (Array Position)
getPositionsByLeverage minLev maxLev bookRef = do
  book <- read bookRef
  let relevantKeys = leverageRangeKeys minLev maxLev
      positions = concat $ map (\k -> fromMaybe [] (Map.lookup k book.dimensions.byLeverage)) relevantKeys
  pure positions

--------------------------------------------------------------------------------
-- Liquidity Aggregation
--------------------------------------------------------------------------------

-- Aggregate liquidity by band
aggregateLiquidityByBand :: TokenType -> LendingBook -> Effect (Map BandTier Number)
aggregateLiquidityByBand token bookRef = do
  book <- read bookRef
  let aggregator tier positions =
        let relevantPositions = filter (\p -> p.tokenPair.base == token || p.tokenPair.quote == token) positions
            totalLiquidity = sum (map _.amount relevantPositions)
        in totalLiquidity
  pure $ Map.mapMaybeWithKey (\tier positions -> Just (aggregator tier positions)) book.dimensions.byBand

-- Aggregate liquidity by term
aggregateLiquidityByTerm :: TokenType -> LendingBook -> Effect (Map String Number)
aggregateLiquidityByTerm token bookRef = do
  book <- read bookRef
  let aggregator termKey positions =
        let relevantPositions = filter (\p -> p.tokenPair.base == token || p.tokenPair.quote == token) positions
            totalLiquidity = sum (map _.amount relevantPositions)
        in totalLiquidity
  pure $ Map.mapMaybeWithKey (\termKey positions -> Just (aggregator termKey positions)) book.dimensions.byTerm

--------------------------------------------------------------------------------
-- Position Matching
--------------------------------------------------------------------------------

-- Match compatible positions
matchPositions :: Position -> LendingBook -> Effect (Array MatchResult)
matchPositions seekingPos bookRef = do
  book <- read bookRef
  -- Find positions that could match
  let candidates = filter (canMatchWith seekingPos) book.positions
      sorted = sortBy compareMatch candidates
      matches = map (createMatch seekingPos) sorted
  pure matches
  where
    -- Check if two positions can match
    canMatchWith pos1 pos2 =
      -- Same token pair
      pos1.tokenPair == pos2.tokenPair &&
      -- Compatible terms
      compatibleTerms pos1.term pos2.term &&
      -- Price bands overlap
      bandsOverlap pos1 pos2

    -- Compare positions for matching priority
    compareMatch p1 p2 = 
      compare p2.amount p1.amount  -- Larger positions first

    -- Create match result
    createMatch seeker provider =
      if seeker.amount <= provider.amount
      then FullMatch seeker provider
      else PartialMatch provider seeker (min seeker.amount provider.amount)

--------------------------------------------------------------------------------
-- POL Allocation
--------------------------------------------------------------------------------

-- Allocate protocol-owned liquidity across bands
allocatePOLAcrossBands :: 
  { totalPOL :: Number
  , stressMultiplier :: Number
  } -> LendingBook -> Effect (Map BandTier Number)
allocatePOLAcrossBands params bookRef = do
  book <- read bookRef
  let -- Base allocation weights
      baseWeights = Map.fromFoldable
        [ Tuple TightBand 0.2   -- 20% to tight bands
        , Tuple MediumBand 0.5  -- 50% to medium bands
        , Tuple WideBand 0.3    -- 30% to wide bands
        ]
      
      -- Adjust for stress (more to wider bands)
      stressAdjusted = if params.stressMultiplier > 1.0
        then Map.fromFoldable
          [ Tuple TightBand (0.2 / params.stressMultiplier)
          , Tuple MediumBand 0.4
          , Tuple WideBand (0.4 + 0.2 * (params.stressMultiplier - 1.0))
          ]
        else baseWeights
      
      -- Calculate allocations
      allocations = Map.mapMaybeWithKey 
        (\tier weight -> Just (params.totalPOL * weight)) 
        stressAdjusted
  
  -- Update book's POL allocation
  modify_ (\b -> b { polAllocation = allocations }) bookRef
  pure allocations

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Get band tier from position
getBandTier :: Position -> Maybe BandTier
getBandTier pos = case pos.priceStrategy of
  BandAligned band -> Just band.tier
  BandConstrained band _ -> Just band.tier
  TickSpecific _ -> Nothing

-- Convert term to string key
termToKey :: TermCommitment -> String
termToKey term = case term of
  Spot -> "spot"
  Hourly _ -> "hourly"
  Daily _ -> "daily"
  Weekly _ -> "weekly"

-- Convert leverage to range key
leverageToKey :: Number -> String
leverageToKey lev
  | lev == 1.0 = "1x"
  | lev <= 2.0 = "1-2x"
  | lev <= 5.0 = "2-5x"
  | otherwise = "5x+"

-- Get leverage range keys
leverageRangeKeys :: Number -> Number -> Array String
leverageRangeKeys minLev maxLev =
  filter inRange ["1x", "1-2x", "2-5x", "5x+"]
  where
    inRange "1x" = minLev <= 1.0 && maxLev >= 1.0
    inRange "1-2x" = minLev <= 2.0 && maxLev >= 1.0
    inRange "2-5x" = minLev <= 5.0 && maxLev >= 2.0
    inRange "5x+" = maxLev >= 5.0
    inRange _ = false

-- Check if terms are compatible
compatibleTerms :: TermCommitment -> TermCommitment -> Boolean
compatibleTerms Spot Spot = true
compatibleTerms (Hourly _) (Hourly _) = true
compatibleTerms (Daily _) (Daily _) = true
compatibleTerms (Weekly _) (Weekly _) = true
compatibleTerms _ _ = false

-- Check if price bands overlap
bandsOverlap :: Position -> Position -> Boolean
bandsOverlap pos1 pos2 =
  case Tuple (getTickRange pos1) (getTickRange pos2) of
    Tuple (Just r1) (Just r2) ->
      r1.upperTick >= r2.lowerTick && r2.upperTick >= r1.lowerTick
    _ -> false
  where
    getTickRange pos = case pos.priceStrategy of
      BandAligned band -> band.cachedBounds
      BandConstrained _ range -> Just range
      TickSpecific range -> Just range

--------------------------------------------------------------------------------
-- Additional Query Functions (from legacy LendingBook)
--------------------------------------------------------------------------------

-- Get total liquidity for a token pair
getTotalLiquidity :: TokenType -> TokenType -> LendingBook -> Effect Number
getTotalLiquidity base quote bookRef = do
  book <- read bookRef
  let relevantPositions = filter (\p -> 
        (p.tokenPair.base == base && p.tokenPair.quote == quote) ||
        (p.tokenPair.base == quote && p.tokenPair.quote == base)
      ) book.positions
      totalLiquidity = sum (map _.amount relevantPositions)
  pure totalLiquidity

-- Get user positions
getUserPositions :: String -> LendingBook -> Effect (Array Position)
getUserPositions owner bookRef = do
  book <- read bookRef
  pure $ filter (\p -> p.owner == owner) book.positions

-- Get all active positions
getActivePositions :: LendingBook -> Effect (Array Position)
getActivePositions bookRef = do
  book <- read bookRef
  pure book.positions  -- All positions are active in new model

-- Get next ID
getNextId :: LendingBook -> Effect Int
getNextId bookRef = do
  book <- read bookRef
  pure book.nextId


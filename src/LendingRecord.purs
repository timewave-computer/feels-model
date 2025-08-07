-- | Compatibility module for legacy code
-- | Maps old LendingRecord types to new Position system
module LendingRecord
  ( LendingRecord
  , LendingTerms(..)
  , UnbondingPeriod(..)
  , LendingSide(..)
  , LendingStatus(..)
  , createLenderRecord
  , createBorrowerRecord
  , validateLendingRecord
  , isLender
  , isBorrower
  , isAvailable
  , canMatch
  , getAvailableAmount
  , unbondingPeriodToDays
  ) where

import Prelude
import Data.Either (Either(..))
import Position (Position, TermCommitment(..), BandTier(..), createPosition, createSpotPosition)
import Token (TokenType)

-- Legacy type mapped to Position
type LendingRecord = Position

-- Legacy terms
data LendingTerms
  = SwapTerms
  | StakingTerms UnbondingPeriod
  | LeverageTerms Number

derive instance eqLendingTerms :: Eq LendingTerms

instance showLendingTerms :: Show LendingTerms where
  show SwapTerms = "Swap"
  show (StakingTerms period) = "Staking (" <> show period <> ")"
  show (LeverageTerms lev) = "Leverage (" <> show lev <> "x)"

-- Legacy unbonding period
data UnbondingPeriod
  = NoBonding
  | OneDay
  | SevenDays
  | FourteenDays

derive instance eqUnbondingPeriod :: Eq UnbondingPeriod
derive instance ordUnbondingPeriod :: Ord UnbondingPeriod

instance showUnbondingPeriod :: Show UnbondingPeriod where
  show NoBonding = "No bonding"
  show OneDay = "1 day"
  show SevenDays = "7 days"
  show FourteenDays = "14 days"

-- Legacy side
data LendingSide
  = Lender
  | Borrower

derive instance eqLendingSide :: Eq LendingSide

instance showLendingSide :: Show LendingSide where
  show Lender = "Lender"
  show Borrower = "Borrower"

-- Legacy status
data LendingStatus
  = Active
  | Unbonding Number
  | Available Number
  | Matched
  | Completed
  | Cancelled

derive instance eqLendingStatus :: Eq LendingStatus

instance showLendingStatus :: Show LendingStatus where
  show Active = "Active"
  show (Unbonding endTime) = "Unbonding (ends: " <> show endTime <> ")"
  show (Available endTime) = "Available (until: " <> show endTime <> ")"
  show Matched = "Matched"
  show Completed = "Completed"
  show Cancelled = "Cancelled"

-- Convert unbonding period to days
unbondingPeriodToDays :: UnbondingPeriod -> Number
unbondingPeriodToDays period = case period of
  NoBonding -> 0.0
  OneDay -> 1.0
  SevenDays -> 7.0
  FourteenDays -> 14.0

-- Create a lender record (now creates a Position)
createLenderRecord :: 
  Int -> 
  String -> 
  TokenType -> 
  Number -> 
  TokenType -> 
  Number -> 
  LendingTerms -> 
  Number -> 
  LendingRecord
createLenderRecord id owner lendAsset amount collateralAsset collateralRatio terms timestamp =
  createSpotPosition id owner amount 
    { base: lendAsset, quote: collateralAsset } 
    MediumBand 
    timestamp

-- Create a borrower record (now creates a Position)
createBorrowerRecord ::
  Int ->
  String ->
  TokenType ->
  Number ->
  TokenType ->
  Number ->
  LendingTerms ->
  Number ->
  LendingRecord
createBorrowerRecord id owner lendAsset amount collateralAsset collateralAmount terms timestamp =
  createSpotPosition id owner amount 
    { base: lendAsset, quote: collateralAsset } 
    MediumBand 
    timestamp

-- Validate lending record (always valid for compatibility)
validateLendingRecord :: LendingRecord -> Either String LendingRecord
validateLendingRecord record = Right record

-- Check if lender (always true for compatibility)
isLender :: LendingRecord -> Boolean
isLender _ = true

-- Check if borrower (always false for compatibility)
isBorrower :: LendingRecord -> Boolean
isBorrower _ = false

-- Check if available (always true for compatibility)
isAvailable :: LendingRecord -> Boolean
isAvailable _ = true

-- Check if can match (always true for compatibility)
canMatch :: LendingRecord -> LendingRecord -> Boolean
canMatch _ _ = true

-- Get available amount
getAvailableAmount :: LendingRecord -> Number
getAvailableAmount record = record.amount


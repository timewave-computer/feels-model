-- Unified lending record system for the "Everything is Lending" protocol.
-- Represents both sides of any lending operation - lenders (offering liquidity)
-- and borrowers (taking loans). This single type replaces the previous Position
-- and Tick separation, providing a cleaner conceptual model where all financial
-- operations are variations of lending.
module LendingRecord
  ( LendingRecord
  , LendingSide(..)
  , LendingTerms(..)
  , LendingStatus(..)
  , UnbondingPeriod(..)
  , createLenderRecord
  , createBorrowerRecord
  , validateLendingRecord
  , isLender
  , isBorrower
  , isAvailable
  , isActive
  , canMatch
  , compatibleTerms
  , getAvailableAmount
  , unbondingPeriodToDays
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Token (TokenType(..))
import Data.Int as Int
import Data.Number (abs)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- Which side of the lending operation
data LendingSide = Lender | Borrower

derive instance eqLendingSide :: Eq LendingSide

instance showLendingSide :: Show LendingSide where
  show Lender = "Lender"
  show Borrower = "Borrower"

-- Unbonding period for staking positions
data UnbondingPeriod = Infinite | Days30 | Days60 | Days90

derive instance eqUnbondingPeriod :: Eq UnbondingPeriod
derive instance ordUnbondingPeriod :: Ord UnbondingPeriod

instance showUnbondingPeriod :: Show UnbondingPeriod where
  show Infinite = "Infinite"
  show Days30 = "30 days"
  show Days60 = "60 days"
  show Days90 = "90 days"

-- Convert unbonding period to days
unbondingPeriodToDays :: UnbondingPeriod -> Int
unbondingPeriodToDays Infinite = 999999 -- Use a very large number to represent infinite
unbondingPeriodToDays Days30 = 30
unbondingPeriodToDays Days60 = 60
unbondingPeriodToDays Days90 = 90

-- Unified lending terms replacing Duration/ReturnType/TickType
data LendingTerms
  = SwapTerms                    -- Perpetual exchange (no yield)
  | StakingTerms UnbondingPeriod -- Fixed duration with dynamic yield
  | LeverageTerms Number         -- Leveraged position with multiplier

derive instance eqLendingTerms :: Eq LendingTerms

instance showLendingTerms :: Show LendingTerms where
  show SwapTerms = "Swap"
  show (StakingTerms period) = "Staking (" <> show period <> ")"
  show (LeverageTerms mult) = show mult <> "x Leverage"

-- Status of a lending record
data LendingStatus
  = Available Number    -- For lenders: amount still available
  | Matched            -- Paired but not yet executed
  | Active             -- Loan is active
  | Unbonding Number   -- Staking positions unbonding (timestamp)
  | Closed             -- Completed/withdrawn

derive instance eqLendingStatus :: Eq LendingStatus

instance showLendingStatus :: Show LendingStatus where
  show (Available amt) = "Available (" <> show amt <> ")"
  show Matched = "Matched"
  show Active = "Active"
  show (Unbonding time) = "Unbonding until " <> show time
  show Closed = "Closed"

-- Unified lending record
type LendingRecord =
  { id :: Int
  , side :: LendingSide
  , owner :: String
  , lendAsset :: TokenType
  , lendAmount :: Number
  , collateralAsset :: TokenType
  , collateralAmount :: Number      -- For Lender: required ratio, For Borrower: actual amount
  , terms :: LendingTerms
  , status :: LendingStatus
  , createdAt :: Number
  , matchedWith :: Maybe Int       -- ID of counterparty record
  , executedAt :: Maybe Number     -- When the match was executed
  }

--------------------------------------------------------------------------------
-- Record Creation
--------------------------------------------------------------------------------

-- Create a lender record (what was a "tick")
createLenderRecord :: 
  Int ->           -- ID
  String ->        -- Owner
  TokenType ->     -- Asset to lend
  Number ->        -- Amount available
  TokenType ->     -- Collateral required
  Number ->        -- Collateral ratio
  LendingTerms ->  -- Terms
  Number ->        -- Timestamp
  LendingRecord
createLenderRecord id owner lendAsset amount collateralAsset ratio terms timestamp =
  { id: id
  , side: Lender
  , owner: owner
  , lendAsset: lendAsset
  , lendAmount: amount
  , collateralAsset: collateralAsset
  , collateralAmount: ratio      -- For lenders, this is the ratio required
  , terms: terms
  , status: Available amount
  , createdAt: timestamp
  , matchedWith: Nothing
  , executedAt: Nothing
  }

-- Create a borrower record (what was a "position")
createBorrowerRecord ::
  Int ->           -- ID
  String ->        -- Owner
  TokenType ->     -- Asset to borrow
  Number ->        -- Amount to borrow
  TokenType ->     -- Collateral provided
  Number ->        -- Collateral amount
  LendingTerms ->  -- Terms
  Number ->        -- Timestamp
  LendingRecord
createBorrowerRecord id owner lendAsset amount collateralAsset collateralAmount terms timestamp =
  { id: id
  , side: Borrower
  , owner: owner
  , lendAsset: lendAsset
  , lendAmount: amount
  , collateralAsset: collateralAsset
  , collateralAmount: collateralAmount   -- For borrowers, this is actual amount
  , terms: terms
  , status: Active                        -- Borrowers start active once created
  , createdAt: timestamp
  , matchedWith: Nothing
  , executedAt: Nothing
  }

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- Validate a lending record
validateLendingRecord :: LendingRecord -> Either String Unit
validateLendingRecord record = do
  -- Amount validation
  when (record.lendAmount <= 0.0) $
    Left "Lend amount must be positive"
  
  -- Collateral validation
  when (record.collateralAmount <= 0.0) $
    Left $ if isLender record 
           then "Collateral ratio must be positive" 
           else "Collateral amount must be positive"
  
  -- Asset validation
  when (record.lendAsset == record.collateralAsset) $
    Left "Lend and collateral assets must be different"
  
  -- Terms-specific validation
  case record.terms of
    LeverageTerms mult ->
      when (mult < 1.0 || mult > 10.0) $
        Left "Leverage must be between 1x and 10x"
    _ -> Right unit
  
  -- Side-specific validation
  case record.side of
    Lender -> case record.status of
      Available amt -> 
        when (amt > record.lendAmount) $
          Left "Available amount cannot exceed total lend amount"
      _ -> Right unit
    Borrower -> case record.status of
      Available _ -> Left "Borrower records cannot have Available status"
      _ -> Right unit
  
  Right unit

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Check if record is a lender
isLender :: LendingRecord -> Boolean
isLender record = record.side == Lender

-- Check if record is a borrower
isBorrower :: LendingRecord -> Boolean
isBorrower record = record.side == Borrower

-- Check if record is available for matching
isAvailable :: LendingRecord -> Boolean
isAvailable record = case record.status of
  Available amt -> amt > 0.0
  _ -> false

-- Check if record is active
isActive :: LendingRecord -> Boolean
isActive record = case record.status of
  Active -> true
  Unbonding _ -> true  -- Unbonding is still considered active
  _ -> false

-- Check if two records can be matched
canMatch :: LendingRecord -> LendingRecord -> Boolean
canMatch lender borrower =
  isLender lender &&
  isBorrower borrower &&
  isAvailable lender &&
  lender.lendAsset == borrower.lendAsset &&
  lender.collateralAsset == borrower.collateralAsset &&
  compatibleTerms lender.terms borrower.terms &&  -- More flexible term matching
  case lender.status of
    Available amt -> amt >= borrower.lendAmount
    _ -> false

-- Check if lending terms are compatible (not necessarily identical)
compatibleTerms :: LendingTerms -> LendingTerms -> Boolean
compatibleTerms lenderTerms borrowerTerms = case lenderTerms, borrowerTerms of
  SwapTerms, SwapTerms -> true
  StakingTerms _, StakingTerms _ -> true  -- Any staking period matches
  LeverageTerms l1, LeverageTerms l2 -> abs (l1 - l2) < 0.5  -- Close leverage values match
  _, _ -> false

-- Get available amount for a lender record
getAvailableAmount :: LendingRecord -> Number
getAvailableAmount record = case record.status of
  Available amt -> amt
  _ -> 0.0
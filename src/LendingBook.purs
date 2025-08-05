-- Unified lending book for the "Everything is Lending" protocol.
-- Manages all lending records (both lender offers and borrower positions) in a single registry.
-- Provides matching, execution, and lifecycle management for all lending operations.
module LendingBook
  ( LendingBook
  , MatchResult
  , WithdrawalStatus(..)
  , initLendingBook
  , createLendOffer
  , takeLoan
  , getRecord
  , getLenderRecords
  , getBorrowerRecords
  , getUserRecords
  , getActiveRecords
  , getRecordsByAsset
  , getUserPositionsByAsset
  , matchAndExecute
  , initiateUnbonding
  , completeWithdrawal
  , checkWithdrawable
  , withdrawPosition
  , getMatchingOffers
  , getTotalLiquidity
  , getNextId
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array ((:), filter, find, foldr, sortBy, take, uncons, length)
import Data.Traversable (traverse)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Number (abs)
import Control.Monad (when)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Token (TokenType)
import LendingRecord (LendingRecord, LendingSide(..), LendingTerms(..), LendingStatus(..), 
                      UnbondingPeriod(..), unbondingPeriodToDays,
                      createLenderRecord, createBorrowerRecord, validateLendingRecord, 
                      isLender, isBorrower, isAvailable, canMatch, getAvailableAmount)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Status of position withdrawal
data WithdrawalStatus
  = NotUnbonding              -- Position is active, not unbonding
  | StillUnbonding Number     -- Days remaining in unbonding period
  | ReadyToWithdraw           -- Can be withdrawn now
  | NotApplicable             -- For perpetual positions
  | AlreadyClosed             -- Position already closed

derive instance eqWithdrawalStatus :: Eq WithdrawalStatus

instance showWithdrawalStatus :: Show WithdrawalStatus where
  show NotUnbonding = "Active (not unbonding)"
  show (StillUnbonding days) = "Unbonding (" <> show days <> " days remaining)"
  show ReadyToWithdraw = "Ready to withdraw"
  show NotApplicable = "N/A (Perpetual)"
  show AlreadyClosed = "Already closed"

-- The unified lending book
type LendingBook =
  { records :: Ref (Array LendingRecord)
  , nextId :: Ref Int
  , lastUpdate :: Ref Number
  }

-- Result of matching operation
type MatchResult =
  { borrowerRecord :: LendingRecord
  , lenderRecord :: LendingRecord
  , executedAmount :: Number
  , remainingOffer :: Maybe LendingRecord  -- If lender had excess liquidity
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize an empty lending book
initLendingBook :: Effect LendingBook
initLendingBook = do
  records <- new []
  nextId <- new 1
  lastUpdate <- new 0.0
  pure { records, nextId, lastUpdate }

--------------------------------------------------------------------------------
-- Core Operations
--------------------------------------------------------------------------------

-- Create a lending offer (what was "creating a tick")
createLendOffer ::
  LendingBook ->
  String ->        -- Owner
  TokenType ->     -- Asset to lend
  Number ->        -- Amount
  TokenType ->     -- Required collateral
  Number ->        -- Collateral ratio
  LendingTerms ->  -- Terms
  Effect (Either String LendingRecord)
createLendOffer book owner lendAsset amount collateralAsset ratio terms = do
  id <- getNextId book
  timestamp <- currentTime
  
  let record = createLenderRecord id owner lendAsset amount collateralAsset ratio terms timestamp
  
  case validateLendingRecord record of
    Left err -> pure $ Left err
    Right _ -> do
      addRecord book record
      pure $ Right record

-- Take a loan (what was "creating a position")
takeLoan ::
  LendingBook ->
  String ->        -- Borrower
  TokenType ->     -- Asset to borrow
  Number ->        -- Amount
  TokenType ->     -- Collateral provided
  Number ->        -- Collateral amount
  LendingTerms ->  -- Terms
  Effect (Either String MatchResult)
takeLoan book borrower lendAsset amount collateralAsset collateralAmount terms = do
  -- Find matching offers
  offers <- getMatchingOffers book lendAsset collateralAsset terms amount
  
  -- Log matching process
  log $ "TakeLoan: Looking for " <> show amount <> " " <> show lendAsset <> " with " <> show collateralAmount <> " " <> show collateralAsset
  log $ "  Terms: " <> show terms
  log $ "  Found " <> show (length offers) <> " matching offers"
  
  case offers of
    [] -> pure $ Left "No matching offers available"
    _ -> do
      let compareOffers a b = compare a.collateralAmount b.collateralAmount  -- Lower collateral requirement is better
          sortedOffers = sortBy compareOffers offers
      case uncons sortedOffers of
        Nothing -> pure $ Left "No matching offers available"
        Just { head: bestOffer, tail: _ } -> do
          -- Validate collateral ratio
          let requiredRatio = bestOffer.collateralAmount  -- For lenders, this is the ratio
              actualRatio = collateralAmount / amount
          
          -- Log collateral validation
          log $ "  Best offer requires ratio: " <> show requiredRatio <> " " <> show bestOffer.collateralAsset <> " per " <> show bestOffer.lendAsset
          log $ "  Borrower provides ratio: " <> show actualRatio <> " (amount: " <> show collateralAmount <> ")"
          
          if actualRatio < requiredRatio
            then pure $ Left $ "Insufficient collateral. Required ratio: " <> show requiredRatio <> ", provided: " <> show actualRatio
            else do
              id <- getNextId book
              timestamp <- currentTime
              
              let borrowerRecord = createBorrowerRecord id borrower lendAsset amount 
                                  collateralAsset collateralAmount terms timestamp
              
              case validateLendingRecord borrowerRecord of
                Left err -> pure $ Left err
                Right _ -> matchAndExecute book borrowerRecord bestOffer

--------------------------------------------------------------------------------
-- Matching and Execution
--------------------------------------------------------------------------------

-- Match and execute a loan
matchAndExecute :: 
  LendingBook -> 
  LendingRecord ->  -- Borrower
  LendingRecord ->  -- Lender
  Effect (Either String MatchResult)
matchAndExecute book borrower lender = do
  if not (canMatch lender borrower)
    then do
      -- Log why matching failed for debugging
      log $ "MATCH FAILED between lender #" <> show lender.id <> " and borrower #" <> show borrower.id
      log $ "  Lender: " <> show lender.lendAsset <> " -> " <> show lender.collateralAsset <> " terms: " <> show lender.terms
      log $ "  Borrower: " <> show borrower.lendAsset <> " -> " <> show borrower.collateralAsset <> " terms: " <> show borrower.terms
      pure $ Left "Records cannot be matched"
    else do
      timestamp <- currentTime
      let executedAmount = borrower.lendAmount
          remainingAmount = getAvailableAmount lender - executedAmount
      
      -- Update lender record
      let updatedLender = lender 
            { status = if remainingAmount > 0.0 
                      then Available remainingAmount 
                      else Matched
            , matchedWith = Just borrower.id
            , executedAt = Just timestamp
            }
      
      -- Update borrower record  
      let updatedBorrower = borrower
            { matchedWith = Just lender.id
            , executedAt = Just timestamp
            }
      
      -- Add borrower and update lender
      addRecord book updatedBorrower
      updateRecord book updatedLender
      
      -- Create remainder offer if needed
      remainingOffer <- if remainingAmount > 0.0
        then do
          id <- getNextId book
          let remainder = createLenderRecord id lender.owner lender.lendAsset 
                         remainingAmount lender.collateralAsset lender.collateralAmount 
                         lender.terms timestamp
          addRecord book remainder
          pure $ Just remainder
        else pure Nothing
      
      -- Update last activity
      write timestamp book.lastUpdate
      
      pure $ Right 
        { borrowerRecord: updatedBorrower
        , lenderRecord: updatedLender
        , executedAmount: executedAmount
        , remainingOffer: remainingOffer
        }

-- Get matching offers for a loan request
getMatchingOffers ::
  LendingBook ->
  TokenType ->     -- Lend asset
  TokenType ->     -- Collateral asset
  LendingTerms ->  -- Terms
  Number ->        -- Minimum amount needed
  Effect (Array LendingRecord)
getMatchingOffers book lendAsset collateralAsset terms minAmount = do
  lenders <- getLenderRecords book
  
  -- Import compatibleTerms from LendingRecord
  let compatibleTerms' = \t1 t2 -> case t1, t2 of
        SwapTerms, SwapTerms -> true
        StakingTerms _, StakingTerms _ -> true
        LeverageTerms l1, LeverageTerms l2 -> abs (l1 - l2) < 0.5
        _, _ -> false
  
  -- First filter by assets
  let assetMatching = filter (\r ->
        r.lendAsset == lendAsset &&
        r.collateralAsset == collateralAsset
      ) lenders
      
  -- Then filter by terms
  let termsMatching = filter (\r ->
        compatibleTerms' r.terms terms
      ) assetMatching
      
  -- Finally filter by amount
  let matching = filter (\r ->
        getAvailableAmount r >= minAmount
      ) termsMatching
      
  -- Log available offers for debugging
  when (length lenders > 0) $ do
    log $ "getMatchingOffers: Looking for " <> show lendAsset <> "/" <> show collateralAsset <> " " <> show terms
    log $ "  Total lender records: " <> show (length lenders)
    log $ "  Matching assets: " <> show (length assetMatching)
    log $ "  Matching terms: " <> show (length termsMatching)
    log $ "  Matching amount (>= " <> show minAmount <> "): " <> show (length matching)
  
  -- Sort by best collateral ratio (lowest first)
  pure $ sortBy (\a b -> compare a.collateralAmount b.collateralAmount) matching

--------------------------------------------------------------------------------
-- Lifecycle Management
--------------------------------------------------------------------------------

-- Initiate unbonding for staking positions
initiateUnbonding :: 
  LendingBook -> 
  Int ->  -- Record ID
  Effect (Either String LendingRecord)
initiateUnbonding book recordId = do
  maybeRecord <- getRecord book recordId
  case maybeRecord of
    Nothing -> pure $ Left "Record not found"
    Just record -> 
      if not (isBorrower record)
        then pure $ Left "Only borrower records can unbond"
        else case record.terms of
          StakingTerms period -> case record.status of
            Active -> do
              timestamp <- currentTime
              let unbondingDays = unbondingPeriodToDays period
                  unbondingComplete = timestamp + (Int.toNumber unbondingDays * 86400.0 * 1000.0)
                  updated = record { status = Unbonding unbondingComplete }
              updateRecord book updated
              pure $ Right updated
            _ -> pure $ Left "Can only unbond active positions"
          _ -> pure $ Left "Only staking positions can unbond"

-- Complete withdrawal for matured positions
completeWithdrawal ::
  LendingBook ->
  Int ->  -- Record ID
  Effect (Either String LendingRecord)
completeWithdrawal book recordId = do
  maybeRecord <- getRecord book recordId
  case maybeRecord of
    Nothing -> pure $ Left "Record not found"
    Just record -> case record.status of
      Unbonding timestamp -> do
        now <- currentTime
        if now >= timestamp
          then do
            let updated = record { status = Closed }
            updateRecord book updated
            pure $ Right updated
          else pure $ Left "Still unbonding"
      _ -> pure $ Left "Record not ready for withdrawal"

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- Get a specific record by ID
getRecord :: LendingBook -> Int -> Effect (Maybe LendingRecord)
getRecord book id = do
  records <- read book.records
  pure $ find (\r -> r.id == id) records

-- Get all lender records (available offers)
getLenderRecords :: LendingBook -> Effect (Array LendingRecord)
getLenderRecords book = do
  records <- read book.records
  pure $ filter (\r -> isLender r && isAvailable r) records

-- Get all borrower records (active positions)
getBorrowerRecords :: LendingBook -> String -> Effect (Array LendingRecord)
getBorrowerRecords book owner = do
  records <- read book.records
  pure $ filter (\r -> isBorrower r && r.owner == owner && r.status /= Closed) records

-- Get all records for a user
getUserRecords :: LendingBook -> String -> Effect (Array LendingRecord)
getUserRecords book owner = do
  records <- read book.records
  pure $ filter (\r -> r.owner == owner) records

-- Get all active records
getActiveRecords :: LendingBook -> Effect (Array LendingRecord)
getActiveRecords book = do
  records <- read book.records
  pure $ filter (\r -> case r.status of
    Active -> true
    Unbonding _ -> true
    Available _ -> true
    _ -> false
  ) records

-- Get records by asset type (useful for finding positions related to specific tokens)
getRecordsByAsset :: LendingBook -> TokenType -> Effect (Array LendingRecord)
getRecordsByAsset book asset = do
  records <- read book.records
  pure $ filter (\r -> r.lendAsset == asset || r.collateralAsset == asset) records

-- Get user positions by asset (positions where user is involved with specific asset)
getUserPositionsByAsset :: LendingBook -> String -> TokenType -> Effect (Array LendingRecord)
getUserPositionsByAsset book owner asset = do
  records <- read book.records
  pure $ filter (\r -> 
    r.owner == owner && 
    (r.lendAsset == asset || r.collateralAsset == asset)
  ) records

-- Get total liquidity for an asset pair
getTotalLiquidity :: 
  LendingBook -> 
  TokenType -> 
  TokenType -> 
  Effect Number
getTotalLiquidity book lendAsset collateralAsset = do
  lenders <- getLenderRecords book
  let matching = filter (\r -> 
        r.lendAsset == lendAsset && 
        r.collateralAsset == collateralAsset
      ) lenders
  pure $ sum (map getAvailableAmount matching)

--------------------------------------------------------------------------------
-- Internal Functions
--------------------------------------------------------------------------------

-- Get next ID
getNextId :: LendingBook -> Effect Int
getNextId book = do
  id <- read book.nextId
  _ <- write (id + 1) book.nextId
  pure id

-- Add a record
addRecord :: LendingBook -> LendingRecord -> Effect Unit
addRecord book record = 
  modify_ (\records -> record : records) book.records

-- Update a record
updateRecord :: LendingBook -> LendingRecord -> Effect Unit
updateRecord book updated =
  modify_ (map (\r -> if r.id == updated.id then updated else r)) book.records

--------------------------------------------------------------------------------
-- Withdrawal Functions
--------------------------------------------------------------------------------

-- Check if a lending record is withdrawable
checkWithdrawable :: LendingRecord -> Effect WithdrawalStatus
checkWithdrawable record = 
  if not (isBorrower record)
    then pure NotApplicable  -- Lenders don't withdraw, they wait for repayment
    else case record.terms of
      SwapTerms -> pure NotApplicable
      LeverageTerms _ -> pure NotApplicable
      StakingTerms _ -> case record.status of
        Active -> pure NotUnbonding
        Unbonding timestamp -> do
          now <- currentTime
          let daysRemaining = (timestamp - now) / (1000.0 * 60.0 * 60.0 * 24.0)
          if daysRemaining <= 0.0
            then pure ReadyToWithdraw
            else pure (StillUnbonding daysRemaining)
        Closed -> pure AlreadyClosed
        _ -> pure NotApplicable

-- Simplified withdrawPosition wrapper for UI
withdrawPosition :: forall r.
  { lendingBook :: LendingBook
  , userBalances :: Ref (Array { owner :: String, token :: TokenType, amount :: Number })
  , priceOracle :: TokenType -> Effect Number
  | r } ->
  Int ->
  Effect (Either String Unit)
withdrawPosition state recordId = do
  result <- completeWithdrawal state.lendingBook recordId
  case result of
    Left err -> pure $ Left err
    Right record -> do
      -- In a real implementation, this would handle balance updates
      -- For now, just return success
      pure $ Right unit
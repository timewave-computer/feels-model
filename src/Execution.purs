-- Execution engine for the unified lending protocol.
-- Handles loan execution, balance management, and fee processing for all lending operations.
-- Works with the unified LendingRecord/LendingBook system where everything is lending.
module Execution
  ( executeLoan
  , LoanResult
  , ExecutionError(..)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Data.Int as Int
import Effect (Effect)

import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingTerms(..), LendingStatus(..), LendingSide(..))
import LendingBook (LendingBook, takeLoan, MatchResult)
import Incentives (MarketDynamics, DynamicsResult, calculateDynamics)

import NFV (NFVState, contributeToNFV)
import BalanceManager (BalanceRegistry, checkBalance, updateBalance, transferBalance)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Result of loan execution
type LoanResult =
  { borrowerRecord :: LendingRecord
  , lenderRecord :: LendingRecord
  , collateralToken :: TokenType
  , feePaid :: Number
  , executedAmount :: Number
  , dynamics :: DynamicsResult  -- Market dynamics for transparency
  }

-- Execution errors
data ExecutionError
  = ValidationError String
  | InsufficientBalance String
  | NoMatchingOffers
  | MatchingError String
  | TransferError String

derive instance eqExecutionError :: Eq ExecutionError

instance showExecutionError :: Show ExecutionError where
  show (ValidationError msg) = "Validation Error: " <> msg
  show (InsufficientBalance msg) = "Insufficient Balance: " <> msg
  show NoMatchingOffers = "No matching offers available"
  show (MatchingError msg) = "Matching Error: " <> msg
  show (TransferError msg) = "Transfer Error: " <> msg

--------------------------------------------------------------------------------
-- Main Execution Function
--------------------------------------------------------------------------------

-- Execute a loan request
executeLoan :: forall r.
  { balances :: BalanceRegistry
  , lendingBook :: LendingBook
  , marketDynamics :: MarketDynamics
  , nfvState :: NFVState
  | r } ->
  String ->        -- Borrower
  TokenType ->     -- Asset to borrow
  Number ->        -- Amount
  TokenType ->     -- Collateral asset
  Number ->        -- Collateral amount
  LendingTerms ->  -- Terms
  Effect (Either ExecutionError LoanResult)
executeLoan state borrower lendAsset amount collateralAsset collateralAmount terms = do
  -- Step 1: Validate inputs
  case validateLoanRequest borrower lendAsset amount collateralAsset collateralAmount terms of
    Left err -> pure $ Left (ValidationError err)
    Right _ -> do
      
      -- Step 2: Check borrower has sufficient collateral
      hasCollateral <- checkBalance state.balances borrower collateralAsset collateralAmount
      if not hasCollateral
        then pure $ Left (InsufficientBalance "Insufficient collateral balance")
        else do
          
          -- Step 3: Calculate market dynamics (fees and returns)
          -- Create temporary record for dynamics calculation
          let tempRecord = 
                { id: 0
                , side: Borrower
                , owner: borrower
                , lendAsset: lendAsset
                , lendAmount: amount
                , collateralAsset: collateralAsset
                , collateralAmount: collateralAmount
                , terms: terms
                , status: Active
                , createdAt: 0.0
                , matchedWith: Nothing
                , executedAt: Nothing
                }
          
          dynamics <- calculateDynamics state.marketDynamics tempRecord
          let userFee = dynamics.borrowerRate * amount - dynamics.nfvFlow * amount
          
          -- Step 4: Check borrower can pay fees
          hasFees <- checkBalance state.balances borrower lendAsset userFee
          if not hasFees
            then pure $ Left (InsufficientBalance "Insufficient balance for fees")
            else do
              
              -- Step 5: Execute the loan through LendingBook
              matchResult <- takeLoan state.lendingBook borrower lendAsset amount 
                            collateralAsset collateralAmount terms
              
              case matchResult of
                Left err -> pure $ Left (MatchingError err)
                Right match -> do
                  
                  -- Step 6: Transfer collateral from borrower
                  collateralTransfer <- transferBalance state.balances borrower "protocol" 
                                       collateralAsset collateralAmount
                  case collateralTransfer of
                    Left err -> pure $ Left (TransferError err)
                    Right _ -> do
                      
                      -- Step 7: Transfer loan amount to borrower
                      loanTransfer <- transferBalance state.balances match.lenderRecord.owner 
                                     borrower lendAsset amount
                      case loanTransfer of
                        Left err -> pure $ Left (TransferError err)
                        Right _ -> do
                          
                          -- Step 8: Process fees
                          _ <- updateBalance state.balances borrower lendAsset (-userFee)
                          _ <- contributeToNFV state.nfvState dynamics.operationType 
                               (dynamics.nfvFlow * amount) (Just match.borrowerRecord.id)
                          
                          -- Step 9: Determine collateral token type
                          let collateralToken = determineCollateralToken collateralAsset terms
                          
                          pure $ Right
                            { borrowerRecord: match.borrowerRecord
                            , lenderRecord: match.lenderRecord
                            , collateralToken: collateralToken
                            , feePaid: userFee
                            , executedAmount: match.executedAmount
                            , dynamics: dynamics
                            }

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- Validate loan request parameters
validateLoanRequest ::
  String ->
  TokenType ->
  Number ->
  TokenType ->
  Number ->
  LendingTerms ->
  Either String Unit
validateLoanRequest borrower lendAsset amount collateralAsset collateralAmount terms = do
  -- Basic validation
  when (borrower == "") $ Left "Borrower cannot be empty"
  when (amount <= 0.0) $ Left "Amount must be positive"
  when (collateralAmount <= 0.0) $ Left "Collateral amount must be positive"
  when (lendAsset == collateralAsset) $ Left "Lend and collateral assets must be different"
  
  -- Terms-specific validation
  case terms of
    LeverageTerms mult ->
      when (mult < 1.0 || mult > 10.0) $ Left "Leverage must be between 1x and 10x"
    _ -> Right unit
  
  Right unit

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Determine collateral token type based on terms
determineCollateralToken :: TokenType -> LendingTerms -> TokenType
determineCollateralToken baseAsset terms = case terms of
  -- Swaps just use the base collateral
  SwapTerms -> baseAsset
  
  -- Staking creates receipt tokens
  StakingTerms period -> 
    Token ("Feels" <> show baseAsset <> "-" <> show period)
  
  -- Leverage creates synthetic tokens
  LeverageTerms mult ->
    Token (show (floor mult) <> "x" <> show baseAsset)
  where
    floor :: Number -> Int
    floor n = case round n of
      m | toNumber m <= n -> m
      m -> m - 1
    round :: Number -> Int
    round n = floor (n + 0.5)
    toNumber :: Int -> Number
    toNumber = Int.toNumber
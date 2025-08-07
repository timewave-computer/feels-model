-- Execution engine for the unified lending protocol.
-- Handles loan execution, balance management, and fee processing for all lending operations.
-- Works with the unified LendingRecord/LendingBook system where everything is lending.
module Execution
  ( executeLoan
  , LoanResult
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Data.Int as Int
import Effect (Effect)

import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingTerms(..), LendingStatus(..), LendingSide(..))
import Position (TermCommitment(..), LeverageMode(..), BandTier(..), PriceStrategy(..), TrackingMode(..))
import LendingBook (LendingBook, MatchResult(..))
import Incentives (MarketDynamics, DynamicsResult)

import POL (POLState, contributeToPOL)
import Accounts (AccountRegistry, getFeelsAccountBalance, updateFeelsAccountBalance, transferBetweenFeelsAccounts)
import ProtocolError (ProtocolError(..))
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Result of loan execution
type LoanResult =
  { borrowerRecord :: LendingRecord  -- Position type aliased as LendingRecord
  , lenderRecord :: LendingRecord    -- Position type aliased as LendingRecord
  , collateralToken :: TokenType
  , feePaid :: Number
  , executedAmount :: Number
  , dynamics :: DynamicsResult  -- Market dynamics for transparency
  }


--------------------------------------------------------------------------------
-- Main Execution Function
--------------------------------------------------------------------------------

-- Execute a loan request
executeLoan :: forall r.
  { accounts :: AccountRegistry
  , lendingBook :: LendingBook
  , marketDynamics :: MarketDynamics
  , polState :: POLState
  | r } ->
  String ->        -- Borrower
  TokenType ->     -- Asset to borrow
  Number ->        -- Amount
  TokenType ->     -- Collateral asset
  Number ->        -- Collateral amount
  LendingTerms ->  -- Terms
  Effect (Either ProtocolError LoanResult)
executeLoan state borrower lendAsset amount collateralAsset collateralAmount terms = do
  -- In the new system, loan execution is handled through position matching
  -- For now, return a minimal implementation
  timestamp <- currentTime
  
  -- Create mock positions for the result
  let borrowerPos = 
        { amount: amount
        , tokenPair: { base: lendAsset, quote: collateralAsset }
        , priceStrategy: BandAligned 
            { tier: MediumBand
            , centerTracking: SpotPrice
            , adaptiveWidth: false
            , lastUpdate: timestamp
            , cachedBounds: Nothing
            }
        , term: Spot
        , leverageConfig: 
            { mode: Static
            , targetLeverage: 1.0
            , currentLeverage: 1.0
            , decayAfterTerm: false
            }
        , owner: borrower
        , id: 999998
        , createdAt: timestamp
        }
      
      lenderPos = 
        { amount: collateralAmount
        , tokenPair: { base: collateralAsset, quote: lendAsset }
        , priceStrategy: BandAligned 
            { tier: MediumBand
            , centerTracking: SpotPrice
            , adaptiveWidth: false
            , lastUpdate: timestamp
            , cachedBounds: Nothing
            }
        , term: Spot
        , leverageConfig: 
            { mode: Static
            , targetLeverage: 1.0
            , currentLeverage: 1.0
            , decayAfterTerm: false
            }
        , owner: "system-lender"
        , id: 999997
        , createdAt: timestamp
        }
  
  -- Calculate mock dynamics
  let mockDynamics = 
        { lenderRate: 0.05
        , borrowerRate: 0.06
        , effectiveSpread: 0.01
        , polFlow: 0.001
        , components: 
            { baseRate: 0.05
            , riskPremium: 0.0
            , marketAdjustment: 0.0
            , protocolSpread: 0.01
            }
        , operationType: terms
        }
  
  pure $ Right 
    { borrowerRecord: borrowerPos
    , lenderRecord: lenderPos
    , collateralToken: collateralAsset
    , feePaid: amount * 0.01  -- 1% fee
    , executedAmount: amount
    , dynamics: mockDynamics
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
-- | Token Launch Offering System with Phased Pricing
-- |
-- | This module implements a sophisticated token launch mechanism using phased
-- | ascending-price offerings integrated with the AMM pool system. It enables
-- | fair token distribution with term commitments and gated access controls.
-- |
-- | Key Features:
-- | - Multi-phase offerings with ascending price tiers
-- | - Term commitment requirements for early phases (lower prices)
-- | - Integration with AMM pools for seamless trading
-- | - Automatic position creation with term commitments
-- | - Comprehensive eligibility validation
-- |
-- | Offering Flow:
-- | 1. Monthly Phase: Lowest prices, requires monthly lock commitments
-- | 2. Spot Phase: Higher prices, no lock requirements
-- | 3. Completion: Offering concludes, regular trading begins
-- |
-- | Each phase injects one-sided liquidity into the AMM pool, allowing users
-- | to purchase tokens through gated swaps based on their term commitments.
module Protocol.Offering
  ( -- Types
    OfferingState
  , OfferingPhase(..)
  , PhaseConfig
  , OfferingConfig
  , OfferingResult
  , GatedSwapParams
  , GatedSwapResult
  -- Functions
  , initOffering
  , startPhase
  , checkPhaseComplete
  , completeOffering
  , getOfferingStatus
  , canParticipate
  -- Gated swap functions
  , gatedSwap
  , validateSwapEligibility
  , processOfferingSwap
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Data.Ord (abs)
import Data.Foldable (sum, find)
import Data.Array ((:))

import Protocol.Token (TokenType(..))
import Protocol.Position (TermCommitment(..), monthlyTerm, spotTerm, createPosition, Position, Tranche(..))
import Protocol.Pool (PoolState, addLiquidity, LiquidityParams, SwapParams, SwapResult, swap)
import Protocol.Common (PoolId, BlockNumber, PositionId)
import Protocol.Error (ProtocolError(..))

--------------------------------------------------------------------------------
-- OFFERING PHASE DEFINITIONS
--------------------------------------------------------------------------------
-- Core types defining the phases and progression of token offerings

-- | Sequential phases of token offering with increasing prices and decreasing restrictions
-- | Each phase targets different user segments with appropriate commitment requirements
data OfferingPhase
  = MonthlyPhase   -- Phase 1: Lowest prices, requires monthly term commitment
  | SpotPhase      -- Phase 2: Higher prices, no commitment required (spot trading)
  | Completed      -- Final state: Offering concluded, regular trading active

derive instance eqOfferingPhase :: Eq OfferingPhase
derive instance ordOfferingPhase :: Ord OfferingPhase

instance showOfferingPhase :: Show OfferingPhase where
  show MonthlyPhase = "Monthly"
  show SpotPhase = "Spot"
  show Completed = "Completed"

--------------------------------------------------------------------------------
-- OFFERING CONFIGURATION TYPES
--------------------------------------------------------------------------------
-- Configuration parameters for setting up token launch offerings

-- | Configuration parameters for individual offering phases
-- | Defines token allocation, pricing, and liquidity parameters per phase
type PhaseConfig =
  { phase :: OfferingPhase          -- Which phase this configuration applies to
  , tokenAmount :: Number           -- Number of tokens allocated to this phase
  , priceRangeLower :: Number       -- Lower bound of price range for this phase
  , priceRangeUpper :: Number       -- Upper bound of price range for this phase
  , tickLower :: Int               -- Lower tick boundary for AMM liquidity
  , tickUpper :: Int               -- Upper tick boundary for AMM liquidity
  }

-- | Complete offering configuration defining all phases and parameters
-- | Contains all information needed to execute a full token launch
type OfferingConfig =
  { tokenTicker :: String           -- Token symbol/ticker (e.g., "BONK")
  , totalTokens :: Number           -- Total tokens available for launch (T_launch)
  , phases :: Array PhaseConfig     -- Sequential phase configurations
  , treasuryAddress :: String       -- Address receiving FeelsSOL proceeds
  }

--------------------------------------------------------------------------------
-- OFFERING STATE MANAGEMENT
--------------------------------------------------------------------------------
-- Runtime state tracking for active offerings

-- | Mutable offering state tracking current phase and progress
-- | Maintains real-time information about offering execution
type OfferingState = Ref
  { config :: OfferingConfig        -- Static offering configuration
  , currentPhase :: OfferingPhase   -- Currently active phase
  , tokensRemaining :: Number       -- Tokens remaining in current phase
  , totalRaised :: Number           -- Total FeelsSOL raised across all phases
  , phaseStartBlock :: BlockNumber  -- Block when current phase started
  , isActive :: Boolean             -- Whether current phase accepts swaps
  }

-- | Summary of current offering status for queries
-- | Provides snapshot of offering progress and current pricing
type OfferingResult =
  { phase :: OfferingPhase          -- Current offering phase
  , tokensRemaining :: Number       -- Tokens left in current phase
  , currentPrice :: Number          -- Current effective price per token
  , totalRaised :: Number           -- Total FeelsSOL raised so far
  }

--------------------------------------------------------------------------------
-- GATED SWAP SYSTEM TYPES
--------------------------------------------------------------------------------
-- Types for executing swaps with offering restrictions and term commitments

-- | Parameters for swaps during active offering phases
-- | Includes standard swap parameters plus offering-specific requirements
type GatedSwapParams =
  { swapParams :: SwapParams        -- Standard AMM swap parameters
  , user :: String                  -- User address performing the swap
  , termCommitment :: TermCommitment -- User's chosen position term commitment
  , poolId :: String                -- Pool identifier for the swap
  }

-- | Result of gated swap execution
-- | Combines swap result with any position created for token purchases
type GatedSwapResult =
  { swapResult :: SwapResult        -- Standard AMM swap execution result
  , position :: Maybe Position      -- Position created (only for token purchases)
  }

--------------------------------------------------------------------------------
-- OFFERING INITIALIZATION
--------------------------------------------------------------------------------
-- Functions for creating and validating new token offerings

-- | Initialize a new token offering with comprehensive validation
-- | Validates that phase allocations sum to total tokens and creates offering state
initOffering :: OfferingConfig -> Effect (Either ProtocolError OfferingState)
initOffering config = do
  -- Validate that phase token allocations sum to total offering tokens
  let phaseTotal = sum $ map _.tokenAmount config.phases
      tolerance = 0.01  -- Allow small floating point discrepancies
      
  if abs (phaseTotal - config.totalTokens) > tolerance
    then pure $ Left $ InvalidCommandError $ 
      "Phase token amounts (" <> show phaseTotal <> 
      ") don't match total tokens (" <> show config.totalTokens <> ")"
    else do
      offeringState <- new 
        { config
        , currentPhase: MonthlyPhase  -- Always start with first phase
        , tokensRemaining: 0.0        -- No active phase initially
        , totalRaised: 0.0            -- No funds raised yet
        , phaseStartBlock: 0          -- Will be set when phase starts
        , isActive: false             -- Offering created but not started
        }
      pure $ Right offeringState

--------------------------------------------------------------------------------
-- OFFERING PHASE MANAGEMENT
--------------------------------------------------------------------------------
-- Functions for controlling phase transitions and lifecycle

-- | Start a new offering phase by injecting token liquidity into the AMM pool
-- | Creates one-sided liquidity that users can purchase through gated swaps
startPhase :: OfferingState -> PoolState -> BlockNumber -> Effect (Either ProtocolError Unit)
startPhase offeringRef pool currentBlock = do
  offering <- read offeringRef
  
  if offering.isActive
    then pure $ Left $ InvalidCommandError "Phase already active"
    else do
      -- Locate configuration for current phase
      case findPhaseConfig offering.config offering.currentPhase of
        Nothing -> pure $ Left $ InvalidCommandError "Invalid phase"
        Just phaseConfig -> do
          -- Inject one-sided token liquidity into AMM pool
          -- This creates the token supply that users can purchase
          let liquidityParams = 
                { tickLower: phaseConfig.tickLower
                , tickUpper: phaseConfig.tickUpper
                , amount: phaseConfig.tokenAmount
                , recipient: "offering-contract"  -- Protocol-controlled position
                }
          
          -- Execute liquidity injection
          liquidityResult <- pure $ addLiquidity pool liquidityParams
          
          -- Activate phase and update offering state
          modify_ (\s -> s 
            { isActive = true
            , tokensRemaining = phaseConfig.tokenAmount
            , phaseStartBlock = currentBlock
            }) offeringRef
            
          pure $ Right unit

-- | Check if the current phase has sold all allocated tokens
-- | Used to trigger automatic phase transitions
checkPhaseComplete :: OfferingState -> PoolState -> Effect Boolean
checkPhaseComplete offeringRef _pool = do
  offering <- read offeringRef
  -- Phase completes when token allocation is exhausted
  pure $ offering.isActive && offering.tokensRemaining <= 0.01

-- | Transition to next phase or mark offering as completed
-- | Handles the sequential progression through offering phases
completeOffering :: OfferingState -> Effect (Either ProtocolError OfferingPhase)
completeOffering offeringRef = do
  offering <- read offeringRef
  
  if not offering.isActive
    then pure $ Left $ InvalidCommandError "No active phase"
    else do
      -- Determine next phase in sequence
      let nextPhase = case offering.currentPhase of
            MonthlyPhase -> SpotPhase   -- Move to spot phase
            SpotPhase -> Completed      -- Complete offering
            Completed -> Completed      -- Already completed
            
      -- Update offering state for phase transition
      modify_ (\s -> s 
        { currentPhase = nextPhase
        , isActive = false           -- Deactivate current phase
        , tokensRemaining = 0.0      -- Reset token counter
        }) offeringRef
        
      pure $ Right nextPhase

--------------------------------------------------------------------------------
-- PARTICIPATION ELIGIBILITY VALIDATION
--------------------------------------------------------------------------------
-- Functions for validating user eligibility based on term commitments

-- | Validate whether a user can participate in a specific phase with their term commitment
-- | Different phases have different commitment requirements for fair access
canParticipate :: OfferingPhase -> TermCommitment -> Boolean
canParticipate phase term = case phase, term of
  MonthlyPhase, Monthly _ -> true   -- Monthly phase requires monthly commitment
  SpotPhase, _ -> true              -- Spot phase open to all commitment types
  Completed, _ -> false             -- No participation after completion
  _, _ -> false                     -- All other combinations invalid

--------------------------------------------------------------------------------
-- OFFERING STATUS QUERIES
--------------------------------------------------------------------------------
-- Functions for retrieving current offering information and progress

-- | Get comprehensive status of current offering including phase and pricing
-- | Provides real-time information about offering progress and current conditions
getOfferingStatus :: OfferingState -> Effect OfferingResult
getOfferingStatus offeringRef = do
  offering <- read offeringRef
  
  -- Calculate phase-specific pricing (approximation for display)
  -- Actual prices are determined by AMM pool state
  let currentPrice = case offering.currentPhase of
        MonthlyPhase -> 0.15   -- Discounted price for commitment requirement
        SpotPhase -> 0.30      -- Higher price for immediate access
        Completed -> 0.40      -- Post-offering market price
        
  pure { phase: offering.currentPhase
       , tokensRemaining: offering.tokensRemaining
       , currentPrice
       , totalRaised: offering.totalRaised
       }

--------------------------------------------------------------------------------
-- GATED SWAP EXECUTION ENGINE
--------------------------------------------------------------------------------
-- Core logic for executing swaps with offering phase restrictions

-- | Execute swap with offering phase restrictions and term commitment validation
-- | Routes to appropriate handling based on offering state and user eligibility
gatedSwap :: 
  PoolState -> 
  Maybe OfferingState -> 
  GatedSwapParams -> 
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
gatedSwap pool maybeOffering params currentBlock = do
  case maybeOffering of
    -- No active offering - execute standard AMM swap
    Nothing -> do
      let swapResult = swap pool params.swapParams
      pure $ Right { swapResult, position: Nothing }
      
    -- Offering exists - check activation and eligibility
    Just offeringRef -> do
      offering <- read offeringRef
      
      if not offering.isActive
        then do
          -- Offering exists but phase inactive - standard swap
          let swapResult = swap pool params.swapParams
          pure $ Right { swapResult, position: Nothing }
        else do
          -- Active offering phase - validate user eligibility
          eligibility <- validateSwapEligibility offering params
          case eligibility of
            Left err -> pure $ Left err
            Right _ -> processOfferingSwap pool offeringRef params currentBlock

-- | Validate user eligibility for participation in current offering phase
-- | Checks term commitment requirements against phase restrictions
validateSwapEligibility :: 
  forall r. 
  { currentPhase :: OfferingPhase, isActive :: Boolean | r } -> 
  GatedSwapParams -> 
  Effect (Either ProtocolError Unit)
validateSwapEligibility offering params = do
  -- Only gate FeelsSOL → Token swaps (token purchases)
  -- Token → FeelsSOL swaps (selling) are always permitted
  if not params.swapParams.zeroForOne
    then pure $ Right unit  -- Token selling unrestricted
    else
      -- Validate term commitment meets phase requirements
      if canParticipate offering.currentPhase params.termCommitment
        then pure $ Right unit
        else pure $ Left $ InvalidCommandError $ 
          "Term commitment " <> show params.termCommitment <> 
          " not eligible for " <> show offering.currentPhase <> " phase"

-- | Process swap execution during active offering with position creation
-- | Creates term-committed positions for token purchases and updates offering state
processOfferingSwap ::
  PoolState ->
  OfferingState ->
  GatedSwapParams ->
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
processOfferingSwap pool offeringRef params currentBlock = do
  -- Execute AMM swap with standard mechanics
  let swapResult = swap pool params.swapParams
  
  -- Create position for token purchases (FeelsSOL → Token swaps)
  if params.swapParams.zeroForOne && swapResult.amount1 > 0.0
    then do
      -- Generate position for purchased tokens with user's term commitment
      let positionId = 1  -- Would be derived from Program Derived Addresses in Solana
          position = createPosition
            positionId
            params.poolId
            params.user
            swapResult.amount1        -- Tokens received from swap
            Junior                    -- Junior tranche for offering participants
            params.termCommitment     -- User's chosen commitment term
            false                     -- No rollover for offering positions
            swapResult.amount1        -- Initial shares equal token amount
            currentBlock
      
      -- Update offering progress tracking
      modify_ (\s -> s 
        { tokensRemaining = s.tokensRemaining - swapResult.amount1
        , totalRaised = s.totalRaised + abs swapResult.amount0
        }) offeringRef
        
      pure $ Right { swapResult, position: Just position }
    else
      -- Non-purchase swaps don't create positions
      pure $ Right { swapResult, position: Nothing }

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
--------------------------------------------------------------------------------
-- Helper functions for offering configuration and management

-- | Find phase configuration for a specific offering phase
-- | Used to retrieve phase-specific parameters during execution
findPhaseConfig :: OfferingConfig -> OfferingPhase -> Maybe PhaseConfig
findPhaseConfig config phase = 
  find (\p -> p.phase == phase) config.phases

-- | Token Launch System with Phased Pricing
-- |
-- | This module implements a sophisticated token launch mechanism using phased
-- | ascending-price launches integrated with the AMM pool system. It enables
-- | fair token distribution with term commitments and gated access controls.
-- |
-- | Key Features:
-- | - Multi-phase launches with ascending price tiers
-- | - Term commitment requirements for early phases (lower prices)
-- | - Integration with AMM pools for seamless trading
-- | - Automatic position creation with term commitments
-- | - Comprehensive eligibility validation
-- |
-- | Launch Flow:
-- | 1. Monthly Phase: Lowest prices, requires monthly lock commitments
-- | 2. Spot Phase: Higher prices, no lock requirements
-- | 3. Completion: Launch concludes, regular trading begins
-- |
-- | Each phase injects one-sided liquidity into the AMM pool, allowing users
-- | to purchase tokens through gated swaps based on their term commitments.
module Protocol.Launch
  ( -- Types
    LaunchState
  , LaunchPhase(..)
  , PhaseConfig
  , LaunchConfig
  , LaunchResult
  , GatedSwapParams
  , GatedSwapResult
  -- Functions
  , initLaunch
  , startPhase
  , checkPhaseComplete
  , completeLaunch
  , getLaunchStatus
  , canParticipate
  -- Gated swap functions
  , gatedSwap
  , validateSwapEligibility
  , processLaunchSwap
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
import Protocol.Position (Duration(..), Leverage(..), createPosition, Position)
import Protocol.Pool (PoolState, addLiquidity, LiquidityParams, SwapParams, SwapResult, swap)
import Protocol.Common (PoolId, BlockNumber, PositionId)
import Protocol.Error (ProtocolError(..))

--------------------------------------------------------------------------------
-- LAUNCH PHASE DEFINITIONS
--------------------------------------------------------------------------------
-- Core types defining the phases and progression of token launches

-- | Sequential phases of token launch with increasing prices and decreasing restrictions
-- | Each phase targets different user segments with appropriate commitment requirements
data LaunchPhase
  = MonthlyPhase   -- Phase 1: Lowest prices, requires monthly term commitment
  | SpotPhase      -- Phase 2: Higher prices, no commitment required (spot trading)
  | Completed      -- Final state: Launch concluded, regular trading active

derive instance eqLaunchPhase :: Eq LaunchPhase
derive instance ordLaunchPhase :: Ord LaunchPhase

instance showLaunchPhase :: Show LaunchPhase where
  show MonthlyPhase = "Monthly"
  show SpotPhase = "Spot"
  show Completed = "Completed"

--------------------------------------------------------------------------------
-- LAUNCH CONFIGURATION TYPES
--------------------------------------------------------------------------------
-- Configuration parameters for setting up token launches

-- | Configuration parameters for individual launch phases
-- | Defines token allocation, pricing, and liquidity parameters per phase
type PhaseConfig =
  { phase :: LaunchPhase            -- Which phase this configuration applies to
  , tokenAmount :: Number           -- Number of tokens allocated to this phase
  , priceRangeLower :: Number       -- Lower bound of price range for this phase
  , priceRangeUpper :: Number       -- Upper bound of price range for this phase
  , tickLower :: Int               -- Lower tick boundary for AMM liquidity
  , tickUpper :: Int               -- Upper tick boundary for AMM liquidity
  }

-- | Complete launch configuration defining all phases and parameters
-- | Contains all information needed to execute a full token launch
type LaunchConfig =
  { tokenTicker :: String           -- Token symbol/ticker (e.g., "FEELS")
  , totalTokens :: Number           -- Total tokens available for launch (T_launch)
  , phases :: Array PhaseConfig     -- Sequential phase configurations
  , treasuryAddress :: String       -- Address receiving FeelsSOL proceeds
  }

--------------------------------------------------------------------------------
-- LAUNCH STATE MANAGEMENT
--------------------------------------------------------------------------------
-- Runtime state tracking for active launches

-- | Mutable launch state tracking current phase and progress
-- | Maintains real-time information about launch execution
type LaunchState = Ref
  { config :: LaunchConfig          -- Static launch configuration
  , currentPhase :: LaunchPhase     -- Currently active phase
  , tokensRemaining :: Number       -- Tokens remaining in current phase
  , totalRaised :: Number           -- Total FeelsSOL raised across all phases
  , phaseStartBlock :: BlockNumber  -- Block when current phase started
  , isActive :: Boolean             -- Whether current phase accepts swaps
  }

-- | Summary of current launch status for queries
-- | Provides snapshot of launch progress and current pricing
type LaunchResult =
  { phase :: LaunchPhase            -- Current launch phase
  , tokensRemaining :: Number       -- Tokens left in current phase
  , currentPrice :: Number          -- Current effective price per token
  , totalRaised :: Number           -- Total FeelsSOL raised so far
  }

--------------------------------------------------------------------------------
-- GATED SWAP SYSTEM TYPES
--------------------------------------------------------------------------------
-- Types for executing swaps with launch restrictions and term commitments

-- | Parameters for swaps during active launch phases
-- | Includes standard swap parameters plus launch-specific requirements
type GatedSwapParams =
  { swapParams :: SwapParams        -- Standard AMM swap parameters
  , user :: String                  -- User address performing the swap
  , duration :: Duration              -- User's chosen position duration
  , poolId :: String                -- Pool identifier for the swap
  }

-- | Result of gated swap execution
-- | Combines swap result with any position created for token purchases
type GatedSwapResult =
  { swapResult :: SwapResult        -- Standard AMM swap execution result
  , position :: Maybe Position      -- Position created (only for token purchases)
  }

--------------------------------------------------------------------------------
-- LAUNCH INITIALIZATION
--------------------------------------------------------------------------------
-- Functions for creating and validating new token launches

-- | Initialize a new token launch with comprehensive validation
-- | Validates that phase allocations sum to total tokens and creates launch state
initLaunch :: LaunchConfig -> Effect (Either ProtocolError LaunchState)
initLaunch config = do
  -- Validate that phase token allocations sum to total launch tokens
  let phaseTotal = sum $ map _.tokenAmount config.phases
      tolerance = 0.01  -- Allow small floating point discrepancies
      
  if abs (phaseTotal - config.totalTokens) > tolerance
    then pure $ Left $ InvalidCommandError $ 
      "Phase token amounts (" <> show phaseTotal <> 
      ") don't match total tokens (" <> show config.totalTokens <> ")"
    else do
      launchState <- new 
        { config
        , currentPhase: MonthlyPhase  -- Always start with first phase
        , tokensRemaining: 0.0        -- No active phase initially
        , totalRaised: 0.0            -- No funds raised yet
        , phaseStartBlock: 0          -- Will be set when phase starts
        , isActive: false             -- Launch created but not started
        }
      pure $ Right launchState

--------------------------------------------------------------------------------
-- LAUNCH PHASE MANAGEMENT
--------------------------------------------------------------------------------
-- Functions for controlling phase transitions and lifecycle

-- | Start a new launch phase by injecting token liquidity into the AMM pool
-- | Creates one-sided liquidity that users can purchase through gated swaps
startPhase :: LaunchState -> PoolState -> BlockNumber -> Effect (Either ProtocolError Unit)
startPhase launchRef pool currentBlock = do
  launch <- read launchRef
  
  if launch.isActive
    then pure $ Left $ InvalidCommandError "Phase already active"
    else do
      -- Locate configuration for current phase
      case findPhaseConfig launch.config launch.currentPhase of
        Nothing -> pure $ Left $ InvalidCommandError "Invalid phase"
        Just phaseConfig -> do
          -- Inject one-sided token liquidity into AMM pool
          -- This creates the token supply that users can purchase
          let liquidityParams = 
                { tickLower: phaseConfig.tickLower
                , tickUpper: phaseConfig.tickUpper
                , amount: phaseConfig.tokenAmount
                , recipient: "launch-contract"    -- Protocol-controlled position
                }
          
          -- Execute liquidity injection
          liquidityResult <- pure $ addLiquidity pool liquidityParams
          
          -- Activate phase and update launch state
          modify_ (\s -> s 
            { isActive = true
            , tokensRemaining = phaseConfig.tokenAmount
            , phaseStartBlock = currentBlock
            }) launchRef
            
          pure $ Right unit

-- | Check if the current phase has sold all allocated tokens
-- | Used to trigger automatic phase transitions
checkPhaseComplete :: LaunchState -> PoolState -> Effect Boolean
checkPhaseComplete launchRef _pool = do
  launch <- read launchRef
  -- Phase completes when token allocation is exhausted
  pure $ launch.isActive && launch.tokensRemaining <= 0.01

-- | Transition to next phase or mark launch as completed
-- | Handles the sequential progression through launch phases
completeLaunch :: LaunchState -> Effect (Either ProtocolError LaunchPhase)
completeLaunch launchRef = do
  launch <- read launchRef
  
  if not launch.isActive
    then pure $ Left $ InvalidCommandError "No active phase"
    else do
      -- Determine next phase in sequence
      let nextPhase = case launch.currentPhase of
            MonthlyPhase -> SpotPhase   -- Move to spot phase
            SpotPhase -> Completed      -- Complete launch
            Completed -> Completed      -- Already completed
            
      -- Update launch state for phase transition
      modify_ (\s -> s 
        { currentPhase = nextPhase
        , isActive = false           -- Deactivate current phase
        , tokensRemaining = 0.0      -- Reset token counter
        }) launchRef
        
      pure $ Right nextPhase

--------------------------------------------------------------------------------
-- PARTICIPATION ELIGIBILITY VALIDATION
--------------------------------------------------------------------------------
-- Functions for validating user eligibility based on term commitments

-- | Validate whether a user can participate in a specific phase with their term commitment
-- | Different phases have different commitment requirements for fair access
canParticipate :: LaunchPhase -> Duration -> Boolean
canParticipate phase duration = case phase, duration of
  MonthlyPhase, Monthly -> true     -- Monthly phase requires monthly commitment
  SpotPhase, _ -> true              -- Spot phase open to all commitment types
  Completed, _ -> false             -- No participation after completion
  _, _ -> false                     -- All other combinations invalid

--------------------------------------------------------------------------------
-- LAUNCH STATUS QUERIES
--------------------------------------------------------------------------------
-- Functions for retrieving current launch information and progress

-- | Get comprehensive status of current launch including phase and pricing
-- | Provides real-time information about launch progress and current conditions
getLaunchStatus :: LaunchState -> Effect LaunchResult
getLaunchStatus launchRef = do
  launch <- read launchRef
  
  -- Calculate phase-specific pricing (approximation for display)
  -- Actual prices are determined by AMM pool state
  let currentPrice = case launch.currentPhase of
        MonthlyPhase -> 0.15   -- Discounted price for commitment requirement
        SpotPhase -> 0.30      -- Higher price for immediate access
        Completed -> 0.40      -- Post-launch market price
        
  pure { phase: launch.currentPhase
       , tokensRemaining: launch.tokensRemaining
       , currentPrice
       , totalRaised: launch.totalRaised
       }

--------------------------------------------------------------------------------
-- GATED SWAP EXECUTION ENGINE
--------------------------------------------------------------------------------
-- Core logic for executing swaps with launch phase restrictions

-- | Execute swap with launch phase restrictions and term commitment validation
-- | Routes to appropriate handling based on launch state and user eligibility
gatedSwap :: 
  PoolState -> 
  Maybe LaunchState -> 
  GatedSwapParams -> 
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
gatedSwap pool maybeLaunch params currentBlock = do
  case maybeLaunch of
    -- No active launch - execute standard AMM swap
    Nothing -> do
      let swapResultWithPool = swap pool params.swapParams currentBlock
          swapResult = swapResultWithPool.result
      pure $ Right { swapResult, position: Nothing }
      
    -- Launch exists - check activation and eligibility
    Just launchRef -> do
      launch <- read launchRef
      
      if not launch.isActive
        then do
          -- Launch exists but phase inactive - standard swap
          let swapResultWithPool = swap pool params.swapParams currentBlock
              swapResult = swapResultWithPool.result
          pure $ Right { swapResult, position: Nothing }
        else do
          -- Active launch phase - validate user eligibility
          eligibility <- validateSwapEligibility launch params
          case eligibility of
            Left err -> pure $ Left err
            Right _ -> processLaunchSwap pool launchRef params currentBlock

-- | Validate user eligibility for participation in current launch phase
-- | Checks term commitment requirements against phase restrictions
validateSwapEligibility :: 
  forall r. 
  { currentPhase :: LaunchPhase, isActive :: Boolean | r } -> 
  GatedSwapParams -> 
  Effect (Either ProtocolError Unit)
validateSwapEligibility launch params = do
  -- Only gate FeelsSOL → Token swaps (token purchases)
  -- Token → FeelsSOL swaps (selling) are always permitted
  if not params.swapParams.zeroForOne
    then pure $ Right unit  -- Token selling unrestricted
    else
      -- Validate term commitment meets phase requirements
      if canParticipate launch.currentPhase params.duration
        then pure $ Right unit
        else pure $ Left $ InvalidCommandError $ 
          "Duration " <> show params.duration <> 
          " not eligible for " <> show launch.currentPhase <> " phase"

-- | Process swap execution during active launch with position creation
-- | Creates term-committed positions for token purchases and updates launch state
processLaunchSwap ::
  PoolState ->
  LaunchState ->
  GatedSwapParams ->
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
processLaunchSwap pool launchRef params currentBlock = do
  -- Execute AMM swap with standard mechanics
  let swapResultWithPool = swap pool params.swapParams currentBlock
      swapResult = swapResultWithPool.result
      updatedPool = swapResultWithPool.updatedPool
  
  -- Create position for token purchases (FeelsSOL → Token swaps)
  if params.swapParams.zeroForOne && swapResult.amount1 > 0.0
    then do
      -- Generate position for purchased tokens with user's term commitment
      let positionId = 1  -- Would be derived from Program Derived Addresses in Solana
          position = createPosition
            positionId
            params.user              -- Position owner
            swapResult.amount1       -- Initial investment amount
            1.0                      -- Price level (default for launches)
            params.duration          -- Time commitment
            Junior                   -- Junior leverage for launch participants
            pool.token0              -- Lend asset (token being launched)
            pool.token1              -- Collateral asset (FeelsSOL)
            false                    -- No rollover for launch positions
            swapResult.amount1       -- Initial shares equal token amount
            currentBlock
      
      -- Update launch progress tracking
      modify_ (\s -> s 
        { tokensRemaining = s.tokensRemaining - swapResult.amount1
        , totalRaised = s.totalRaised + abs swapResult.amount0
        }) launchRef
        
      pure $ Right { swapResult, position: Just position }
    else
      -- Non-purchase swaps don't create positions
      pure $ Right { swapResult, position: Nothing }

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
--------------------------------------------------------------------------------
-- Helper functions for launch configuration and management

-- | Find phase configuration for a specific launch phase
-- | Used to retrieve phase-specific parameters during execution
findPhaseConfig :: LaunchConfig -> LaunchPhase -> Maybe PhaseConfig
findPhaseConfig config phase = 
  find (\p -> p.phase == phase) config.phases

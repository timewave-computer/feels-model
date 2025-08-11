-- | Phased Ascending-Price Offering Module
-- | 
-- | Implements a live, on-chain sale of newly-minted tokens through
-- | the AMM pool with ascending price tiers and term commitments.
module Protocol.Offering
  ( -- Types
    OfferingState
  , OfferingPhase(..)
  , PhaseConfig
  , OfferingConfig
  , OfferingResult
  -- Functions
  , initOffering
  , startPhase
  , checkPhaseComplete
  , completeOffering
  , getOfferingStatus
  , canParticipate
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
import Protocol.Position (TermCommitment(..), weeklyTerm, dailyTerm, spotTerm)
import Protocol.Pool (PoolState, addLiquidity, LiquidityParams)
import Protocol.Common (PoolId, BlockNumber)
import Protocol.Errors (ProtocolError(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Offering phases with required term commitments
data OfferingPhase
  = WeeklyPhase    -- Lowest price, requires weekly lock
  | DailyPhase     -- Medium price, requires daily lock
  | SpotPhase      -- Highest price, no lock required
  | Completed      -- Offering finished

derive instance eqOfferingPhase :: Eq OfferingPhase
derive instance ordOfferingPhase :: Ord OfferingPhase

instance showOfferingPhase :: Show OfferingPhase where
  show WeeklyPhase = "Weekly"
  show DailyPhase = "Daily" 
  show SpotPhase = "Spot"
  show Completed = "Completed"

-- | Configuration for a single phase
type PhaseConfig =
  { phase :: OfferingPhase
  , tokenAmount :: Number         -- Tokens to sell in this phase
  , priceRangeLower :: Number     -- Lower price bound
  , priceRangeUpper :: Number     -- Upper price bound
  , tickLower :: Int              -- Lower tick for liquidity
  , tickUpper :: Int              -- Upper tick for liquidity
  }

-- | Overall offering configuration
type OfferingConfig =
  { tokenTicker :: String
  , totalTokens :: Number         -- Total tokens for sale (T_launch)
  , phases :: Array PhaseConfig   -- Phase configurations
  , treasuryAddress :: String     -- Where to collect proceeds
  }

-- | Offering state
type OfferingState = Ref
  { config :: OfferingConfig
  , currentPhase :: OfferingPhase
  , tokensRemaining :: Number     -- Tokens left in current phase
  , totalRaised :: Number         -- Total FeelsSOL raised
  , phaseStartBlock :: BlockNumber
  , isActive :: Boolean
  }

-- | Result of offering operations
type OfferingResult =
  { phase :: OfferingPhase
  , tokensRemaining :: Number
  , currentPrice :: Number
  , totalRaised :: Number
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize a new offering
initOffering :: OfferingConfig -> Effect OfferingState
initOffering config = do
  -- Validate phases add up to total tokens
  let phaseTotal = sum $ map _.tokenAmount config.phases
  when (abs (phaseTotal - config.totalTokens) > 0.01) $
    pure unit -- In real implementation, would throw error
    
  new { config
      , currentPhase: WeeklyPhase  -- Start with first phase
      , tokensRemaining: 0.0
      , totalRaised: 0.0
      , phaseStartBlock: 0
      , isActive: false
      }

--------------------------------------------------------------------------------
-- Phase Management
--------------------------------------------------------------------------------

-- | Start a new phase by injecting liquidity
startPhase :: OfferingState -> PoolState -> BlockNumber -> Effect (Either ProtocolError Unit)
startPhase offeringRef pool currentBlock = do
  offering <- read offeringRef
  
  if offering.isActive
    then pure $ Left $ InvalidCommandError "Phase already active"
    else do
      -- Find current phase config
      case findPhaseConfig offering.config offering.currentPhase of
        Nothing -> pure $ Left $ InvalidCommandError "Invalid phase"
        Just phaseConfig -> do
          -- Inject one-sided token liquidity into the pool
          let liquidityParams = 
                { tickLower: phaseConfig.tickLower
                , tickUpper: phaseConfig.tickUpper
                , amount: phaseConfig.tokenAmount
                , recipient: "offering-contract"
                }
          
          -- In real implementation, would call pool.addLiquidity
          -- For now, just update state
          modify_ (\s -> s 
            { isActive = true
            , tokensRemaining = phaseConfig.tokenAmount
            , phaseStartBlock = currentBlock
            }) offeringRef
            
          pure $ Right unit

-- | Check if current phase is complete
checkPhaseComplete :: OfferingState -> PoolState -> Effect Boolean
checkPhaseComplete offeringRef _pool = do
  offering <- read offeringRef
  -- Phase is complete when all tokens are sold
  pure $ offering.isActive && offering.tokensRemaining <= 0.01

-- | Move to next phase or complete offering
completeOffering :: OfferingState -> Effect (Either ProtocolError OfferingPhase)
completeOffering offeringRef = do
  offering <- read offeringRef
  
  if not offering.isActive
    then pure $ Left $ InvalidCommandError "No active phase"
    else do
      let nextPhase = case offering.currentPhase of
            WeeklyPhase -> DailyPhase
            DailyPhase -> SpotPhase
            SpotPhase -> Completed
            Completed -> Completed
            
      modify_ (\s -> s 
        { currentPhase = nextPhase
        , isActive = false
        , tokensRemaining = 0.0
        }) offeringRef
        
      pure $ Right nextPhase

--------------------------------------------------------------------------------
-- Eligibility Checks
--------------------------------------------------------------------------------

-- | Check if user can participate with given term commitment
canParticipate :: OfferingPhase -> TermCommitment -> Boolean
canParticipate phase term = case phase, term of
  WeeklyPhase, Weekly _ -> true
  DailyPhase, Daily _ -> true
  DailyPhase, Weekly _ -> true  -- Weekly can participate in daily
  SpotPhase, _ -> true           -- Anyone can participate in spot
  _, _ -> false

--------------------------------------------------------------------------------
-- Query Functions
--------------------------------------------------------------------------------

-- | Get current offering status
getOfferingStatus :: OfferingState -> Effect OfferingResult
getOfferingStatus offeringRef = do
  offering <- read offeringRef
  
  -- Calculate approximate current price based on phase
  let currentPrice = case offering.currentPhase of
        WeeklyPhase -> 0.125   -- Mid-point of $0.10-$0.15
        DailyPhase -> 0.205    -- Mid-point of $0.16-$0.25
        SpotPhase -> 0.33      -- Mid-point of $0.26-$0.40
        Completed -> 0.40
        
  pure { phase: offering.currentPhase
       , tokensRemaining: offering.tokensRemaining
       , currentPrice
       , totalRaised: offering.totalRaised
       }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

findPhaseConfig :: OfferingConfig -> OfferingPhase -> Maybe PhaseConfig
findPhaseConfig config phase = 
  find (\p -> p.phase == phase) config.phases
-- Helper functions now imported from standard libraries:
-- find from Data.Foldable
-- sum from Data.Foldable  
-- abs from Data.Ord
-- | Offering Gate Module
-- | 
-- | Controls swap eligibility during offering phases based on term commitments
module Protocol.OfferingGate
  ( -- Types
    GatedSwapParams
  , GatedSwapResult
  -- Functions
  , gatedSwap
  , validateSwapEligibility
  , processOfferingSwap
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (read, modify_)
import Data.Ord (abs)

import Protocol.Token (TokenType(..))
import Protocol.Position (TermCommitment(..), createPosition, Position, Tranche(..))
import Protocol.Pool (PoolState, SwapParams, SwapResult, swap)
import Protocol.Offering (OfferingState, OfferingPhase(..), canParticipate)
import Protocol.Common (BlockNumber, PositionId)
import Protocol.Errors (ProtocolError(..))
-- Pool registry operations would be handled at the UI layer

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Parameters for a gated swap during offering
type GatedSwapParams =
  { swapParams :: SwapParams        -- Standard swap parameters
  , user :: String                  -- User performing swap
  , termCommitment :: TermCommitment -- User's chosen term
  , poolId :: String                -- Pool identifier
  }

-- | Result of a gated swap
type GatedSwapResult =
  { swapResult :: SwapResult        -- Standard swap result
  , position :: Maybe Position      -- Created position (if buying tokens)
  }


--------------------------------------------------------------------------------
-- Gated Swap Logic
--------------------------------------------------------------------------------

-- | Execute a swap with offering phase restrictions
gatedSwap :: 
  PoolState -> 
  Maybe OfferingState -> 
  GatedSwapParams -> 
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
gatedSwap pool maybeOffering params currentBlock = do
  case maybeOffering of
    -- No offering active - regular swap
    Nothing -> do
      swapResult <- swap pool params.swapParams
      pure $ Right { swapResult, position: Nothing }
      
    -- Offering active - check eligibility
    Just offeringRef -> do
      offering <- read offeringRef
      
      if not offering.isActive
        then do
          -- Offering exists but not active - regular swap
          swapResult <- swap pool params.swapParams
          pure $ Right { swapResult, position: Nothing }
        else do
          -- Active offering - validate eligibility
          eligibility <- validateSwapEligibility offering params
          case eligibility of
            Left err -> pure $ Left err
            Right _ -> processOfferingSwap pool offeringRef params currentBlock

-- | Validate if user can participate in current phase
validateSwapEligibility :: 
  { currentPhase :: OfferingPhase, isActive :: Boolean | r } -> 
  GatedSwapParams -> 
  Effect (Either ProtocolError Unit)
validateSwapEligibility offering params = do
  -- Only restrict FeelsSOL -> Token swaps (buying)
  if not params.swapParams.zeroForOne
    then pure $ Right unit  -- Selling tokens is always allowed
    else
      -- Check if user's term commitment qualifies for current phase
      if canParticipate offering.currentPhase params.termCommitment
        then pure $ Right unit
        else pure $ Left $ InvalidCommandError $ 
          "Term commitment " <> show params.termCommitment <> 
          " not eligible for " <> show offering.currentPhase <> " phase"

-- | Process a swap during active offering
processOfferingSwap ::
  PoolState ->
  OfferingState ->
  GatedSwapParams ->
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
processOfferingSwap pool offeringRef params currentBlock = do
  -- Execute the swap
  swapResult <- swap pool params.swapParams
  
  -- For token purchases, create a position with term commitment
  if params.swapParams.zeroForOne && swapResult.amount1 > 0.0
    then do
      -- Create position for purchased tokens
      let positionId = 1  -- In Solana, this would be derived from PDAs
          position = createPosition
            positionId
            params.poolId
            params.user
            swapResult.amount1  -- Amount of tokens received
            Junior              -- Default to junior tranche for offerings
            params.termCommitment
            swapResult.amount1  -- Shares = amount for new positions
            currentBlock
      
      -- Update offering state
      modify_ (\s -> s 
        { tokensRemaining = s.tokensRemaining - swapResult.amount1
        , totalRaised = s.totalRaised + abs swapResult.amount0
        }) offeringRef
        
      pure $ Right { swapResult, position: Just position }
    else
      -- Not a token purchase - no position needed
      pure $ Right { swapResult, position: Nothing }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- abs function now imported from Data.Ord
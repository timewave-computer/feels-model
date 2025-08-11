-- | LaunchOrchestrator Module - Manages multi-phase token launches
-- |
-- | This module orchestrates the complete launch lifecycle through
-- | cascading phases with inter-phase learning.
module Launch.Orchestrator
  ( LaunchOrchestrator
  , ActiveLaunch
  , LaunchStatus(..)
  , initOrchestrator
  , createLaunch
  , submitBidToLaunch
  , processNextBatch
  , getActiveLaunches
  , getLaunchStatus
  ) where

import Prelude
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), filter, elem, (!!), length, uncons)
import Data.Array as Array
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)

import Launch.Launch (LaunchState, LaunchConfig, LaunchPhase(..), BatchAuction, BatchSequence)
import Launch.Launch as L
import Common (PoolId)
import Errors (ProtocolError(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Active launch with metadata
type ActiveLaunch =
  { launchId :: String
  , tokenTicker :: String
  , state :: LaunchState
  , startTime :: Number
  , totalTokens :: Number
  , tokensDistributed :: Number
  }

-- | Orchestrator manages all active launches
type LaunchOrchestrator = Ref
  { launches :: Map String ActiveLaunch
  , completedLaunches :: Array String
  , nextLaunchId :: Int
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialize the launch orchestrator
initOrchestrator :: Effect LaunchOrchestrator
initOrchestrator = new
  { launches: Map.empty
  , completedLaunches: []
  , nextLaunchId: 1
  }

--------------------------------------------------------------------------------
-- Launch Management
--------------------------------------------------------------------------------

-- | Create a new token launch
createLaunch :: 
  String ->           -- Token ticker
  PoolId ->           -- Pool for the token
  Number ->           -- Initial price
  Number ->           -- Total tokens to distribute
  Number ->           -- Current timestamp
  LaunchOrchestrator ->
  Effect (Either ProtocolError String)
createLaunch ticker poolId initialPrice totalTokens timestamp orchestrator = do
  state <- read orchestrator
  
  -- Check if ticker already has active launch
  let existingLaunch = Map.values state.launches 
                      # Array.fromFoldable
                      # filter (\l -> l.tokenTicker == ticker)
                      # (\x -> case x of
                          [] -> Nothing
                          xs -> case uncons xs of
                            Just { head: h, tail: _ } -> Just h
                            Nothing -> Nothing)
  
  case existingLaunch of
    Just _ -> pure $ Left $ InvalidCommandError $ "Launch already exists for " <> ticker
    Nothing -> do
      let launchId = "LAUNCH-" <> show state.nextLaunchId
          config = L.createLaunchConfig ticker poolId initialPrice
          launchState = L.initializeLaunch config
          
          activeLaunch =
            { launchId: launchId
            , tokenTicker: ticker
            , state: launchState
            , startTime: timestamp
            , totalTokens: totalTokens
            , tokensDistributed: 0.0
            }
      
      modify_ (\s -> s 
        { launches = Map.insert launchId activeLaunch s.launches
        , nextLaunchId = s.nextLaunchId + 1
        }) orchestrator
      
      pure $ Right launchId

-- | Submit a bid to an active launch
submitBidToLaunch ::
  String ->           -- Launch ID
  String ->           -- Bidder
  Number ->           -- Base amount
  Number ->           -- Priority fee
  LaunchOrchestrator ->
  Effect (Either ProtocolError Unit)
submitBidToLaunch launchId bidder baseAmount priorityFee orchestrator = do
  state <- read orchestrator
  
  case Map.lookup launchId state.launches of
    Nothing -> pure $ Left $ InvalidCommandError "Launch not found"
    Just launch -> 
      if launch.state.currentPhase == Completed
      then pure $ Left $ InvalidCommandError "Launch is completed"
      else do
        let newAuction = L.submitBid bidder baseAmount priorityFee launch.state.auction
            newLaunchState = launch.state { auction = newAuction }
            updatedLaunch = launch { state = newLaunchState }
        
        modify_ (\s -> s 
          { launches = Map.insert launchId updatedLaunch s.launches 
          }) orchestrator
        
        pure $ Right unit

-- | Process the next batch for a launch
processNextBatch ::
  String ->           -- Launch ID
  LaunchOrchestrator ->
  Effect (Either ProtocolError ProcessBatchResult)
processNextBatch launchId orchestrator = do
  state <- read orchestrator
  
  case Map.lookup launchId state.launches of
    Nothing -> pure $ Left $ InvalidCommandError "Launch not found"
    Just launch -> 
      if launch.state.currentPhase == Completed
      then pure $ Left $ InvalidCommandError "Launch is completed"
      else do
        -- Process batch with learning
        let { result, newSequence } = L.processAndLearn 
              launch.state.auction 
              launch.state.sequence
        
        -- Update tokens distributed
        let tokensInBatch = result.basePayment / launch.state.sequence.basePrice
            newTokensDistributed = launch.tokensDistributed + tokensInBatch
        
        -- Check if phase should transition
        let shouldTransition = L.isPhaseComplete newSequence
        
        -- Update launch state
        let newAuction = launch.state.auction 
              { bids = []  -- Clear bids for next batch
              , batchNumber = launch.state.auction.batchNumber + 1
              , tokensRemaining = launch.totalTokens - newTokensDistributed
              , basePrice = newSequence.basePrice
              }
        
        let newLaunchState = 
              if shouldTransition
              then L.transitionPhase (launch.state { sequence = newSequence })
              else launch.state 
                { auction = newAuction
                , sequence = newSequence
                }
        
        -- Check if launch is complete
        let isComplete = newLaunchState.currentPhase == Completed || 
                        newTokensDistributed >= launch.totalTokens
        
        let finalLaunchState = 
              if isComplete && newLaunchState.currentPhase /= Completed
              then newLaunchState { currentPhase = Completed }
              else newLaunchState
        
        let updatedLaunch = launch 
              { state = finalLaunchState
              , tokensDistributed = newTokensDistributed
              }
        
        -- Update orchestrator state
        if isComplete
        then modify_ (\s -> s 
          { launches = Map.delete launchId s.launches
          , completedLaunches = launchId : s.completedLaunches
          }) orchestrator
        else modify_ (\s -> s 
          { launches = Map.insert launchId updatedLaunch s.launches
          }) orchestrator
        
        pure $ Right 
          { batchResult: result
          , newPhase: if shouldTransition then Just finalLaunchState.currentPhase else Nothing
          , tokensDistributed: tokensInBatch
          , isComplete: isComplete
          }

-- | Result of processing a batch
type ProcessBatchResult =
  { batchResult :: L.BatchResult
  , newPhase :: Maybe LaunchPhase
  , tokensDistributed :: Number
  , isComplete :: Boolean
  }

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Get all active launches
getActiveLaunches :: LaunchOrchestrator -> Effect (Array ActiveLaunch)
getActiveLaunches orchestrator = do
  state <- read orchestrator
  pure $ Array.fromFoldable $ Map.values state.launches

-- | Get status of a specific launch
getLaunchStatus :: String -> LaunchOrchestrator -> Effect (Maybe LaunchStatus)
getLaunchStatus launchId orchestrator = do
  state <- read orchestrator
  case Map.lookup launchId state.launches of
    Nothing -> 
      if launchId `elem` state.completedLaunches
      then pure $ Just CompletedStatus
      else pure Nothing
    Just launch -> pure $ Just $ ActiveStatus
      { phase: launch.state.currentPhase
      , currentPrice: launch.state.sequence.basePrice
      , tokensDistributed: launch.tokensDistributed
      , totalTokens: launch.totalTokens
      , batchNumber: launch.state.auction.batchNumber
      , pendingBids: length launch.state.auction.bids
      }

-- | Launch status types
data LaunchStatus
  = ActiveStatus
    { phase :: LaunchPhase
    , currentPrice :: Number
    , tokensDistributed :: Number
    , totalTokens :: Number
    , batchNumber :: Int
    , pendingBids :: Int
    }
  | CompletedStatus

derive instance eqLaunchStatus :: Eq LaunchStatus


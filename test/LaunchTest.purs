-- | Test suite for the Cascading Term-Based Batch Auction Launch System
module Test.LaunchTest where

import Prelude
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Data.Array (length, head, last)
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (Result(..), quickCheck, (===))

-- Import launch modules (commented out - modules missing)
-- import Protocol.Launch.Launch
-- import Protocol.Launch.Orchestrator as LO
import Protocol.Common (PoolId)

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- Create a test launch configuration
createTestLaunchConfig :: LaunchConfig
createTestLaunchConfig =
  { tokenTicker: "TEST"
  , poolId: "TEST/FeelsSOL" 
  , batchSize: 10
  , initialPrice: 1.0
  , phasePremiums:
    { daily: 1.2
    , hourly: 1.1
    , spot: 1.05
    }
  }

-- Create a test bid
createTestBid :: String -> Number -> Number -> Bid
createTestBid bidder baseAmount priorityFee =
  { bidder: bidder
  , baseAmount: baseAmount
  , priorityFee: priorityFee
  , totalPayment: baseAmount + priorityFee
  }

--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

-- Test batch auction winner selection
testBatchAuctionWinnerSelection :: Effect Unit
testBatchAuctionWinnerSelection = do
  log "\n=== Testing Batch Auction Winner Selection ==="
  
  let config = createTestLaunchConfig
      launchState = initializeLaunch config
      auction = launchState.auction
      
  -- Submit bids with varying priority fees
  let auction1 = submitBid "user1" 100.0 10.0 auction    -- Total: 110
      auction2 = submitBid "user2" 100.0 5.0 auction1    -- Total: 105
      auction3 = submitBid "user3" 100.0 15.0 auction2   -- Total: 115
      auction4 = submitBid "user4" 100.0 0.0 auction3    -- Total: 100
      
  -- Process batch
  let result = processBatch auction4
  
  -- Verify winners are sorted by total payment
  quickCheck $ length result.winners === 4
  
  case head result.winners of
    Just winner -> quickCheck $ winner.bidder === "user3"  -- Highest priority fee
    Nothing -> log "ERROR: No winners found"
    
  case last result.winners of
    Just winner -> quickCheck $ winner.bidder === "user4"  -- No priority fee
    Nothing -> log "ERROR: No last winner"
    
  log $ "Total protocol revenue: " <> show result.protocolRevenue
  quickCheck $ result.protocolRevenue === 30.0  -- Sum of priority fees

-- Test continuous learning price updates
testContinuousLearning :: Effect Unit
testContinuousLearning = do
  log "\n=== Testing Continuous Learning ==="
  
  let config = createTestLaunchConfig
      launchState = initializeLaunch config
      sequence = launchState.sequence
      auction = launchState.auction
      
  -- Submit bids with 10% average priority fee
  let auctionWithBids = 
        submitBid "user1" 100.0 10.0 $
        submitBid "user2" 100.0 10.0 $
        submitBid "user3" 100.0 10.0 auction
        
  -- Process and learn
  let { result, newSequence } = processAndLearn auctionWithBids sequence
  
  -- Verify price increased based on fee signal
  log $ "Old base price: " <> show sequence.basePrice
  log $ "New base price: " <> show newSequence.basePrice
  log $ "Learning rate: " <> show newSequence.learningRate
  
  quickCheck $ newSequence.basePrice > sequence.basePrice
  quickCheck $ result.avgPriorityFeeRatio === 0.1  -- 10% average

-- Test phase transitions
testPhaseTransitions :: Effect Unit
testPhaseTransitions = do
  log "\n=== Testing Phase Transitions ==="
  
  let config = createTestLaunchConfig
      launchState = initializeLaunch config
      
  -- Verify initial phase
  quickCheck $ launchState.currentPhase === WeeklyPhase
  
  -- Simulate convergence by adding low-fee history
  let convergedSequence = launchState.sequence 
        { feeHistory = [0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05] }
      
  -- Check convergence detection
  quickCheck $ isPhaseComplete convergedSequence === true
  
  -- Transition to next phase
  let stateWithConvergedSequence = launchState { sequence = convergedSequence }
      nextState = transitionPhase stateWithConvergedSequence
      
  quickCheck $ nextState.currentPhase === DailyPhase
  
  -- Verify price premium applied
  let expectedPrice = launchState.sequence.basePrice * config.phasePremiums.daily
  quickCheck $ nextState.sequence.basePrice === expectedPrice

-- Test edge cases
testEdgeCases :: Effect Unit
testEdgeCases = do
  log "\n=== Testing Edge Cases ==="
  
  -- Test empty auction
  let config = createTestLaunchConfig
      launchState = initializeLaunch config
      emptyResult = processBatch launchState.auction
      
  quickCheck $ length emptyResult.winners === 0
  quickCheck $ emptyResult.protocolRevenue === 0.0
  
  -- Test identical bids
  let auction = launchState.auction
      auctionIdentical = 
        submitBid "user1" 100.0 5.0 $
        submitBid "user2" 100.0 5.0 $
        submitBid "user3" 100.0 5.0 auction
        
  let identicalResult = processBatch auctionIdentical
  quickCheck $ length identicalResult.winners === 3
  quickCheck $ identicalResult.avgPriorityFeeRatio === 0.05

-- Test launch orchestrator
testLaunchOrchestrator :: Effect Unit
testLaunchOrchestrator = do
  log "\n=== Testing Launch Orchestrator ==="
  
  -- Create orchestrator
  orchestrator <- LO.initOrchestrator
  
  -- Start a launch
  launchResult <- LO.createLaunch "TEST" "TEST/FeelsSOL" 1.0 10000.0 0.0 orchestrator
  
  case launchResult of
    Right launchId -> do
      log $ "Created launch: " <> launchId
      
      -- Submit some bids
      bidResult1 <- LO.submitBidToLaunch launchId "user1" 100.0 10.0 orchestrator
      bidResult2 <- LO.submitBidToLaunch launchId "user2" 100.0 5.0 orchestrator
      
      quickCheck $ isRight bidResult1
      quickCheck $ isRight bidResult2
      
      -- Process batch
      processResult <- LO.processNextBatch launchId orchestrator
      
      case processResult of
        Right result -> do
          log $ "Batch processed, tokens distributed: " <> show result.tokensDistributed
          quickCheck $ result.tokensDistributed > 0.0
        Left err -> log $ "ERROR: Failed to process batch: " <> show err
        
    Left err -> log $ "ERROR: Failed to create launch: " <> show err

--------------------------------------------------------------------------------
-- Run All Tests
--------------------------------------------------------------------------------

runLaunchTests :: Effect Unit
runLaunchTests = do
  log "Running Launch System Tests..."
  
  testBatchAuctionWinnerSelection
  testContinuousLearning
  testPhaseTransitions
  testEdgeCases
  testLaunchOrchestrator
  
  log "\nAll launch tests completed!"
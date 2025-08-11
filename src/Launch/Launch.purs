-- | Launch Module - Cascading Term-Based Batch Auction with Continuous Learning
-- |
-- | This module implements the token launch mechanism that discovers price through
-- | sequential batch auctions with continuous learning from priority fees.
module Launch.Launch
  ( -- Types
    BatchAuction
  , BatchSequence
  , LaunchPhase(..)
  , Bid
  , BatchResult
  , LaunchConfig
  , LaunchState
  -- Functions
  , createLaunchConfig
  , initializeLaunch
  , submitBid
  , processBatch
  , processAndLearn
  , isPhaseComplete
  , transitionPhase
  ) where

import Prelude
import Data.Array ((:), take, drop, sortBy, filter, length, uncons)
import Data.Maybe (Maybe(..))
import Data.Foldable (sum, foldl)
import Data.Int (toNumber)
import Data.Number (abs)
import Data.Ord (min)
import Common (PoolId)

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Launch phases in order of execution
data LaunchPhase
  = WeeklyPhase
  | DailyPhase  
  | HourlyPhase
  | SpotPhase
  | Completed

derive instance eqLaunchPhase :: Eq LaunchPhase
derive instance ordLaunchPhase :: Ord LaunchPhase

instance showLaunchPhase :: Show LaunchPhase where
  show WeeklyPhase = "Weekly"
  show DailyPhase = "Daily"
  show HourlyPhase = "Hourly"
  show SpotPhase = "Spot"
  show Completed = "Completed"

-- | A bid in the batch auction
type Bid =
  { bidder :: String
  , baseAmount :: Number      -- Amount at base price
  , priorityFee :: Number     -- Additional fee for priority
  , totalPayment :: Number    -- baseAmount + priorityFee
  }

-- | Result of processing a batch
type BatchResult =
  { winners :: Array Bid
  , basePayment :: Number
  , protocolRevenue :: Number  -- Sum of priority fees
  , avgPriorityFeeRatio :: Number
  , batchNumber :: Int
  }

-- | Configuration for a launch
type LaunchConfig =
  { tokenTicker :: String
  , poolId :: PoolId
  , batchSize :: Int           -- Tokens per batch
  , initialPrice :: Number     -- Starting base price
  , phasePremiums ::           -- Premium for each phase transition
    { daily :: Number
    , hourly :: Number
    , spot :: Number
    }
  }

-- | Batch auction state
type BatchAuction =
  { phase :: LaunchPhase
  , bids :: Array Bid
  , batchNumber :: Int
  , tokensRemaining :: Number
  , basePrice :: Number
  }

-- | Learning state for continuous price discovery
type BatchSequence =
  { currentBatch :: Int
  , basePrice :: Number         -- Updates after each batch
  , learningRate :: Number      -- How aggressively to adjust
  , feeHistory :: Array Number  -- Recent fee ratios for stability
  }

-- | Complete launch state
type LaunchState =
  { config :: LaunchConfig
  , currentPhase :: LaunchPhase
  , auction :: BatchAuction
  , sequence :: BatchSequence
  , phaseResults :: Array        -- Results from completed phases
    { phase :: LaunchPhase
    , finalPrice :: Number
    , totalRevenue :: Number
    , batchCount :: Int
    }
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Create a launch configuration
createLaunchConfig :: String -> PoolId -> Number -> LaunchConfig
createLaunchConfig ticker poolId initialPrice =
  { tokenTicker: ticker
  , poolId: poolId
  , batchSize: 100             -- 100 tokens per batch
  , initialPrice: initialPrice
  , phasePremiums:
    { daily: 1.2               -- 20% premium
    , hourly: 1.1              -- 10% premium  
    , spot: 1.05               -- 5% premium
    }
  }

-- | Initialize a new launch
initializeLaunch :: LaunchConfig -> LaunchState
initializeLaunch config =
  { config: config
  , currentPhase: WeeklyPhase
  , auction:
    { phase: WeeklyPhase
    , bids: []
    , batchNumber: 0
    , tokensRemaining: 10000.0  -- Total supply to distribute
    , basePrice: config.initialPrice
    }
  , sequence:
    { currentBatch: 0
    , basePrice: config.initialPrice
    , learningRate: 0.5         -- Start with moderate learning
    , feeHistory: []
    }
  , phaseResults: []
  }

--------------------------------------------------------------------------------
-- Bid Management
--------------------------------------------------------------------------------

-- | Submit a bid to the current auction
submitBid :: String -> Number -> Number -> BatchAuction -> BatchAuction
submitBid bidder baseAmount priorityFee auction =
  let bid = 
        { bidder: bidder
        , baseAmount: baseAmount
        , priorityFee: priorityFee
        , totalPayment: baseAmount + priorityFee
        }
  in auction { bids = bid : auction.bids }

--------------------------------------------------------------------------------
-- Batch Processing
--------------------------------------------------------------------------------

-- | Process a batch auction and select winners
processBatch :: BatchAuction -> BatchResult
processBatch auction =
  let
    -- Sort bids by total payment (highest first)
    sortedBids = sortBy (\a b -> compare b.totalPayment a.totalPayment) auction.bids
    
    -- Calculate how many tokens this batch offers
    batchTokens = min (toNumber auction.batchNumber * 100.0) auction.tokensRemaining
    
    -- Select winners until batch is filled
    selectWinners :: Array Bid -> Number -> Array Bid -> Array Bid
    selectWinners bids remaining acc = case bids of
      [] -> acc
      _ -> case uncons bids of
        Nothing -> acc
        Just { head: bid, tail: rest } ->
          if remaining <= 0.0 then acc
          else 
            let allocated = min bid.baseAmount remaining
            in selectWinners rest (remaining - allocated) (acc <> [bid])
    
    winners = selectWinners sortedBids batchTokens []
    
    -- Calculate results
    basePayment = sum (map _.baseAmount winners)
    protocolRevenue = sum (map _.priorityFee winners)
    avgFeeRatio = if basePayment > 0.0 
                  then protocolRevenue / basePayment
                  else 0.0
  in
    { winners: winners
    , basePayment: basePayment
    , protocolRevenue: protocolRevenue
    , avgPriorityFeeRatio: avgFeeRatio
    , batchNumber: auction.batchNumber
    }

--------------------------------------------------------------------------------
-- Continuous Learning
--------------------------------------------------------------------------------

-- | Process batch and update learning state
processAndLearn :: BatchAuction -> BatchSequence -> { result :: BatchResult, newSequence :: BatchSequence }
processAndLearn auction sequence =
  let 
    result = processBatch auction
    feeRatio = result.avgPriorityFeeRatio
    
    -- Update base price based on fee signal
    adjustment = 1.0 + (feeRatio * sequence.learningRate)
    newBasePrice = sequence.basePrice * adjustment
    
    -- Update fee history
    newHistory = take 10 (feeRatio : sequence.feeHistory)
    
    -- Adapt learning rate based on fee stability
    feeVariance = if length newHistory > 2
                  then calculateVariance newHistory
                  else 1.0
    
    newLearningRate = 
      if feeVariance > 0.5 
      then sequence.learningRate * 0.9      -- High variance: learn slower
      else min 0.8 (sequence.learningRate * 1.1)  -- Low variance: learn faster
    
    newSequence = sequence 
      { currentBatch = sequence.currentBatch + 1
      , basePrice = newBasePrice
      , learningRate = newLearningRate
      , feeHistory = newHistory
      }
  in
    { result: result, newSequence: newSequence }

-- | Calculate variance of fee ratios
calculateVariance :: Array Number -> Number
calculateVariance fees =
  let mean = sum fees / toNumber (length fees)
      squaredDiffs = map (\x -> (x - mean) * (x - mean)) fees
  in sum squaredDiffs / toNumber (length fees)

--------------------------------------------------------------------------------
-- Phase Management
--------------------------------------------------------------------------------

-- | Check if current phase is complete
isPhaseComplete :: BatchSequence -> Boolean
isPhaseComplete sequence =
  let recentFees = take 10 sequence.feeHistory
      allLowFees = length (filter (_ < 0.1) recentFees) == length recentFees
  in length recentFees >= 10 && allLowFees

-- | Transition to next phase
transitionPhase :: LaunchState -> LaunchState
transitionPhase state =
  let
    nextPhase = case state.currentPhase of
      WeeklyPhase -> DailyPhase
      DailyPhase -> HourlyPhase
      HourlyPhase -> SpotPhase
      SpotPhase -> Completed
      Completed -> Completed
    
    -- Calculate starting price for next phase
    phaseMultiplier = case nextPhase of
      DailyPhase -> state.config.phasePremiums.daily
      HourlyPhase -> state.config.phasePremiums.hourly
      SpotPhase -> state.config.phasePremiums.spot
      _ -> 1.0
    
    newBasePrice = state.sequence.basePrice * phaseMultiplier
    
    -- Store results from completed phase
    phaseResult = 
      { phase: state.currentPhase
      , finalPrice: state.sequence.basePrice
      , totalRevenue: 0.0  -- TODO: Track cumulative revenue
      , batchCount: state.sequence.currentBatch
      }
    
    -- Reset for new phase
    newAuction = state.auction
      { phase = nextPhase
      , bids = []
      , batchNumber = 0
      , basePrice = newBasePrice
      }
    
    newSequence = 
      { currentBatch: 0
      , basePrice: newBasePrice
      , learningRate: 0.6    -- Can be more aggressive in later phases
      , feeHistory: []
      }
  in
    state 
      { currentPhase = nextPhase
      , auction = newAuction
      , sequence = newSequence
      , phaseResults = state.phaseResults <> [phaseResult]
      }
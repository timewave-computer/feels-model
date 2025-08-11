-- | Simulation tests for the Launch System under various market conditions
module Test.LaunchSimulation where

import Prelude (class Eq, class Show, Unit, bind, discard, mod, pure, show, ($), (*), (+), (-), (/), (<>), (==), (>=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (range, length, foldl)
import Data.Traversable (traverse)
import Data.Int as Int
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (random)
import Effect.Ref (new, read, write)

-- Import launch and state modules
import Launch.Launch (BatchResult)
import Launch.Orchestrator as LO
import Launch.Orchestrator (LaunchStatus(..))
import Launch.Integration as LI
import State.Types (AppState)
import State.State (initState)
import POL (getTotalPOL)

--------------------------------------------------------------------------------
-- Market Simulation Types
--------------------------------------------------------------------------------

data MarketCondition
  = HighDemand    -- Many bidders, high priority fees
  | LowDemand     -- Few bidders, low priority fees  
  | Volatile      -- Alternating high/low demand
  | Convergent    -- Gradually decreasing fees

derive instance eqMarketCondition :: Eq MarketCondition

instance showMarketCondition :: Show MarketCondition where
  show HighDemand = "High Demand"
  show LowDemand = "Low Demand"
  show Volatile = "Volatile"
  show Convergent = "Convergent"

type SimulationParams =
  { condition :: MarketCondition
  , numBatches :: Int
  , numBiddersPerBatch :: Int
  , tokenTicker :: String
  , initialPrice :: Number
  }

--------------------------------------------------------------------------------
-- Simulation Helpers
--------------------------------------------------------------------------------

-- Generate bids based on market condition
generateBids :: MarketCondition -> Int -> Int -> Effect (Array { bidder :: String, baseAmount :: Number, priorityFee :: Number })
generateBids condition batchNum numBidders = do
  traverse (\i -> generateBid condition batchNum i) (range 1 numBidders)
  where
    generateBid :: MarketCondition -> Int -> Int -> Effect { bidder :: String, baseAmount :: Number, priorityFee :: Number }
    generateBid cond batch bidderNum = do
      rand <- random
      let bidder = "sim-user-" <> show bidderNum
          baseAmount = 100.0 + rand * 50.0  -- 100-150 base
          
      priorityFee <- case cond of
        HighDemand -> do
          r <- random
          pure $ baseAmount * (0.15 + r * 0.15)  -- 15-30% priority fee
          
        LowDemand -> do
          r <- random  
          pure $ baseAmount * (0.0 + r * 0.05)   -- 0-5% priority fee
          
        Volatile -> do
          r <- random
          if batch `mod` 2 == 0
            then pure $ baseAmount * (0.15 + r * 0.15)  -- High on even batches
            else pure $ baseAmount * (0.0 + r * 0.05)   -- Low on odd batches
            
        Convergent -> do
          r <- random
          let decayFactor = 1.0 - (Int.toNumber batch / 20.0)  -- Decay over 20 batches
          pure $ baseAmount * (0.2 * decayFactor + r * 0.05)
          
      pure { bidder, baseAmount, priorityFee }

-- Run a complete launch simulation
runLaunchSimulation :: SimulationParams -> Effect (Either String SimulationResults)
runLaunchSimulation params = do
  log $ "\n=== Running Launch Simulation: " <> show params.condition <> " ==="
  
  -- Initialize system
  runtime <- initState
  state <- read runtime.state
  
  -- Create launch
  let poolId = params.tokenTicker <> "/FeelsSOL"
  poolResult <- LI.createLaunchPool params.tokenTicker poolId params.initialPrice state.poolRegistry
  
  case poolResult of
    Left err -> pure $ Left $ "Failed to create pool: " <> show err
    Right _ -> do
      launchResult <- LO.createLaunch params.tokenTicker poolId params.initialPrice 10000.0 0.0 state.launchOrchestrator
      
      case launchResult of
        Left err -> pure $ Left $ "Failed to create launch: " <> show err
        Right launchId -> do
          -- Run batches
          batchResults <- runBatches launchId params state
          
          -- Get final metrics
          finalPOL <- getTotalPOL state.polState
          statusResult <- LO.getLaunchStatus launchId state.launchOrchestrator
          
          case statusResult of
            Just status -> 
              case status of
                ActiveStatus s -> pure $ Right
                  { launchId: launchId
                  , totalBatches: length batchResults
                  , finalPrice: s.currentPrice
                  , totalRevenue: foldl (\acc r -> acc + r.protocolRevenue) 0.0 batchResults
                  , totalPOL: finalPOL
                  , convergencePhase: show s.phase
                  , priceHistory: []  -- TODO: Track prices during batch processing
                  }
                CompletedStatus -> pure $ Right
                  { launchId: launchId
                  , totalBatches: length batchResults
                  , finalPrice: 0.0  -- Should get from final batch
                  , totalRevenue: foldl (\acc r -> acc + r.protocolRevenue) 0.0 batchResults
                  , totalPOL: finalPOL
                  , convergencePhase: "Completed"
                  , priceHistory: []  -- TODO: Track prices during batch processing
                  }
            Nothing -> pure $ Left $ "Failed to get launch status"

type SimulationResults =
  { launchId :: String
  , totalBatches :: Int
  , finalPrice :: Number
  , totalRevenue :: Number
  , totalPOL :: Number
  , convergencePhase :: String
  , priceHistory :: Array Number
  }

-- Run multiple batches for a launch
runBatches :: String -> SimulationParams -> AppState -> Effect (Array BatchResult)
runBatches launchId params state = do
  resultsRef <- new []
  
  let processBatch batchNum = do
        if batchNum >= params.numBatches
          then read resultsRef
          else do
            -- Generate and submit bids
            bids <- generateBids params.condition batchNum params.numBiddersPerBatch
            
            _ <- traverse (\bid -> 
              LO.submitBidToLaunch launchId bid.bidder bid.baseAmount bid.priorityFee state.launchOrchestrator
            ) bids
            
            -- Process batch
            processResult <- LO.processNextBatch launchId state.launchOrchestrator
            
            case processResult of
              Right result -> do
                -- Extract batch result from process result
                let batchResult = result.batchResult
                
                -- Store result
                results <- read resultsRef
                write (results <> [batchResult]) resultsRef
                
                -- Route fees to POL
                LI.routeFeesToPOL batchResult state.polState
                
                -- Continue if not complete
                if result.isComplete
                  then read resultsRef
                  else processBatch (batchNum + 1)
                  
              Left err -> do
                log $ "ERROR processing batch " <> show batchNum <> ": " <> show err
                read resultsRef
                
  processBatch 0

--------------------------------------------------------------------------------
-- Simulation Tests
--------------------------------------------------------------------------------

-- Test high demand scenario
testHighDemandScenario :: Effect Unit
testHighDemandScenario = do
  let params = 
        { condition: HighDemand
        , numBatches: 30
        , numBiddersPerBatch: 20
        , tokenTicker: "HIGHD"
        , initialPrice: 1.0
        }
        
  result <- runLaunchSimulation params
  
  case result of
    Right res -> do
      log $ "High Demand Results:"
      log $ "  Final price: " <> show res.finalPrice
      log $ "  Total revenue: " <> show res.totalRevenue
      log $ "  Convergence phase: " <> res.convergencePhase
      log $ "  Price multiplier: " <> show (res.finalPrice / params.initialPrice)
      
    Left err -> log $ "ERROR: " <> err

-- Test low demand scenario
testLowDemandScenario :: Effect Unit
testLowDemandScenario = do
  let params = 
        { condition: LowDemand
        , numBatches: 30
        , numBiddersPerBatch: 5
        , tokenTicker: "LOWD"
        , initialPrice: 1.0
        }
        
  result <- runLaunchSimulation params
  
  case result of
    Right res -> do
      log $ "Low Demand Results:"
      log $ "  Final price: " <> show res.finalPrice
      log $ "  Total revenue: " <> show res.totalRevenue
      log $ "  Convergence phase: " <> res.convergencePhase
      log $ "  Price multiplier: " <> show (res.finalPrice / params.initialPrice)
      
    Left err -> log $ "ERROR: " <> err

-- Test volatile market
testVolatileScenario :: Effect Unit
testVolatileScenario = do
  let params = 
        { condition: Volatile
        , numBatches: 40
        , numBiddersPerBatch: 15
        , tokenTicker: "VOLT"
        , initialPrice: 1.0
        }
        
  result <- runLaunchSimulation params
  
  case result of
    Right res -> do
      log $ "Volatile Market Results:"
      log $ "  Final price: " <> show res.finalPrice
      log $ "  Total revenue: " <> show res.totalRevenue
      log $ "  Convergence phase: " <> res.convergencePhase
      log $ "  Total batches: " <> show res.totalBatches
      
    Left err -> log $ "ERROR: " <> err

-- Test convergent market (ideal scenario)
testConvergentScenario :: Effect Unit  
testConvergentScenario = do
  let params = 
        { condition: Convergent
        , numBatches: 25
        , numBiddersPerBatch: 10
        , tokenTicker: "CONV"
        , initialPrice: 1.0
        }
        
  result <- runLaunchSimulation params
  
  case result of
    Right res -> do
      log $ "Convergent Market Results:"
      log $ "  Final price: " <> show res.finalPrice
      log $ "  Total revenue: " <> show res.totalRevenue
      log $ "  Convergence phase: " <> res.convergencePhase
      log $ "  POL accumulated: " <> show res.totalPOL
      
    Left err -> log $ "ERROR: " <> err

--------------------------------------------------------------------------------
-- Run All Simulations
--------------------------------------------------------------------------------

runLaunchSimulations :: Effect Unit
runLaunchSimulations = do
  log "Running Launch System Simulations..."
  
  testHighDemandScenario
  testLowDemandScenario
  testVolatileScenario
  testConvergentScenario
  
  log "\nAll launch simulations completed!"
module POL
  ( POLState
  , POLBandAllocation
  , POLCounterbalancing
  , POLMetrics
  , initPOL
  , allocatePOLToBands
  , updateCounterbalancing
  , contributeToPOL
  , getPOLAllocation
  , getPOLMetrics
  , rebalancePOL
  , getPOLBalance
  , getTokenPOLBalance
  , getAllTokenPOLBalances
  , contributeToTokenPOL
  , captureStakingRewards
  , createPOLTicks
  , isPOLPosition
  , takeSnapshot
  , getUtilizationRate
  , getPOLValueForChart
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (Map)
import Data.Map as Map
import Data.Array ((:), filter, foldr, sortBy, find, uncons, partition) as Array
import Data.Tuple (Tuple(..))
import Data.Foldable (sum, maximum)
import Data.Number (abs)
import Data.Ord (max)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Token (TokenType(..))
import Position (BandTier(..), TokenPair)
import RiskManagement (StressLevel(..))
import FFI (currentTime)
import LendingRecord (LendingRecord, LendingTerms(..))
import LendingBook (LendingBook)
import Control.Monad (when)
import Data.Either (Either(..))
import Data.Int as Int

--------------------------------------------------------------------------------
-- POL Types
--------------------------------------------------------------------------------

-- POL allocation to a specific band
type POLBandAllocation =
  { bandTier :: BandTier
  , tokenPair :: TokenPair
  , allocatedAmount :: Number      -- Amount in base token
  , targetPercentage :: Number     -- Target % of total POL
  , actualPercentage :: Number     -- Actual % allocated
  , lastRebalance :: Number        -- Last rebalance timestamp
  }

-- Counterbalancing configuration
type POLCounterbalancing =
  { userBias :: Map BandTier Number    -- User concentration by band
  , polOffset :: Map BandTier Number   -- POL offset to counterbalance
  , aggressiveness :: Number           -- How strongly to counterbalance (0-1)
  , updateFrequency :: Number          -- Min ms between updates
  }

-- POL metrics
type POLMetrics =
  { totalValue :: Number               -- Total POL value
  , allocations :: Array POLBandAllocation
  , utilizationByBand :: Map BandTier Number
  , rebalanceCount :: Int
  , lastRebalance :: Number
  , growthRate24h :: Number
  }

-- POL state
type POLState =
  { totalBalance :: Ref Number        -- Total POL balance
  , allocations :: Ref (Array POLBandAllocation)
  , counterbalancing :: Ref POLCounterbalancing
  , metrics :: Ref POLMetrics
  , lastUpdate :: Ref Number
  , rebalanceThreshold :: Number      -- Min % deviation to trigger rebalance
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize POL V2
initPOL :: Effect POLState
initPOL = do
  now <- currentTime
  
  -- Initial allocations (default strategy)
  let initialAllocations = []
      
      initialCounterbalancing =
        { userBias: Map.empty
        , polOffset: Map.empty
        , aggressiveness: 0.5     -- Medium counterbalancing
        , updateFrequency: 60000.0 -- 1 minute minimum
        }
      
      initialMetrics =
        { totalValue: 0.0
        , allocations: []
        , utilizationByBand: Map.empty
        , rebalanceCount: 0
        , lastRebalance: now
        , growthRate24h: 0.0
        }
  
  totalBalance <- new 0.0
  allocations <- new initialAllocations
  counterbalancing <- new initialCounterbalancing
  metrics <- new initialMetrics
  lastUpdate <- new now
  
  pure { totalBalance
       , allocations
       , counterbalancing
       , metrics
       , lastUpdate
       , rebalanceThreshold: 0.05  -- 5% deviation triggers rebalance
       }

--------------------------------------------------------------------------------
-- POL Allocation
--------------------------------------------------------------------------------

-- Allocate POL to bands based on strategy
allocatePOLToBands :: 
  { tokenPair :: TokenPair
  , stressLevel :: StressLevel
  , userActivity :: Map BandTier Number  -- User volume by band
  } -> POLState -> Effect (Array POLBandAllocation)
allocatePOLToBands params polState = do
  totalBal <- read polState.totalBalance
  counter <- read polState.counterbalancing
  now <- currentTime
  
  -- Calculate base allocation strategy
  let baseAllocation = calculateBaseAllocation params.stressLevel
      
      -- Apply counterbalancing
      counterbalanced = applyCounterbalancing 
        { baseAllocation
        , userActivity: params.userActivity
        , counterbalancing: counter
        }
      
      -- Create allocations
      allocations = createAllocations
        { tokenPair: params.tokenPair
        , totalAmount: totalBal
        , targetPercentages: counterbalanced
        , timestamp: now
        }
  
  -- Update state
  modify_ (\_ -> allocations) polState.allocations
  
  pure allocations

-- Calculate base allocation based on stress
calculateBaseAllocation :: StressLevel -> Map BandTier Number
calculateBaseAllocation stress = case stress of
  Normal ->
    -- In normal conditions, favor tight bands for efficiency
    Map.fromFoldable
      [ Tuple TightBand 0.50    -- 50% to tight
      , Tuple MediumBand 0.35   -- 35% to medium
      , Tuple WideBand 0.15     -- 15% to wide
      ]
  Mild ->
    -- Mild stress, slightly wider allocation
    Map.fromFoldable
      [ Tuple TightBand 0.35    -- 35% to tight
      , Tuple MediumBand 0.45   -- 45% to medium
      , Tuple WideBand 0.20     -- 20% to wide
      ]
  Moderate ->
    -- Balanced allocation for moderate stress
    Map.fromFoldable
      [ Tuple TightBand 0.25    -- 25% to tight
      , Tuple MediumBand 0.50   -- 50% to medium
      , Tuple WideBand 0.25     -- 25% to wide
      ]
  Severe ->
    -- In severe stress, favor wider bands
    Map.fromFoldable
      [ Tuple TightBand 0.15    -- 15% to tight
      , Tuple MediumBand 0.35   -- 35% to medium
      , Tuple WideBand 0.50     -- 50% to wide
      ]
  Critical ->
    -- Critical stress, maximize wide bands for stability
    Map.fromFoldable
      [ Tuple TightBand 0.10    -- 10% to tight
      , Tuple MediumBand 0.30   -- 30% to medium
      , Tuple WideBand 0.60     -- 60% to wide
      ]

-- Apply counterbalancing to offset user bias
applyCounterbalancing ::
  { baseAllocation :: Map BandTier Number
  , userActivity :: Map BandTier Number
  , counterbalancing :: POLCounterbalancing
  } -> Map BandTier Number
applyCounterbalancing params =
  let -- Calculate user bias (normalized)
      totalUserActivity = sum (Map.values params.userActivity)
      userBias = if totalUserActivity > 0.0
        then map (\(Tuple k v) -> Tuple k (v / totalUserActivity)) (Map.toUnfoldable params.userActivity :: Array (Tuple BandTier Number))
               # Map.fromFoldable
        else params.baseAllocation
      
      -- Calculate offset needed
      offsets = Map.mapMaybeWithKey (\band baseTarget ->
        let userConcentration = fromMaybe 0.0 (Map.lookup band userBias)
            deviation = userConcentration - baseTarget
            -- Offset in opposite direction
            offset = -deviation * params.counterbalancing.aggressiveness
        in Just offset
      ) params.baseAllocation
      
      -- Apply offsets and normalize
      adjusted = Map.mapMaybeWithKey (\band baseTarget ->
        let offset = fromMaybe 0.0 (Map.lookup band offsets)
        in Just (max 0.1 (baseTarget + offset))  -- Min 10% to any band
      ) params.baseAllocation
      
      -- Normalize to sum to 1.0
      total = sum (Map.values adjusted)
      normalized = map (\(Tuple k v) -> Tuple k (v / total)) (Map.toUnfoldable adjusted :: Array (Tuple BandTier Number))
                     # Map.fromFoldable
      
  in normalized

-- Create specific allocations
createAllocations ::
  { tokenPair :: TokenPair
  , totalAmount :: Number
  , targetPercentages :: Map BandTier Number
  , timestamp :: Number
  } -> Array POLBandAllocation
createAllocations params =
  Map.toUnfoldable params.targetPercentages # map (\(Tuple band pct) ->
    { bandTier: band
    , tokenPair: params.tokenPair
    , allocatedAmount: params.totalAmount * pct
    , targetPercentage: pct
    , actualPercentage: pct  -- Initially matches target
    , lastRebalance: params.timestamp
    }
  )

--------------------------------------------------------------------------------
-- Counterbalancing Updates
--------------------------------------------------------------------------------

-- Update counterbalancing based on user activity
updateCounterbalancing ::
  { userActivity :: Map BandTier Number
  , currentTime :: Number
  } -> POLState -> Effect Unit
updateCounterbalancing params polState = do
  counter <- read polState.counterbalancing
  lastUpdate <- read polState.lastUpdate
  
  -- Check update frequency
  if params.currentTime - lastUpdate < counter.updateFrequency
    then pure unit
    else do
      -- Calculate new user bias
      let totalActivity = sum (Map.values params.userActivity)
          newBias = if totalActivity > 0.0
            then map (\(Tuple k v) -> Tuple k (v / totalActivity)) (Map.toUnfoldable params.userActivity :: Array (Tuple BandTier Number))
                   # Map.fromFoldable
            else counter.userBias
          
          -- Calculate POL offsets
          newOffsets = Map.mapMaybeWithKey (\band bias ->
            -- Target even distribution
            let target = 1.0 / 3.0  -- Assuming 3 bands
                deviation = bias - target
            in Just (-deviation)  -- Opposite direction
          ) newBias
      
      -- Update state
      modify_ (\c -> c 
        { userBias = newBias
        , polOffset = newOffsets
        }) polState.counterbalancing
      
      modify_ (\_ -> params.currentTime) polState.lastUpdate

--------------------------------------------------------------------------------
-- POL Management
--------------------------------------------------------------------------------

-- Contribute to POL
contributeToPOL :: POLState -> LendingTerms -> Number -> Maybe Int -> Effect Unit
contributeToPOL polState lendingTerms amount positionId = do
  modify_ (\bal -> bal + amount) polState.totalBalance
  
  -- Update metrics
  metrics <- read polState.metrics
  modify_ (\m -> m 
    { totalValue = m.totalValue + amount
    }) polState.metrics

-- Get current POL allocation for a band
getPOLAllocation :: BandTier -> TokenPair -> POLState -> Effect (Maybe Number)
getPOLAllocation band tokenPair polState = do
  allocations <- read polState.allocations
  let allocation = Array.find (\a -> a.bandTier == band && a.tokenPair == tokenPair) allocations
  pure $ map _.allocatedAmount allocation

-- Get POL metrics
getPOLMetrics :: POLState -> Effect POLMetrics
getPOLMetrics polState = read polState.metrics

--------------------------------------------------------------------------------
-- Rebalancing
--------------------------------------------------------------------------------

-- Rebalance POL across bands
rebalancePOL :: 
  { currentAllocations :: Map BandTier Number
  , targetAllocations :: Map BandTier Number
  } -> POLState -> Effect Boolean
rebalancePOL params polState = do
  -- Check if rebalance is needed
  let deviations = Map.mapMaybeWithKey (\band current ->
        let target = fromMaybe 0.0 (Map.lookup band params.targetAllocations)
            deviation = abs (current - target)
        in Just deviation
      ) params.currentAllocations
      
      maxDeviation = fromMaybe 0.0 (maximum (Map.values deviations))
      threshold = polState.rebalanceThreshold
  
  if maxDeviation > threshold
    then do
      -- Perform rebalance
      now <- currentTime
      allocations <- read polState.allocations
      totalBal <- read polState.totalBalance
      
      -- Update allocations
      let newAllocations = map (\alloc ->
            let target = fromMaybe alloc.targetPercentage 
                  (Map.lookup alloc.bandTier params.targetAllocations)
            in alloc 
              { allocatedAmount = totalBal * target
              , targetPercentage = target
              , actualPercentage = target
              , lastRebalance = now
              }
          ) allocations
      
      modify_ (\_ -> newAllocations) polState.allocations
      
      -- Update metrics
      modify_ (\m -> m 
        { rebalanceCount = m.rebalanceCount + 1
        , lastRebalance = now
        }) polState.metrics
      
      pure true
    else pure false

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Get total POL balance
getPOLBalance :: POLState -> Effect Number
getPOLBalance polState = read polState.totalBalance

-- Get POL balance for specific token pair
getTokenPOLBalance :: TokenPair -> POLState -> Effect Number
getTokenPOLBalance tokenPair polState = do
  allocations <- read polState.allocations
  let relevantAllocations = Array.filter (\a -> a.tokenPair == tokenPair) allocations
      totalForPair = sum (map _.allocatedAmount relevantAllocations)
  pure totalForPair

-- Get all token POL balances
getAllTokenPOLBalances :: POLState -> Effect (Map String Number)
getAllTokenPOLBalances polState = do
  allocations <- read polState.allocations
  -- Build a map by folding over allocations
  let balanceMap = Array.foldr (\alloc acc ->
        let key = showTokenPair alloc.tokenPair
            currentAmount = fromMaybe 0.0 (Map.lookup key acc)
        in Map.insert key (currentAmount + alloc.allocatedAmount) acc
      ) Map.empty allocations
  pure balanceMap
  where
    showTokenPair tp = showTokenType tp.base <> "/" <> showTokenType tp.quote
    
    showTokenType FeelsSOL = "FeelsSOL"
    showTokenType JitoSOL = "JitoSOL"
    showTokenType (Token s) = s


--------------------------------------------------------------------------------
-- Legacy Compatibility Functions
--------------------------------------------------------------------------------

-- Contribute to a specific token's POL
contributeToTokenPOL :: POLState -> TokenType -> Number -> Effect Unit
contributeToTokenPOL polState tokenType amount = do
  -- Just add to total balance for now
  modify_ (\bal -> bal + amount) polState.totalBalance

-- Capture staking rewards
captureStakingRewards :: POLState -> Number -> Number -> Effect Unit
captureStakingRewards polState totalFeelsSOLSupply jitoSOLPrice = do
  -- Simulate a constant staking yield of ~5% APY, accrued per block
  let dailyYield = 0.05 / 365.0
      blockYield = dailyYield / 43200.0 -- Assuming 2 blocks per second
      
      -- The differential value is the yield earned on the total supply
      differentialValue = totalFeelsSOLSupply * jitoSOLPrice * blockYield
  
  -- Only capture positive differentials
  when (differentialValue > 0.0) $ do
    modify_ (\bal -> bal + differentialValue) polState.totalBalance

-- Create POL ticks (legacy compatibility)
createPOLTicks :: POLState -> LendingBook -> Effect (Array LendingRecord)
createPOLTicks polState lendingBook = do
  -- For now, return empty array as POL now works with bands
  pure []

-- Check if position is POL-owned
isPOLPosition :: forall r. { owner :: String | r } -> Boolean
isPOLPosition position = position.owner == "POL"

-- Take snapshot
takeSnapshot :: POLState -> Effect String
takeSnapshot polState = do
  metrics <- getPOLMetrics polState
  now <- currentTime
  
  pure $ """POL Snapshot (""" <> show now <> """):
  Balance: """ <> show metrics.totalValue <> """
  24h Growth: """ <> show (metrics.growthRate24h * 100.0) <> """%"""

-- Get utilization rate
getUtilizationRate :: POLState -> Effect Number
getUtilizationRate polState = do
  allocations <- read polState.allocations
  totalBalance <- read polState.totalBalance
  
  if totalBalance <= 0.0
    then pure 0.0
    else do
      let totalAllocated = Array.foldr (\alloc acc -> acc + alloc.allocatedAmount) 0.0 allocations
      pure $ totalAllocated / totalBalance

-- Get POL value for charting
getPOLValueForChart :: POLState -> Effect { timestamp :: Number, value :: Number }
getPOLValueForChart polState = do
  balance <- getPOLBalance polState
  now <- currentTime
  pure { timestamp: now, value: balance }
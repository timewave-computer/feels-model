module POL
  ( POLState
  , POLMetrics
  , POLDistribution
  , POLContribution
  , ContributionBreakdown
  , Range
  , TokenPOLInfo
  , initPOL
  , contributeToPOL
  , contributeToTokenPOL
  , captureStakingRewards
  , getPOLBalance
  , getTokenPOLBalance
  , getAllTokenPOLBalances
  , getPOLMetrics
  , createPOLTicks
  , createPOLTicksWithDistribution
  , isPOLPosition
  , defaultPOLDistribution
  , takeSnapshot
  , getUtilizationRate
  , getPOLValueForChart
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), filter, catMaybes)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingTerms(..), LendingSide(..), UnbondingPeriod(..))
import LendingBook (LendingBook, createLendOffer)
import Data.Int as Int
import Data.Foldable (sum, foldr)
import Data.Array (uncons, range)
import Control.Monad (when)
import Data.Traversable (traverse)
import LendingRecord (LendingTerms(..))
import FFI (currentTime)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- POL Types
--------------------------------------------------------------------------------

-- Range for tick placement
type Range =
  { min :: Number  -- Minimum price/ratio multiplier
  , max :: Number  -- Maximum price/ratio multiplier
  }

-- POL distribution curve parameters
type POLDistribution =
  { floorWeight :: Number      -- Portion allocated below spot (e.g., 0.6)
  , spotWeight :: Number       -- Portion allocated near spot (e.g., 0.3)
  , discoveryWeight :: Number  -- Portion allocated above spot (e.g., 0.1)
  , floorRange :: Range        -- Price range for floor liquidity
  , spotRange :: Range         -- Price range for near-spot liquidity
  , discoveryRange :: Range    -- Price range for discovery liquidity
  , ticksPerRange :: Int       -- Number of ticks to create per range
  }

-- Per-token POL information
type TokenPOLInfo =
  { balance :: Number           -- POL balance for this token pair
  , lastContribution :: Number  -- Timestamp of last contribution
  , totalContributions :: Number -- Total contributions to this pair
  }

-- POL state tracking
type POLState =
  { balance :: Ref Number              -- Total POL balance in FeelsSOL
  , contributions :: Ref (Array POLContribution)  -- History of contributions
  , metricsCache :: Ref POLMetrics    -- Cached metrics
  , lastUpdate :: Ref Number           -- Last metrics update timestamp
  , distribution :: Ref POLDistribution  -- Current distribution parameters
  , tokenPOLBalances :: Ref (Map String TokenPOLInfo)  -- Per-token POL balances
  }

-- Individual contribution record
type POLContribution =
  { amount :: Number
  , source :: LendingTerms
  , timestamp :: Number
  , positionId :: Maybe Int  -- Associated position if applicable
  }

-- POL metrics for monitoring
type POLMetrics =
  { totalBalance :: Number
  , growthRate24h :: Number      -- 24-hour growth rate
  , growth7d :: Number       -- 7-day growth rate
  , contributionsByType :: ContributionBreakdown
  , utilizationRate :: Number    -- How much POL is actively lending
  , lastUpdated :: Number
  }

-- Contribution breakdown by operation type
type ContributionBreakdown =
  { swap :: Number
  , staking :: Number
  , leverage :: Number
  , gateway :: Number
  }

--------------------------------------------------------------------------------
-- Default Distribution
--------------------------------------------------------------------------------

-- Default POL distribution curve
defaultPOLDistribution :: POLDistribution
defaultPOLDistribution = 
  { floorWeight: 0.6        -- 60% below spot
  , spotWeight: 0.3         -- 30% near spot
  , discoveryWeight: 0.1    -- 10% above spot
  , floorRange: { min: 0.5, max: 0.9 }      -- 0.5x to 0.9x spot
  , spotRange: { min: 0.9, max: 1.1 }       -- 0.9x to 1.1x spot
  , discoveryRange: { min: 1.1, max: 2.0 }  -- 1.1x to 2.0x spot
  , ticksPerRange: 5        -- 5 ticks per range = 15 total
  }

--------------------------------------------------------------------------------
-- POL Initialization
--------------------------------------------------------------------------------

-- Initialize POL system
initPOL :: Effect POLState
initPOL = do
  balance <- new 0.0
  contributions <- new []
  now <- currentTime
  
  let initialMetrics =
        { totalBalance: 0.0
        , growthRate24h: 0.0
        , growth7d: 0.0
        , contributionsByType:
          { swap: 0.0
          , staking: 0.0
          , leverage: 0.0
          , gateway: 0.0
          }
        , utilizationRate: 0.0
        , lastUpdated: now
        }
  
  metricsCache <- new initialMetrics
  lastUpdate <- new now
  distribution <- new defaultPOLDistribution
  tokenPOLBalances <- new Map.empty
  
  pure { balance, contributions, metricsCache, lastUpdate, distribution, tokenPOLBalances }

--------------------------------------------------------------------------------
-- POL Operations
--------------------------------------------------------------------------------

-- Contribute fees to POL
contributeToPOL :: POLState -> LendingTerms -> Number -> Maybe Int -> Effect Unit
contributeToPOL state lendingTerms amount positionId = do
  -- Update balance
  _ <- modify_ (_ + amount) state.balance
  
  -- Record contribution
  now <- currentTime
  let contribution =
        { amount: amount
        , source: lendingTerms
        , timestamp: now
        , positionId: positionId
        }
  
  contributions <- read state.contributions
  write (contribution : contributions) state.contributions
  
  -- Update metrics if needed
  lastUpdate <- read state.lastUpdate
  when (now - lastUpdate > 300000.0) $ do  -- Update every 5 minutes
    updateMetrics state

-- Calculate and capture JitoSOL/FeelsSOL differential as POL
-- This should be called periodically (e.g., every block) to capture staking rewards
captureStakingRewards :: POLState -> Number -> Number -> Effect Unit
captureStakingRewards state totalFeelsSOLSupply jitoSOLPrice = do
  -- Simulate a constant staking yield of ~5% APY, accrued per block.
  -- This is more stable than relying on price fluctuations.
  let dailyYield = 0.05 / 365.0
      blockYield = dailyYield / 43200.0 -- Assuming 2 blocks per second
      
      -- The differential value is the yield earned on the total supply
      differentialValue = totalFeelsSOLSupply * jitoSOLPrice * blockYield
  
  -- Only capture positive differentials (JitoSOL should always appreciate)
  when (differentialValue > 0.0) $ do
    -- Contribute to POL
    _ <- modify_ (_ + differentialValue) state.balance
    
    -- Also contribute to FeelsSOL token POL specifically
    contributeToTokenPOL state FeelsSOL differentialValue
    
    -- Record as a staking contribution
    now <- currentTime
    let contribution =
          { amount: differentialValue
          , source: StakingTerms Days90  -- Staking rewards represented as long-term staking
          , timestamp: now
          , positionId: Nothing
          }
    
    contributions <- read state.contributions
    write (contribution : contributions) state.contributions

-- Get current POL balance
getPOLBalance :: POLState -> Effect Number
getPOLBalance state = read state.balance

-- Contribute to a specific token's POL
contributeToTokenPOL :: POLState -> TokenType -> Number -> Effect Unit
contributeToTokenPOL state tokenType amount = do
  -- Update total POL balance
  _ <- modify_ (_ + amount) state.balance
  
  -- Update per-token balance
  now <- currentTime
  let tokenKey = show tokenType
  
  tokenBalances <- read state.tokenPOLBalances
  let updatedInfo = case Map.lookup tokenKey tokenBalances of
        Nothing -> 
          { balance: amount
          , lastContribution: now
          , totalContributions: amount
          }
        Just info ->
          { balance: info.balance + amount
          , lastContribution: now
          , totalContributions: info.totalContributions + amount
          }
  
  let updatedBalances = Map.insert tokenKey updatedInfo tokenBalances
  write updatedBalances state.tokenPOLBalances

-- Get POL balance for a specific token
getTokenPOLBalance :: POLState -> TokenType -> Effect Number
getTokenPOLBalance state tokenType = do
  tokenBalances <- read state.tokenPOLBalances
  let tokenKey = show tokenType
  pure $ case Map.lookup tokenKey tokenBalances of
    Nothing -> 0.0
    Just info -> info.balance

-- Get all token POL balances
getAllTokenPOLBalances :: POLState -> Effect (Array (Tuple String TokenPOLInfo))
getAllTokenPOLBalances state = do
  tokenBalances <- read state.tokenPOLBalances
  pure $ Map.toUnfoldable tokenBalances

-- Get POL metrics
getPOLMetrics :: POLState -> Effect POLMetrics
getPOLMetrics state = do
  -- Check if metrics need updating
  now <- currentTime
  lastUpdate <- read state.lastUpdate
  
  when (now - lastUpdate > 300000.0) $ do  -- Update every 5 minutes
    updateMetrics state
  
  read state.metricsCache

--------------------------------------------------------------------------------
-- POL Tick Creation
--------------------------------------------------------------------------------

-- Create POL lending offers using default distribution (backward compatibility)
createPOLTicks :: POLState -> LendingBook -> Effect (Array LendingRecord)
createPOLTicks state lendingBook = createPOLTicksWithDistribution state lendingBook 1.0

-- Create POL lending offers according to distribution curve
createPOLTicksWithDistribution :: POLState -> LendingBook -> Number -> Effect (Array LendingRecord)
createPOLTicksWithDistribution state lendingBook spotPrice = do
  balance <- getPOLBalance state
  dist <- read state.distribution
  
  if balance <= 0.0
  then pure []
  else do
    -- Generate offers for each range
    floorOffers <- generateRangeOffers 
          lendingBook
          (balance * dist.floorWeight)
          dist.floorRange
          spotPrice
          dist.ticksPerRange
          
    spotOffers <- generateRangeOffers
          lendingBook
          (balance * dist.spotWeight)
          dist.spotRange
          spotPrice
          dist.ticksPerRange
          
    discoveryOffers <- generateRangeOffers
          lendingBook
          (balance * dist.discoveryWeight)
          dist.discoveryRange
          spotPrice
          dist.ticksPerRange
    
    pure (floorOffers <> spotOffers <> discoveryOffers)

-- Generate offers for a specific range
generateRangeOffers :: LendingBook -> Number -> Range -> Number -> Int -> Effect (Array LendingRecord)
generateRangeOffers lendingBook totalAmount priceRange _ numTicks = 
  if numTicks <= 0 || totalAmount <= 0.0
  then pure []
  else do
    let amountPerOffer = totalAmount / Int.toNumber numTicks
        step = (priceRange.max - priceRange.min) / Int.toNumber numTicks
        
        generateOffer i = do
          let ratio = priceRange.min + (Int.toNumber i * step)
              collateralRatio = 1.0 / ratio  -- Inverse for collateral requirement
              collateralAmount = amountPerOffer * collateralRatio
          result <- createLendOffer
            lendingBook
            "POL"
            FeelsSOL
            amountPerOffer
            JitoSOL  -- POL uses JitoSOL as collateral
            collateralAmount
            SwapTerms  -- POL provides swap liquidity
          case result of
            Left _ -> pure Nothing  -- Skip failed offers
            Right offer -> pure $ Just offer
            
    maybeOffers <- traverse generateOffer (range 0 (numTicks - 1))
    pure $ catMaybes maybeOffers

-- Check if a position is POL-owned (never matures)
isPOLPosition :: forall r. { owner :: String | r } -> Boolean
isPOLPosition position = position.owner == "POL"

--------------------------------------------------------------------------------
-- Metrics Calculation
--------------------------------------------------------------------------------

-- Update cached metrics
updateMetrics :: POLState -> Effect Unit
updateMetrics state = do
  now <- currentTime
  balance <- read state.balance
  contributions <- read state.contributions
  
  -- Calculate growth rates
  let dayAgo = now - (24.0 * 60.0 * 60.0 * 1000.0)
      weekAgo = now - (7.0 * 24.0 * 60.0 * 60.0 * 1000.0)
      
      recentContributions = filter (\c -> c.timestamp > dayAgo) contributions
      weekContributions = filter (\c -> c.timestamp > weekAgo) contributions
      
      growth24h = sum (map _.amount recentContributions)
      growth7d = sum (map _.amount weekContributions)
      
      -- Growth rates as percentage
      growthRate24h = if balance > 0.0 then growth24h / balance else 0.0
      growthRate7d = if balance > 0.0 then growth7d / balance else 0.0
  
  -- Calculate contribution breakdown
  let breakdown = calculateBreakdown contributions
  
  -- Update metrics
  let metrics =
        { totalBalance: balance
        , growthRate24h: growthRate24h
        , growth7d: growthRate7d
        , contributionsByType: breakdown
        , utilizationRate: 0.0  -- TODO: Calculate from active POL positions
        , lastUpdated: now
        }
  
  write metrics state.metricsCache
  write now state.lastUpdate

-- Calculate contribution breakdown by type
calculateBreakdown :: Array POLContribution -> ContributionBreakdown
calculateBreakdown contributions =
  let swapTotal = sum $ map _.amount $ filter (\c -> case c.source of
        SwapTerms -> true
        _ -> false) contributions
      stakingTotal = sum $ map _.amount $ filter (\c -> case c.source of
        StakingTerms _ -> true
        _ -> false) contributions  
      leverageTotal = sum $ map _.amount $ filter (\c -> case c.source of
        LeverageTerms _ -> true
        _ -> false) contributions
  in { swap: swapTotal
     , staking: stakingTotal
     , leverage: leverageTotal
     , gateway: 0.0  -- No longer used, keeping for compatibility
     }

--------------------------------------------------------------------------------
-- Monitoring Functions
--------------------------------------------------------------------------------

-- Take a simple snapshot of current POL state
takeSnapshot :: POLState -> Effect String
takeSnapshot polState = do
  metrics <- getPOLMetrics polState
  now <- currentTime
  
  pure $ """POL Snapshot (""" <> show now <> """):
  Balance: """ <> show metrics.totalBalance <> """
  Utilization: """ <> show (metrics.utilizationRate * 100.0) <> """%
  24h Growth: """ <> show (metrics.growthRate24h * 100.0) <> """%"""

-- Get current utilization rate
getUtilizationRate :: POLState -> Effect Number
getUtilizationRate polState = do
  metrics <- getPOLMetrics polState
  pure metrics.utilizationRate

-- Get POL value for historical tracking
getPOLValueForChart :: POLState -> Effect { timestamp :: Number, value :: Number }
getPOLValueForChart polState = do
  balance <- getPOLBalance polState
  now <- currentTime
  pure { timestamp: now, value: balance }
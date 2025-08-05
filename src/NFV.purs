-- Network Floor Value (NFV) management for the Everything is Lending protocol.
-- NFV represents permanently locked protocol-owned FeelsSOL that acts as a
-- universal lender of last resort, growing from fees across all position types.
module NFV
  ( NFVState
  , NFVMetrics
  , NFVDistribution
  , NFVContribution
  , ContributionBreakdown
  , Range
  , TokenNFVInfo
  , initNFV
  , contributeToNFV
  , contributeToTokenNFV
  , getNFVBalance
  , getTokenNFVBalance
  , getAllTokenNFVBalances
  , getNFVMetrics
  , createNFVTicks
  , createNFVTicksWithDistribution
  , isNFVPosition
  , defaultNFVDistribution
  , takeSnapshot
  , getUtilizationRate
  , getNFVValueForChart
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), filter, catMaybes)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingTerms(..), LendingSide(..))
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

--------------------------------------------------------------------------------
-- NFV Types
--------------------------------------------------------------------------------

-- Range for tick placement
type Range =
  { min :: Number  -- Minimum price/ratio multiplier
  , max :: Number  -- Maximum price/ratio multiplier
  }

-- NFV distribution curve parameters
type NFVDistribution =
  { floorWeight :: Number      -- Portion allocated below spot (e.g., 0.6)
  , nearSpotWeight :: Number   -- Portion allocated near spot (e.g., 0.3)
  , tailWeight :: Number       -- Portion allocated above spot (e.g., 0.1)
  , floorRange :: Range        -- Price range for floor liquidity
  , nearSpotRange :: Range     -- Price range for near-spot liquidity
  , tailRange :: Range         -- Price range for tail liquidity
  , ticksPerRange :: Int       -- Number of ticks to create per range
  }

-- Per-token NFV information
type TokenNFVInfo =
  { balance :: Number           -- NFV balance for this token pair
  , lastContribution :: Number  -- Timestamp of last contribution
  , totalContributions :: Number -- Total contributions to this pair
  }

-- NFV state tracking
type NFVState =
  { balance :: Ref Number              -- Total NFV balance in FeelsSOL
  , contributions :: Ref (Array NFVContribution)  -- History of contributions
  , metricsCache :: Ref NFVMetrics    -- Cached metrics
  , lastUpdate :: Ref Number           -- Last metrics update timestamp
  , distribution :: Ref NFVDistribution  -- Current distribution parameters
  , tokenNFVBalances :: Ref (Map String TokenNFVInfo)  -- Per-token NFV balances
  }

-- Individual contribution record
type NFVContribution =
  { amount :: Number
  , source :: LendingTerms
  , timestamp :: Number
  , positionId :: Maybe Int  -- Associated position if applicable
  }

-- NFV metrics for monitoring
type NFVMetrics =
  { totalBalance :: Number
  , growthRate24h :: Number      -- 24-hour growth rate
  , growthRate7d :: Number       -- 7-day growth rate
  , contributionsByType :: ContributionBreakdown
  , utilizationRate :: Number    -- How much NFV is actively lending
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

-- Default NFV distribution curve
defaultNFVDistribution :: NFVDistribution
defaultNFVDistribution =
  { floorWeight: 0.6        -- 60% below spot
  , nearSpotWeight: 0.3     -- 30% near spot
  , tailWeight: 0.1         -- 10% above spot
  , floorRange: { min: 0.5, max: 0.9 }      -- 0.5x to 0.9x spot
  , nearSpotRange: { min: 0.9, max: 1.1 }   -- 0.9x to 1.1x spot
  , tailRange: { min: 1.1, max: 2.0 }       -- 1.1x to 2.0x spot
  , ticksPerRange: 5        -- 5 ticks per range = 15 total
  }

--------------------------------------------------------------------------------
-- NFV Initialization
--------------------------------------------------------------------------------

-- Initialize NFV system
initNFV :: Effect NFVState
initNFV = do
  balance <- new 0.0
  contributions <- new []
  now <- currentTime
  
  let initialMetrics =
        { totalBalance: 0.0
        , growthRate24h: 0.0
        , growthRate7d: 0.0
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
  distribution <- new defaultNFVDistribution
  tokenNFVBalances <- new Map.empty
  
  pure { balance, contributions, metricsCache, lastUpdate, distribution, tokenNFVBalances }

--------------------------------------------------------------------------------
-- NFV Operations
--------------------------------------------------------------------------------

-- Contribute fees to NFV
contributeToNFV :: NFVState -> LendingTerms -> Number -> Maybe Int -> Effect Unit
contributeToNFV state lendingTerms amount positionId = do
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

-- Get current NFV balance
getNFVBalance :: NFVState -> Effect Number
getNFVBalance state = read state.balance

-- Contribute to a specific token's NFV
contributeToTokenNFV :: NFVState -> TokenType -> Number -> Effect Unit
contributeToTokenNFV state tokenType amount = do
  -- Update total NFV balance
  _ <- modify_ (_ + amount) state.balance
  
  -- Update per-token balance
  now <- currentTime
  let tokenKey = show tokenType
  
  tokenBalances <- read state.tokenNFVBalances
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
  write updatedBalances state.tokenNFVBalances

-- Get NFV balance for a specific token
getTokenNFVBalance :: NFVState -> TokenType -> Effect Number
getTokenNFVBalance state tokenType = do
  tokenBalances <- read state.tokenNFVBalances
  let tokenKey = show tokenType
  pure $ case Map.lookup tokenKey tokenBalances of
    Nothing -> 0.0
    Just info -> info.balance

-- Get all token NFV balances
getAllTokenNFVBalances :: NFVState -> Effect (Array (Tuple String TokenNFVInfo))
getAllTokenNFVBalances state = do
  tokenBalances <- read state.tokenNFVBalances
  pure $ Map.toUnfoldable tokenBalances

-- Get NFV metrics
getNFVMetrics :: NFVState -> Effect NFVMetrics
getNFVMetrics state = do
  -- Check if metrics need updating
  now <- currentTime
  lastUpdate <- read state.lastUpdate
  
  when (now - lastUpdate > 300000.0) $ do  -- Update every 5 minutes
    updateMetrics state
  
  read state.metricsCache

--------------------------------------------------------------------------------
-- NFV Tick Creation
--------------------------------------------------------------------------------

-- Create NFV lending offers using default distribution (backward compatibility)
createNFVTicks :: NFVState -> LendingBook -> Effect (Array LendingRecord)
createNFVTicks state lendingBook = createNFVTicksWithDistribution state lendingBook 1.0

-- Create NFV lending offers according to distribution curve
createNFVTicksWithDistribution :: NFVState -> LendingBook -> Number -> Effect (Array LendingRecord)
createNFVTicksWithDistribution state lendingBook spotPrice = do
  balance <- getNFVBalance state
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
          
    nearSpotOffers <- generateRangeOffers
          lendingBook
          (balance * dist.nearSpotWeight)
          dist.nearSpotRange
          spotPrice
          dist.ticksPerRange
          
    tailOffers <- generateRangeOffers
          lendingBook
          (balance * dist.tailWeight)
          dist.tailRange
          spotPrice
          dist.ticksPerRange
    
    pure (floorOffers <> nearSpotOffers <> tailOffers)

-- Generate offers for a specific range
generateRangeOffers :: LendingBook -> Number -> Range -> Number -> Int -> Effect (Array LendingRecord)
generateRangeOffers lendingBook totalAmount priceRange spotPrice numTicks =
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
            "NFV"
            FeelsSOL
            amountPerOffer
            JitoSOL  -- NFV uses JitoSOL as collateral
            collateralAmount
            SwapTerms  -- NFV provides swap liquidity
          case result of
            Left _ -> pure Nothing  -- Skip failed offers
            Right offer -> pure $ Just offer
            
    maybeOffers <- traverse generateOffer (range 0 (numTicks - 1))
    pure $ catMaybes maybeOffers

-- Check if a position is NFV-owned (never matures)
isNFVPosition :: forall r. { owner :: String | r } -> Boolean
isNFVPosition position = position.owner == "NFV"

--------------------------------------------------------------------------------
-- Metrics Calculation
--------------------------------------------------------------------------------

-- Update cached metrics
updateMetrics :: NFVState -> Effect Unit
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
        , growthRate7d: growthRate7d
        , contributionsByType: breakdown
        , utilizationRate: 0.0  -- TODO: Calculate from active NFV positions
        , lastUpdated: now
        }
  
  write metrics state.metricsCache
  write now state.lastUpdate

-- Calculate contribution breakdown by type
calculateBreakdown :: Array NFVContribution -> ContributionBreakdown
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

-- Take a simple snapshot of current NFV state
takeSnapshot :: NFVState -> Effect String
takeSnapshot nfvState = do
  metrics <- getNFVMetrics nfvState
  now <- currentTime
  
  pure $ """NFV Snapshot (""" <> show now <> """):
  Balance: """ <> show metrics.totalBalance <> """
  Utilization: """ <> show (metrics.utilizationRate * 100.0) <> """%
  24h Growth: """ <> show (metrics.growthRate24h * 100.0) <> """%"""

-- Get current utilization rate
getUtilizationRate :: NFVState -> Effect Number
getUtilizationRate nfvState = do
  metrics <- getNFVMetrics nfvState
  pure metrics.utilizationRate

-- Get NFV value for historical tracking
getNFVValueForChart :: NFVState -> Effect { timestamp :: Number, value :: Number }
getNFVValueForChart nfvState = do
  balance <- getNFVBalance nfvState
  now <- currentTime
  pure { timestamp: now, value: balance }
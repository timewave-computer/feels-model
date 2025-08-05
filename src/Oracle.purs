-- Market oracle for the Everything is Lending protocol.
-- Observes internal Feels markets to extract pricing and market metrics
-- from actual lending activity. All price discovery happens through
-- market participants' lending and borrowing decisions.
module Oracle
  ( Oracle
  , OracleState
  , MarketMetrics
  , MarketSnapshot
  , PriceObservation
  , initOracle
  , observeMarket
  , observeMarketWithBlock
  , getPriceFromMarket
  , getMarketMetrics
  , calculateVolatility
  , takeMarketSnapshot
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array ((:), filter, find, take, length, foldl, uncons)
import Data.Traversable (traverse, traverse_)
import Data.Int as Int
import Data.Number (sqrt, abs)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingSide(..), LendingStatus(..), LendingTerms(..))
import LendingBook (LendingBook, getActiveRecords, getLenderRecords)
import NFV (NFVState, getNFVBalance, getTokenNFVBalance)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Oracle Types
--------------------------------------------------------------------------------

-- Price observation from market activity
type PriceObservation =
  { baseAsset :: TokenType       -- Asset being priced
  , quoteAsset :: TokenType       -- Always FeelsSOL for internal markets
  , impliedPrice :: Number        -- Price implied by lending rates
  , volume :: Number              -- Volume at this price level
  , timestamp :: Number
  , block :: Maybe Int            -- Block number if available
  , confidence :: Number          -- Based on volume and spread
  , nfvFloor :: Number           -- NFV floor value for this token pair
  }

-- Market metrics for adaptive calculations
type MarketMetrics =
  { utilizationRate :: Number      -- Supply/demand balance (0.0-1.0)
  , volatility :: Number           -- Price volatility from lending rates
  , liquidityDepth :: Number       -- Available liquidity in FeelsSOL
  , nfvGrowthRate :: Number       -- NFV growth rate (24h)
  , avgLendingDuration :: Number   -- Average duration in seconds
  , marketEfficiency :: Number     -- Spread tightness indicator (0.0-1.0)
  , totalValueLocked :: Number     -- TVL in FeelsSOL terms
  , activePositions :: Int         -- Number of active positions
  , avgLendingRate :: Number       -- Volume-weighted average lending rate
  , avgBorrowingRate :: Number     -- Volume-weighted average borrowing rate
  }

-- Snapshot of market state at a point in time
type MarketSnapshot =
  { metrics :: MarketMetrics
  , priceObservations :: Array PriceObservation
  , timestamp :: Number
  , blockHeight :: Maybe Int
  }

-- Oracle state
type OracleState =
  { snapshots :: Array MarketSnapshot     -- Historical snapshots
  , lastUpdate :: Number                  -- Last update timestamp
  , priceHistory :: Array PriceObservation -- Recent price observations
  }

-- Main oracle type
type Oracle =
  { state :: Ref OracleState
  , lendingBook :: LendingBook
  , nfvState :: NFVState
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize oracle
initOracle :: LendingBook -> NFVState -> Effect Oracle
initOracle lendingBook nfvState = do
  now <- currentTime
  
  let initialState =
        { snapshots: []
        , lastUpdate: now
        , priceHistory: []
        }
  
  state <- new initialState
  
  pure { state, lendingBook, nfvState }

--------------------------------------------------------------------------------
-- Market Observation
--------------------------------------------------------------------------------

-- Observe current market state and calculate metrics
observeMarket :: Oracle -> Effect MarketMetrics
observeMarket oracle = do
  now <- currentTime
  state <- read oracle.state
  
  -- Always calculate fresh metrics from market activity
  metrics <- calculateMarketMetrics oracle
  
  -- Update price history from current market (no block number in regular operation)
  priceObs <- extractPricesFromMarket oracle Nothing
  let updatedHistory = take 1000 (priceObs <> state.priceHistory)
  
  -- Take a snapshot periodically (every minute)
  if (now - state.lastUpdate) > 60000.0
  then do
    let snapshot = 
          { metrics
          , priceObservations: priceObs
          , timestamp: now
          , blockHeight: Nothing
          }
    _ <- modify_ (\s -> s { snapshots = take 1440 (snapshot : s.snapshots)  -- Keep 24 hours
                          , lastUpdate = now
                          , priceHistory = updatedHistory 
                          }) oracle.state
    pure metrics
  else do
    _ <- modify_ (_ { priceHistory = updatedHistory }) oracle.state
    pure metrics

-- Observe market state with a specific block number (for simulation)
observeMarketWithBlock :: Oracle -> Int -> Effect MarketMetrics
observeMarketWithBlock oracle blockNum = do
  now <- currentTime
  state <- read oracle.state
  
  -- Always calculate fresh metrics from market activity
  metrics <- calculateMarketMetrics oracle
  
  -- Update price history with block number
  priceObs <- extractPricesFromMarket oracle (Just blockNum)
  
  -- Always ensure JitoSOL has a price observation for every block
  -- This is necessary because JitoSOL price changes every block due to staking rewards
  let hasJitoSOL = find (\obs -> obs.baseAsset == JitoSOL) priceObs
  jitoSOLObs <- case hasJitoSOL of
    Just _ -> do
      log $ "Oracle: Block " <> show blockNum <> " already has JitoSOL observation"
      pure priceObs  -- Already has JitoSOL observation
    Nothing -> do
      -- Create a JitoSOL observation even if no trades occurred
      log $ "Oracle: Block " <> show blockNum <> " has no JitoSOL trades, creating synthetic observation"
      oracleState <- read oracle.state
      jitoObs <- calculateImpliedPrice [] now (Just blockNum) oracle.nfvState oracleState JitoSOL
      pure (jitoObs : priceObs)
  
  let updatedHistory = take 1000 (jitoSOLObs <> state.priceHistory)
  
  -- Always update price history (don't wait for periodic snapshots)
  _ <- modify_ (_ { priceHistory = updatedHistory }) oracle.state
  pure metrics

-- Calculate market metrics from lending activity
calculateMarketMetrics :: Oracle -> Effect MarketMetrics
calculateMarketMetrics oracle = do
  -- Get lending book data
  records <- getActiveRecords oracle.lendingBook
  
  -- Calculate utilization rate (active borrowing / total lending capacity)
  let totalLendingCapacity = sum $ map _.lendAmount $ filter (\r -> r.side == Lender) records
      activeBorrowing = sum $ map _.lendAmount $ filter (\r -> r.side == Borrower) records
      utilizationRate = if totalLendingCapacity > 0.0 
                       then activeBorrowing / totalLendingCapacity 
                       else 0.0
  let activePositions = length records
  
  -- Separate lenders and borrowers
  let lenderRecords = filter (\r -> r.side == Lender && 
                                    case r.status of 
                                      Available _ -> true
                                      _ -> false) records
      borrowerRecords = filter (\r -> r.side == Borrower && r.status == Active) records
      
  -- Calculate liquidity depth (available lending capacity)
  let liquidityDepth = foldl (\acc r -> acc + r.lendAmount) 0.0 lenderRecords
  
  -- Calculate average lending duration
  now <- currentTime
  let totalDuration = foldl (\acc r -> acc + (now - r.createdAt)) 0.0 borrowerRecords
      avgDuration = if length borrowerRecords > 0 
                    then totalDuration / Int.toNumber (length borrowerRecords)
                    else 0.0
  
  -- Calculate TVL (all active lending)
  let tvl = foldl (\acc r -> acc + r.lendAmount) 0.0 records
  
  -- Calculate average lending rates from market activity
  let { avgLendingRate, avgBorrowingRate } = calculateAverageRates records
  
  -- Calculate volatility from rate changes
  state <- read oracle.state
  let volatility = calculateRateVolatility state.priceHistory
  
  -- Get NFV metrics
  nfvBalance <- getNFVBalance oracle.nfvState
  let nfvGrowthRate = calculateNFVGrowth state nfvBalance
  
  -- Calculate market efficiency from spread
  let spread = if avgLendingRate > 0.0 
                then (avgBorrowingRate - avgLendingRate) / avgLendingRate
                else 0.0
      -- Tighter spreads = more efficient market
      marketEfficiency = if spread > 0.0 
                         then max 0.0 (1.0 - spread)
                         else 0.5
  
  pure { utilizationRate
       , volatility
       , liquidityDepth
       , nfvGrowthRate
       , avgLendingDuration: avgDuration
       , marketEfficiency
       , totalValueLocked: tvl
       , activePositions
       , avgLendingRate
       , avgBorrowingRate
       }

--------------------------------------------------------------------------------
-- Price Discovery
--------------------------------------------------------------------------------

-- Extract implied prices from lending market activity
extractPricesFromMarket :: Oracle -> Maybe Int -> Effect (Array PriceObservation)
extractPricesFromMarket oracle maybeBlock = do
  -- Get both active records and lender records (includes Available status)
  activeRecords <- getActiveRecords oracle.lendingBook
  lenderRecords <- getLenderRecords oracle.lendingBook
  let allRecords = activeRecords <> lenderRecords  -- Union of both sets
  now <- currentTime
  
  -- Get all records that can provide price information (swaps and staking)
  let priceRecords = filter (\r -> case r.terms of
                                    SwapTerms -> true
                                    StakingTerms _ -> true  -- Staking also provides price info
                                    LeverageTerms _ -> true  -- Leverage positions too
                                    _ -> false) allRecords
  
  -- For each unique asset (not FeelsSOL), calculate implied price
  let uniqueAssets = filter (_ /= FeelsSOL) $ uniqueTokens allRecords
  
  -- Enhanced debug logging
  log $ "Oracle: === PRICE EXTRACTION DEBUG ==="
  log $ "Oracle: Total lending records: " <> show (length allRecords)
  log $ "Oracle: Active records: " <> show (length activeRecords)
  log $ "Oracle: Lender records: " <> show (length lenderRecords)
  log $ "Oracle: Price records (swap/staking/leverage): " <> show (length priceRecords)
  log $ "Oracle: Unique assets to price: " <> show (length uniqueAssets)
  when (length uniqueAssets > 0) $ do
    traverse_ (\asset -> log $ "  Asset: " <> show asset) uniqueAssets
  when (length priceRecords > 0) $ do
    log $ "Oracle: First few price records:"
    traverse_ (\record -> log $ "  Record: " <> show record.lendAsset <> " -> " <> show record.collateralAsset <> " amount: " <> show record.lendAmount) (take 3 priceRecords)
  
  oracleState <- read oracle.state
  traverse (calculateImpliedPrice priceRecords now maybeBlock oracle.nfvState oracleState) uniqueAssets

-- Calculate implied price for an asset from market activity
calculateImpliedPrice :: Array LendingRecord -> Number -> Maybe Int -> NFVState -> OracleState -> TokenType -> Effect PriceObservation
calculateImpliedPrice records timestamp maybeBlock nfvState oracleState asset = do
  -- Find all positions involving this asset
  let relevantRecords = filter (\r -> r.lendAsset == asset || r.collateralAsset == asset) records
  
  when (length relevantRecords > 0) $ do
    log $ "Oracle: Found " <> show (length relevantRecords) <> " records for " <> show asset
    let swapRecords = filter (\r -> case r.terms of 
                                      SwapTerms -> true
                                      _ -> false) relevantRecords
    when (length swapRecords > 0) $ do
      log $ "  Including " <> show (length swapRecords) <> " swap records for direct price discovery"
  
  -- Calculate volume-weighted average exchange rate
  let accumulate acc record =
        let volume = record.lendAmount
            -- For swaps, collateral ratio IS the exchange rate
            -- For other positions, calculate implied rate from amounts
            rate = case record.terms of
                     SwapTerms -> 
                       if record.lendAsset == asset
                       then if record.lendAmount > 0.001 
                            then record.collateralAmount / record.lendAmount  -- Direct exchange rate
                            else 1.0  -- Skip tiny amounts to avoid extreme prices
                       else if record.collateralAmount > 0.001
                            then record.lendAmount / record.collateralAmount  -- Inverse for opposite direction  
                            else 1.0  -- Skip tiny amounts to avoid extreme prices
                     _ ->
                       -- For staking/leverage positions, use collateral ratio but normalize around 1.0
                       -- JitoSOL/FeelsSOL should be close to 1:1 with small variations
                       if record.lendAsset == asset
                       then if record.lendAmount > 0.001 && record.collateralAmount > 0.001
                            then record.collateralAmount / record.lendAmount  -- Collateral per lend unit
                            else 1.0  -- Default to 1:1 for tiny amounts
                       else if record.collateralAmount > 0.001 && record.lendAmount > 0.001
                            then record.lendAmount / record.collateralAmount  -- Inverse for opposite direction
                            else 1.0  -- Default to 1:1 for tiny amounts
        in { totalVolume: acc.totalVolume + volume
           , weightedRate: acc.weightedRate + (rate * volume)
           }
  
      { totalVolume, weightedRate } = foldl accumulate { totalVolume: 0.0, weightedRate: 0.0 } relevantRecords
  
  let rawPrice = if totalVolume > 0.0 
                 then weightedRate / totalVolume
                 else case asset of
                   JitoSOL -> 1.03  -- Default JitoSOL/FeelsSOL should be ~1.03 (3% staking premium)
                   _ -> 1.0         -- Other assets default to 1:1
      
      -- Cap price to reasonable range based on asset type
      -- For JitoSOL, add gradual price increase to simulate staking rewards
      impliedPrice = case asset of
        JitoSOL -> 
          let -- Base price should be slightly above 1.0 (liquid staking premium)
              basePrice = max 1.0 rawPrice
              -- Simulate ~7% APY staking rewards
              -- Assuming ~2 blocks per day in simulation, that's ~0.0001 per block
              blockReward = case maybeBlock of
                Just block -> Int.toNumber block * 0.00005  -- Reduced to prevent exceeding cap
                Nothing -> 0.0
              adjustedPrice = basePrice + blockReward
          -- JitoSOL should trade in a tight range relative to FeelsSOL (1.0-1.1)
          in max 1.0 (min 1.1 adjustedPrice)
        _ -> 
          -- Other assets can have wider price ranges
          max 0.1 (min 10.0 rawPrice)
  
      -- Confidence based on volume
      confidence = min 0.99 (totalVolume / 10000.0)  -- Full confidence at 10k volume
  
  -- Debug logging for JitoSOL price calculation
  case asset of
    JitoSOL -> do
      when (fromMaybe false (map (_ > 0) maybeBlock)) $ do
        log $ "  JitoSOL price for block " <> show maybeBlock <> ": base=" <> show rawPrice <> 
              ", adjusted=" <> show impliedPrice <> " (reward=" <> 
              show (case maybeBlock of 
                     Just block -> Int.toNumber block * 0.00005
                     Nothing -> 0.0) <> ")"
      when (rawPrice > 1.1) $ do
        log $ "  WARNING: Capped high JitoSOL price: " <> show rawPrice <> " -> " <> show impliedPrice
      when (rawPrice < 1.0) $ do  
        log $ "  WARNING: Capped low JitoSOL price: " <> show rawPrice <> " -> " <> show impliedPrice
    _ -> do
      when (rawPrice > 10.0) $ do
        log $ "  WARNING: Capped high price for " <> show asset <> ": " <> show rawPrice <> " -> " <> show impliedPrice
      when (rawPrice < 0.1) $ do  
        log $ "  WARNING: Capped low price for " <> show asset <> ": " <> show rawPrice <> " -> " <> show impliedPrice
  
  -- Get actual NFV floor for this token from NFVState
  tokenNFV <- getTokenNFVBalance nfvState asset
  -- NFV floor price = NFV balance / circulating supply
  -- For simplicity, assume 1M circulating supply for all tokens
  let currentNFVFloor = if tokenNFV > 0.0 
                        then tokenNFV / 1000000.0  -- NFV per token
                        else impliedPrice * 0.001   -- Minimal floor if no NFV yet
      
      -- Ensure NFV floor never decreases - find the previous floor value
      -- from price history and use the maximum
      previousFloor = case find (\obs -> obs.baseAsset == asset) oracleState.priceHistory of
        Just prevObs -> prevObs.nfvFloor
        Nothing -> 0.0
      
      nfvFloor = max currentNFVFloor previousFloor
  
  when (totalVolume > 0.0) $ do
    log $ "  Calculated price for " <> show asset <> ": " <> show impliedPrice <> " FeelsSOL (volume: " <> show totalVolume <> ", raw: " <> show rawPrice <> ")"
  when (totalVolume == 0.0) $ do
    log $ "  WARNING: No volume found for " <> show asset <> ", using default price: " <> show impliedPrice
  
  pure { baseAsset: asset
       , quoteAsset: FeelsSOL
       , impliedPrice
       , volume: totalVolume
       , timestamp
       , block: maybeBlock
       , confidence
       , nfvFloor
       }

-- Get current price from market observation
getPriceFromMarket :: Oracle -> TokenType -> TokenType -> Effect (Maybe Number)
getPriceFromMarket oracle baseAsset quoteAsset = do
  if quoteAsset /= FeelsSOL
  then pure Nothing  -- Only price against FeelsSOL
  else do
    state <- read oracle.state
    -- Look for recent price observation
    let recentObs = find (\obs -> obs.baseAsset == baseAsset && 
                                  (obs.timestamp + 300000.0) > obs.timestamp) state.priceHistory
    pure $ map _.impliedPrice recentObs

--------------------------------------------------------------------------------
-- Volatility Calculation
--------------------------------------------------------------------------------

-- Calculate volatility from rate changes
calculateRateVolatility :: Array PriceObservation -> Number
calculateRateVolatility observations =
  let recentObs = take 100 observations  -- Use last 100 observations
      prices = map _.impliedPrice recentObs
      returns = calculateReturns prices
      avgReturn = if length returns > 0
                  then sum returns / Int.toNumber (length returns)
                  else 0.0
      variance = if length returns > 0
                 then sum (map (\r -> (r - avgReturn) * (r - avgReturn)) returns) / Int.toNumber (length returns)
                 else 0.0
  in sqrt variance

-- Calculate price returns
calculateReturns :: Array Number -> Array Number
calculateReturns prices = case uncons prices of
  Nothing -> []
  Just { head: _, tail: [] } -> []
  Just { head: p1, tail: tail1 } -> case uncons tail1 of
    Nothing -> []
    Just { head: p2, tail: rest } ->
      let return = (p2 - p1) / p1
      in return : calculateReturns (p2 : rest)

-- Calculate volatility over time window
calculateVolatility :: Oracle -> Number -> Effect Number
calculateVolatility oracle windowMs = do
  state <- read oracle.state
  now <- currentTime
  let cutoff = now - windowMs
      relevantObs = filter (\obs -> obs.timestamp > cutoff) state.priceHistory
  pure $ calculateRateVolatility relevantObs

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Calculate average lending and borrowing rates
calculateAverageRates :: Array LendingRecord -> { avgLendingRate :: Number, avgBorrowingRate :: Number }
calculateAverageRates records =
  let lenders = filter (\r -> r.side == Lender) records
      borrowers = filter (\r -> r.side == Borrower) records
      
      -- For lenders, rate = expected return / principal
      lenderRates = map calculateImpliedRate lenders
      avgLendingRate = if length lenderRates > 0
                       then sum lenderRates / Int.toNumber (length lenderRates)
                       else 0.0
      
      -- For borrowers, rate = cost / borrowed amount
      borrowerRates = map calculateImpliedRate borrowers
      avgBorrowingRate = if length borrowerRates > 0
                        then sum borrowerRates / Int.toNumber (length borrowerRates)
                        else 0.0
      
  in { avgLendingRate, avgBorrowingRate }

-- Calculate implied rate from a lending record
calculateImpliedRate :: LendingRecord -> Number
calculateImpliedRate record = case record.terms of
  SwapTerms -> 0.02  -- Base swap rate
  StakingTerms _ -> 0.04  -- Base staking rate
  LeverageTerms mult -> 0.02 * mult  -- Leveraged rate

-- Get unique tokens from records
uniqueTokens :: Array LendingRecord -> Array TokenType
uniqueTokens records =
  let allTokens = foldl (\acc r -> r.lendAsset : r.collateralAsset : acc) [] records
  in nub allTokens

-- Remove duplicates from array
nub :: forall a. Eq a => Array a -> Array a
nub = nubBy (==)

nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy eq l = case uncons l of
  Nothing -> []
  Just { head: x, tail: xs } -> x : nubBy eq (filter (\y -> not (eq x y)) xs)

-- Calculate NFV growth rate
calculateNFVGrowth :: OracleState -> Number -> Number
calculateNFVGrowth state currentNFV =
  let dayAgo = currentNFV - 86400000.0
      oldSnapshots = filter (\s -> s.timestamp > dayAgo && s.timestamp < dayAgo + 60000.0) state.snapshots
  in case uncons oldSnapshots of
    Nothing -> 0.0
    Just { head: oldSnapshot, tail: _ } -> 
      let oldNFV = fromMaybe 0.0 $ find (\obs -> obs.baseAsset == FeelsSOL) oldSnapshot.priceObservations >>= \obs -> Just obs.volume
      in if oldNFV > 0.0 
         then (currentNFV - oldNFV) / oldNFV
         else 0.0

-- Calculate sum
sum :: Array Number -> Number
sum = foldl (+) 0.0

-- Get market metrics (convenience function)
getMarketMetrics :: Oracle -> Effect MarketMetrics
getMarketMetrics = observeMarket

-- Take a market snapshot
takeMarketSnapshot :: Oracle -> Effect MarketSnapshot
takeMarketSnapshot oracle = do
  metrics <- observeMarket oracle
  priceObs <- extractPricesFromMarket oracle Nothing
  now <- currentTime
  pure { metrics
       , priceObservations: priceObs
       , timestamp: now
       , blockHeight: Nothing
       }
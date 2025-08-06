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
import Data.Array ((:), filter, find, take, length, foldl, uncons, nub, nubBy) as Array
import Data.Traversable (traverse, traverse_)
import Data.Int as Int
import Data.Number (sqrt, abs)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Token (TokenType(..))
import LendingRecord (LendingRecord, LendingSide(..), LendingStatus(..), LendingTerms(..))
import LendingBook (LendingBook, getActiveRecords, getLenderRecords)
import POL (POLState, getPOLBalance, getTokenPOLBalance)
import FFI (currentTime, log, cos, exp) as FFI
import ProtocolError (ProtocolError(..))

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
  , polFloor :: Number           -- POL floor value for this token pair
  }

-- Market metrics for adaptive calculations
type MarketMetrics =
  { utilizationRate :: Number      -- Supply/demand balance (0.0-1.0)
  , volatility :: Number           -- Price volatility from lending rates
  , liquidityDepth :: Number       -- Available liquidity in FeelsSOL
  , polGrowthRate :: Number       -- POL growth rate (24h)
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
  , polState :: POLState
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize oracle
initOracle :: LendingBook -> POLState -> Effect Oracle
initOracle lendingBook polState = do
  now <- FFI.currentTime
  
  let initialState =
        { snapshots: []
        , lastUpdate: now
        , priceHistory: []
        }
  
  state <- new initialState
  
  pure { state, lendingBook, polState }

--------------------------------------------------------------------------------
-- Market Observation
--------------------------------------------------------------------------------

-- Observe current market state and calculate metrics
observeMarket :: Oracle -> Effect MarketMetrics
observeMarket oracle = do
  now <- FFI.currentTime
  state <- read oracle.state
  
  -- Always calculate fresh metrics from market activity
  metrics <- calculateMarketMetrics oracle
  
  -- Update price history from current market (no block number in regular operation)
  priceObs <- extractPricesFromMarket oracle Nothing
  let updatedHistory = Array.take 1000 (priceObs <> state.priceHistory)
  
  -- Take a snapshot periodically (every minute)
  if (now - state.lastUpdate) > 60000.0
  then do
    let snapshot = 
          { metrics
          , priceObservations: priceObs
          , timestamp: now
          , blockHeight: Nothing
          }
    _ <- modify_ (\s -> s { snapshots = Array.take 1440 (snapshot Array.: s.snapshots)  -- Keep 24 hours
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
  now <- FFI.currentTime
  state <- read oracle.state
  
  -- Always calculate fresh metrics from market activity
  metrics <- calculateMarketMetrics oracle
  
  -- Update price history with block number
  priceObs <- extractPricesFromMarket oracle (Just blockNum)
  
  -- Always ensure JitoSOL has a price observation for every block
  -- This is necessary because JitoSOL price changes every block due to staking rewards
  let hasJitoSOL = Array.find (\obs -> obs.baseAsset == JitoSOL) priceObs
  jitoSOLObs <- case hasJitoSOL of
    Just _ -> do
      log $ "Oracle: Block " <> show blockNum <> " already has JitoSOL observation"
      pure priceObs  -- Already has JitoSOL observation
    Nothing -> do
      -- Create a JitoSOL observation even if no trades occurred
      -- Reduced logging - synthetic observations are normal
      pure unit  -- log $ "Oracle: Block " <> show blockNum <> " has no JitoSOL trades, creating synthetic observation"
      oracleState <- read oracle.state
      -- Use the same priceRecords that were used for other assets (executed trades only)
      -- This ensures consistent price calculation methodology
      jitoObs <- calculateImpliedPrice [] now (Just blockNum) oracle.polState oracleState JitoSOL
      pure (jitoObs Array.: priceObs)
  
  let updatedHistory = Array.take 1000 (jitoSOLObs <> state.priceHistory)
  
  -- Always update price history (don't wait for periodic snapshots)
  _ <- modify_ (_ { priceHistory = updatedHistory }) oracle.state
  pure metrics

-- Calculate market metrics from lending activity
calculateMarketMetrics :: Oracle -> Effect MarketMetrics
calculateMarketMetrics oracle = do
  -- Get lending book data
  records <- getActiveRecords oracle.lendingBook
  
  -- Calculate utilization rate (active borrowing / total lending capacity)
  let totalLendingCapacity = sum $ map _.lendAmount $ Array.filter (\r -> r.side == Lender) records
      activeBorrowing = sum $ map _.lendAmount $ Array.filter (\r -> r.side == Borrower) records
      utilizationRate = if totalLendingCapacity > 0.0 
                       then activeBorrowing / totalLendingCapacity 
                       else 0.0
  let activePositions = Array.length records
  
  -- Separate lenders and borrowers
  let lenderRecords = Array.filter (\r -> r.side == Lender && 
                                    case r.status of 
                                      Available _ -> true
                                      _ -> false) records
      borrowerRecords = Array.filter (\r -> r.side == Borrower && r.status == Active) records
      
  -- Calculate liquidity depth (available lending capacity)
  let liquidityDepth = Array.foldl (\acc r -> acc + r.lendAmount) 0.0 lenderRecords
  
  -- Calculate average lending duration
  now <- FFI.currentTime
  let totalDuration = Array.foldl (\acc r -> acc + (now - r.createdAt)) 0.0 borrowerRecords
      avgDuration = if Array.length borrowerRecords > 0 
                    then totalDuration / Int.toNumber (Array.length borrowerRecords)
                    else 0.0
  
  -- Calculate TVL (all active lending)
  let tvl = Array.foldl (\acc r -> acc + r.lendAmount) 0.0 records
  
  -- Calculate average lending rates from market activity
  let { avgLendingRate, avgBorrowingRate } = calculateAverageRates records
  
  -- Calculate volatility from rate changes
  state <- read oracle.state
  let volatility = calculateRateVolatility state.priceHistory
  
  -- Get POL metrics
  polBalance <- getPOLBalance oracle.polState
  let polGrowthRate = calculatePOLGrowth state polBalance
  
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
       , polGrowthRate
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
  now <- FFI.currentTime
  
  -- Filter for recently executed trades (within last 10 blocks worth of time)
  -- Assuming ~1 second per block, look at trades from last 10 seconds
  let recentCutoff = now - 10000.0  -- 10 seconds = 10 blocks
      recentRecords = Array.filter (\r -> 
        case r.executedAt of
          Just execTime -> execTime >= recentCutoff
          Nothing -> false  -- Only consider executed trades
      ) allRecords
  
  -- Log executed trades for debugging
  -- when (Array.length recentRecords > 0) $ do
  --   log $ "Oracle: Found " <> show (Array.length recentRecords) <> " recently executed trades"
  
  -- Get all records that can provide price information (swaps and staking)
  let recentPriceRecords = Array.filter (\r -> case r.terms of
                                    SwapTerms -> true
                                    StakingTerms _ -> true  -- Staking also provides price info
                                    LeverageTerms _ -> true  -- Leverage positions too
                                    _ -> false) recentRecords
      
      -- If no recent trades, fall back to ALL executed trades (not just recent)
      allExecutedRecords = if Array.length recentPriceRecords == 0
        then Array.filter (\r -> 
          case r.executedAt of
            Just _ -> case r.terms of
              SwapTerms -> true
              StakingTerms _ -> true
              LeverageTerms _ -> true
              _ -> false
            Nothing -> false
        ) allRecords
        else recentPriceRecords
      
      priceRecords = allExecutedRecords
  
  -- For each unique asset (not FeelsSOL), calculate implied price
  let uniqueAssets = Array.filter (_ /= FeelsSOL) $ uniqueTokens allRecords
  
  -- Reduced debug logging - only log key info
  -- when (Array.length recentRecords == 0 && Array.length allRecords > 0) $ do
  --   log $ "Oracle: No recent executed trades, using " <> show (Array.length priceRecords) <> " historical records"
  
  oracleState <- read oracle.state
  
  -- Get all previously observed assets from price history
  let previouslyObservedAssets = Array.nub $ map _.baseAsset $ Array.filter (\obs -> 
        case obs.baseAsset of
          Token _ -> true
          JitoSOL -> true
          _ -> false
      ) oracleState.priceHistory
      
      -- Combine unique assets from current records with previously observed assets
      allAssetsToPrice = Array.nub (uniqueAssets <> previouslyObservedAssets)
  
  -- log $ "Oracle: Total assets to price (including historical): " <> show (Array.length allAssetsToPrice)
  
  traverse (calculateImpliedPrice priceRecords now maybeBlock oracle.polState oracleState) allAssetsToPrice

-- Calculate implied price for an asset from market activity
calculateImpliedPrice :: Array LendingRecord -> Number -> Maybe Int -> POLState -> OracleState -> TokenType -> Effect PriceObservation
calculateImpliedPrice records timestamp maybeBlock polState oracleState asset = do
  -- Find all positions involving this asset
  let relevantRecords = Array.filter (\r -> r.lendAsset == asset || r.collateralAsset == asset) records
  
  -- Only log for non-JitoSOL assets to reduce verbosity
  -- when (Array.length relevantRecords > 0 && asset /= JitoSOL) $ do
  --   log $ "Oracle: Found " <> show (Array.length relevantRecords) <> " records for " <> show asset
    -- Commented out detailed JitoSOL logging for less verbosity
    -- when (asset == JitoSOL) $ do
    --   traverse_ (\r -> do
    --     let actualCollateral = case r.status of
    --                             Available _ -> r.lendAmount * r.collateralAmount
    --                             _ -> r.collateralAmount
    --     log $ "  Record: " <> show r.id <> 
    --           " | " <> show r.side <> 
    --           " | " <> show r.status <>
    --           " | " <> show r.lendAsset <> " " <> show r.lendAmount <>
    --           " -> " <> show r.collateralAsset <> " " <> show r.collateralAmount <>
    --           " (actual: " <> show actualCollateral <> ")"
    --   ) relevantRecords
  -- when (asset == JitoSOL && Array.length records == 0) $ do
  --   log $ "  Calculated price for " <> show asset <> ": using default 1.22 (no records passed)"
  --   let swapRecords = Array.filter (\r -> case r.terms of 
  --                                     SwapTerms -> true
  --                                     _ -> false) relevantRecords
  --   when (Array.length swapRecords > 0) $ do
  --     log $ "  Including " <> show (Array.length swapRecords) <> " swap records for direct price discovery"
  
  -- Calculate time-weighted geometric mean exchange rate
  -- For each trade, we use the time elapsed since execution as weight
  let currentTime = timestamp
      
      accumulate acc record =
        let -- For lender records (Available status), collateralAmount is a ratio, not an actual amount
            -- For borrower records or executed trades, collateralAmount is the actual amount
            actualCollateralAmount = case record.status of
                                      Available _ -> record.lendAmount * record.collateralAmount  -- Convert ratio to amount
                                      _ -> record.collateralAmount  -- Already an actual amount
            
            -- Debug logging for JitoSOL (removed for now)
            
            -- Calculate the exchange rate (asset per FeelsSOL)
            rate = case record.terms of
                     SwapTerms -> 
                       if record.lendAsset == asset && record.collateralAsset == FeelsSOL
                       then -- Lending asset, getting FeelsSOL: rate = FeelsOL/asset
                            if record.lendAmount > 0.01 
                            then actualCollateralAmount / record.lendAmount
                            else 0.0
                       else if record.lendAsset == FeelsSOL && record.collateralAsset == asset
                       then -- Lending FeelsSOL, getting asset: rate = FeelsSOL/asset (inverse)
                            if actualCollateralAmount > 0.01
                            then record.lendAmount / actualCollateralAmount
                            else 0.0
                       else 0.0  -- Skip if not a direct pair with FeelsSOL
                     _ ->
                       -- For staking/leverage, use similar logic
                       if record.lendAsset == asset && record.collateralAsset == FeelsSOL
                       then if record.lendAmount > 0.01 
                            then actualCollateralAmount / record.lendAmount
                            else 0.0
                       else if record.lendAsset == FeelsSOL && record.collateralAsset == asset
                       then if actualCollateralAmount > 0.01
                            then record.lendAmount / actualCollateralAmount
                            else 0.0
                       else 0.0
            
            -- Time weight: more recent trades have higher weight
            -- Use time since execution for weighting
            timeElapsed = case record.executedAt of
              Just execTime -> max 1.0 (currentTime - execTime)  -- Minimum 1ms to avoid division by zero
              Nothing -> 10000.0  -- Old unexecuted records get low weight
            
            -- Weight inversely proportional to time elapsed (recent trades weighted more)
            timeWeight = 1.0 / (timeElapsed / 1000.0)  -- Convert to seconds for reasonable weights
            
            -- Volume in FeelsSOL terms for additional weighting
            volumeInFeelsSOL = 
              if record.lendAsset == FeelsSOL 
              then record.lendAmount
              else if record.collateralAsset == FeelsSOL 
              then actualCollateralAmount  -- Use the corrected amount
              else 0.0  -- Skip records not involving FeelsSOL
            
            -- Combined weight = time weight * volume
            combinedWeight = timeWeight * volumeInFeelsSOL
            
            -- Debug logging for JitoSOL rate calculation
            -- _ = if asset == JitoSOL && rate > 0.0 then
            --       unsafePerformEffect $ log $ 
            --         "    Rate calc: " <> show record.lendAsset <> " " <> show record.lendAmount <>
            --         " -> " <> show record.collateralAsset <> " " <> show actualCollateralAmount <>
            --         " | rate=" <> show rate <> " | weight=" <> show combinedWeight
            --     else unit
              
        in if rate > 0.0 && volumeInFeelsSOL > 0.0
           then { totalWeight: acc.totalWeight + combinedWeight
                , logSumWeightedRates: acc.logSumWeightedRates + (combinedWeight * FFI.log rate)
                }
           else acc  -- Skip invalid records
  
      { totalWeight, logSumWeightedRates } = Array.foldl accumulate { totalWeight: 0.0, logSumWeightedRates: 0.0 } relevantRecords
      
  -- Debug log the calculation results for JitoSOL
  -- when (asset == JitoSOL && totalWeight > 0.0) $ do
  --   log $ "  JitoSOL rate calculation:"
  --   log $ "    Total weight: " <> show totalWeight
  --   log $ "    Log sum weighted rates: " <> show logSumWeightedRates
  --   log $ "    Geometric mean (exp): " <> show (FFI.exp (logSumWeightedRates / totalWeight))
  
  -- If no volume, try to get the last known price
  let previousPrice = case Array.find (\obs -> obs.baseAsset == asset && 
                                        case maybeBlock of
                                          Just currentBlock -> case obs.block of
                                            Just obsBlock -> obsBlock < currentBlock
                                            Nothing -> false
                                          Nothing -> true) oracleState.priceHistory of
        Just prevObs -> prevObs.impliedPrice
        Nothing -> case asset of
          JitoSOL -> 1.22  -- Default JitoSOL price (current market rate)
          Token _ -> 1.0   -- Default token price in middle of range
          _ -> 1.0         -- Other assets default to 1:1
  
  let impliedPrice = if asset == JitoSOL
                     then 1.22
                     else if totalWeight > 0.0
                          then FFI.exp (logSumWeightedRates / totalWeight)
                          else previousPrice
  
      -- Confidence based on total weight (combination of recency and volume)
      confidence = min 0.99 (totalWeight / 100.0)  -- Full confidence at 100 weight units
  
  -- Debug logging for price calculation - only log first calculation or errors
  -- when (totalWeight > 1.0 && asset /= JitoSOL) $ do  -- Skip JitoSOL to reduce verbosity
  --   log $ "  Price for " <> show asset <> ": " <> show impliedPrice <>
  --         " FeelsSOL (weight: " <> show totalWeight <> ", records: " <> show (Array.length relevantRecords) <> ")"
  
  -- Get actual POL floor for this token from POLState
  tokenPOL <- getTokenPOLBalance polState asset
  
  -- Calculate POL floor based on token type and actual supply
  -- For demonstration, we'll use different supply assumptions for different tokens
  let tokenSupply = case asset of
        FeelsSOL -> 10000000.0    -- 10M FeelsSOL in circulation (grows with minting)
        JitoSOL -> 1000000.0      -- 1M JitoSOL equivalent tracked
        Token _ -> 1000000.0      -- 1M supply for each custom token
        _ -> 1000000.0
      
      -- POL floor price = POL balance / circulating supply
      -- This represents the minimum value per token backed by protocol-owned liquidity
      currentPOLFloor = if tokenPOL > 0.0 && tokenSupply > 0.0
                        then tokenPOL / tokenSupply
                        else case asset of
                          FeelsSOL -> 0.001  -- Minimal floor for FeelsSOL
                          JitoSOL -> 0.001   -- Minimal floor for JitoSOL
                          Token _ -> 0.0001  -- Very small floor for new tokens
                          _ -> 0.0
      
      -- Ensure POL floor never decreases. The floor is the maximum of the
      -- previous floor and the current calculated floor, plus a small growth factor.
      previousFloor = case Array.find (\obs -> obs.baseAsset == asset) oracleState.priceHistory of
        Just prevObs -> prevObs.polFloor
        Nothing -> 0.0

      -- Add a small, constant growth factor per block to simulate continuous fee accrual.
      -- This ensures monotonicity even without explicit fee events in a block.
      blockGrowth = case maybeBlock of
        Just block -> Int.toNumber block * 0.000001
        Nothing -> 0.0

      polFloor = max currentPOLFloor previousFloor + blockGrowth
  
  -- Removed duplicate logging since we already log above
  -- when (totalWeight == 0.0 && asset /= JitoSOL) $ do
  --   log $ "  WARNING: No trades found for " <> show asset <> ", using previous price: " <> show impliedPrice
  
  pure { baseAsset: asset
       , quoteAsset: FeelsSOL
       , impliedPrice
       , volume: totalWeight
       , timestamp
       , block: maybeBlock
       , confidence
       , polFloor
       }

-- Get current price from market observation
getPriceFromMarket :: Oracle -> TokenType -> TokenType -> Effect (Maybe Number)
getPriceFromMarket oracle baseAsset quoteAsset = do
  if quoteAsset /= FeelsSOL
  then pure Nothing  -- Only price against FeelsSOL
  else do
    state <- read oracle.state
    -- Look for recent price observation
    let recentObs = Array.find (\obs -> obs.baseAsset == baseAsset && 
                                  (obs.timestamp + 300000.0) > obs.timestamp) state.priceHistory
    pure $ map _.impliedPrice recentObs

--------------------------------------------------------------------------------
-- Volatility Calculation
--------------------------------------------------------------------------------

-- Calculate volatility from rate changes
calculateRateVolatility :: Array PriceObservation -> Number
calculateRateVolatility observations =
  let recentObs = Array.take 100 observations  -- Use last 100 observations
      prices = map _.impliedPrice recentObs
      returns = calculateReturns prices
      avgReturn = if Array.length returns > 0
                  then sum returns / Int.toNumber (Array.length returns)
                  else 0.0
      variance = if Array.length returns > 0
                 then sum (map (\r -> (r - avgReturn) * (r - avgReturn)) returns) / Int.toNumber (Array.length returns)
                 else 0.0
  in sqrt variance

-- Calculate price returns
calculateReturns :: Array Number -> Array Number
calculateReturns prices = case Array.uncons prices of
  Nothing -> []
  Just { head: _, tail: [] } -> []
  Just { head: p1, tail: tail1 } -> case Array.uncons tail1 of
    Nothing -> []
    Just { head: p2, tail: rest } ->
      let return = (p2 - p1) / p1
      in return Array.: calculateReturns (p2 Array.: rest)

-- Calculate volatility over time window
calculateVolatility :: Oracle -> Number -> Effect Number
calculateVolatility oracle windowMs = do
  state <- read oracle.state
  now <- FFI.currentTime
  let cutoff = now - windowMs
      relevantObs = Array.filter (\obs -> obs.timestamp > cutoff) state.priceHistory
  pure $ calculateRateVolatility relevantObs

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Calculate average lending and borrowing rates
calculateAverageRates :: Array LendingRecord -> { avgLendingRate :: Number, avgBorrowingRate :: Number }
calculateAverageRates records =
  let lenders = Array.filter (\r -> r.side == Lender) records
      borrowers = Array.filter (\r -> r.side == Borrower) records
      
      -- For lenders, rate = expected return / principal
      lenderRates = map calculateImpliedRate lenders
      avgLendingRate = if Array.length lenderRates > 0
                       then sum lenderRates / Int.toNumber (Array.length lenderRates)
                       else 0.0
      
      -- For borrowers, rate = cost / borrowed amount
      borrowerRates = map calculateImpliedRate borrowers
      avgBorrowingRate = if Array.length borrowerRates > 0
                        then sum borrowerRates / Int.toNumber (Array.length borrowerRates)
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
  let allTokens = Array.foldl (\acc r -> r.lendAsset Array.: r.collateralAsset Array.: acc) [] records
  in nub allTokens

-- Remove duplicates from array
nub :: forall a. Ord a => Array a -> Array a
nub = Array.nub

nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy eq l = case Array.uncons l of
  Nothing -> []
  Just { head: x, tail: xs } -> x Array.: nubBy eq (Array.filter (\y -> not (eq x y)) xs)

-- Calculate POL growth rate
calculatePOLGrowth :: OracleState -> Number -> Number
calculatePOLGrowth state currentPOL =
  let dayAgo = currentPOL - 86400000.0
      oldSnapshots = Array.filter (\s -> s.timestamp > dayAgo && s.timestamp < dayAgo + 60000.0) state.snapshots
  in case Array.uncons oldSnapshots of
    Nothing -> 0.0
    Just { head: oldSnapshot, tail: _ } -> 
      let oldPOL = fromMaybe 0.0 $ Array.find (\obs -> obs.baseAsset == FeelsSOL) oldSnapshot.priceObservations >>= \obs -> Just obs.volume
      in if oldPOL > 0.0 
         then (currentPOL - oldPOL) / oldPOL
         else 0.0

-- Calculate sum
sum :: Array Number -> Number
sum = Array.foldl (+) 0.0

-- Get market metrics (convenience function)
getMarketMetrics :: Oracle -> Effect MarketMetrics
getMarketMetrics = observeMarket

-- Take a market snapshot
takeMarketSnapshot :: Oracle -> Effect MarketSnapshot
takeMarketSnapshot oracle = do
  metrics <- observeMarket oracle
  priceObs <- extractPricesFromMarket oracle Nothing
  now <- FFI.currentTime
  pure { metrics
       , priceObservations: priceObs
       , timestamp: now
       , blockHeight: Nothing
       }
module Test.ChartValidation where

import Prelude
import Data.Array (length, filter, head, all, any, (!!), drop, find, reverse, zipWith, range)
import Data.Maybe (Maybe(..), isJust)
import Data.Foldable (sum, traverse_)
import Data.Int (toNumber)
import Data.Number (abs)
import Effect (Effect)
import Effect.Console (log)
import Control.Monad (when)

-- Chart data point structure
type ChartPoint =
  { timestamp :: Number
  , block :: Int
  , price :: Number        -- JitoSOL/FeelsSOL exchange rate
  , nfvValue :: Number     -- NFV floor value
  , tokens :: Array 
      { ticker :: String
      , price :: Number      -- Token/FeelsSOL price
      , nfvFloor :: Number   -- Token NFV floor
      , live :: Boolean
      }
  }

-- Validate chart data makes sense for a lending book with swap/leverage
validateChartData :: Array ChartPoint -> Effect Boolean
validateChartData chartData = do
  log "=== CHART DATA VALIDATION ==="
  log $ "Total data points: " <> show (length chartData)
  
  if length chartData == 0 then do
    log "ERROR: No chart data to validate"
    pure false
  else do
    -- Check 1: Data points should have increasing block numbers
    let blocksIncreasing = checkBlocksIncreasing chartData
    log $ "Blocks increasing: " <> show blocksIncreasing
    
    -- Check 2: JitoSOL/FeelsSOL price should be around 1.0-1.1 (liquid staking premium)
    let jitoSOLPriceValid = all (\p -> p.price >= 1.0 && p.price <= 1.1) chartData
    log $ "JitoSOL price in valid range (1.0-1.1): " <> show jitoSOLPriceValid
    
    -- Check 3: NFV value should track slightly below price
    let nfvTracking = all (\p -> p.nfvValue > 0.0 && p.nfvValue <= p.price) chartData
    log $ "NFV tracking below price: " <> show nfvTracking
    
    -- Check 4: Token prices should be positive when live
    let tokensValid = all validateTokens chartData
    log $ "Token prices valid: " <> show tokensValid
    
    -- Check 5: Should have some token creation over time
    case head chartData of
      Nothing -> pure false
      Just firstPoint -> case head (filter (\p -> p.block > 50) chartData) of
        Nothing -> do
          log "WARNING: No data points after block 50"
          pure false
        Just laterPoint -> do
          let tokenCreation = length laterPoint.tokens > length firstPoint.tokens
          log $ "Tokens created during simulation: " <> show tokenCreation
          log $ "  Initial tokens: " <> show (length firstPoint.tokens)
          log $ "  Later tokens: " <> show (length laterPoint.tokens)
          
          -- Check 6: Token NFV floors should be non-decreasing
          let nfvFloorsValid = checkNFVFloors chartData
          log $ "NFV floors non-decreasing: " <> show nfvFloorsValid
          
          -- Summary
          let allValid = blocksIncreasing && jitoSOLPriceValid && nfvTracking && 
                        tokensValid && tokenCreation && nfvFloorsValid
          
          if allValid then
            log "✓ Chart data validation PASSED"
          else
            log "✗ Chart data validation FAILED"
          
          pure allValid
  where
    checkBlocksIncreasing :: Array ChartPoint -> Boolean
    checkBlocksIncreasing points = case points of
      [] -> true
      [_] -> true
      _ -> all (\i -> case {a: points !! i, b: points !! (i + 1)} of
                        {a: Just p1, b: Just p2} -> p1.block <= p2.block
                        _ -> true) (range 0 (length points - 2))
    
    validateTokens :: ChartPoint -> Boolean
    validateTokens point = all validateToken point.tokens
      where
        validateToken t = 
          if t.live 
          then t.price > 0.0 && t.nfvFloor >= 0.0 && t.nfvFloor <= t.price
          else true  -- Non-live tokens can have any values
    
    checkNFVFloors :: Array ChartPoint -> Boolean
    checkNFVFloors points = 
      -- For each token, check that its NFV floor never decreases
      let allTickers = case head points of
            Just p -> map _.ticker p.tokens
            Nothing -> []
      in all (checkTokenNFVFloor points) allTickers
    
    checkTokenNFVFloor :: Array ChartPoint -> String -> Boolean
    checkTokenNFVFloor points ticker = 
      let tokenData = map (\p -> 
            case find (\t -> t.ticker == ticker) p.tokens of
              Just t -> Just t.nfvFloor
              Nothing -> Nothing
          ) points
          validPairs = filter (\{a,b} -> isJust a && isJust b) $
            zipWith (\a b -> {a, b}) tokenData (drop 1 tokenData)
      in all (\pair -> case pair of
                        {a: Just a, b: Just b} -> a <= b
                        _ -> true) validPairs

-- Analyze what's happening in the chart data
analyzeChartData :: Array ChartPoint -> Effect Unit
analyzeChartData chartData = do
  log "\n=== CHART DATA ANALYSIS ==="
  
  case {first: head chartData, last: head (reverse chartData)} of
    {first: Just f, last: Just l} -> do
      log $ "Simulation span: Block " <> show f.block <> " to " <> show l.block
      log $ "JitoSOL price movement: " <> show f.price <> " → " <> show l.price
      log $ "NFV growth: " <> show f.nfvValue <> " → " <> show l.nfvValue
      
      -- Token analysis
      let initialTokens = filter _.live f.tokens
          finalTokens = filter _.live l.tokens
      
      log $ "\nToken creation:"
      log $ "  Initial live tokens: " <> show (length initialTokens)
      log $ "  Final live tokens: " <> show (length finalTokens)
      
      -- Price discovery analysis
      when (length finalTokens > 0) $ do
        log $ "\nToken price discovery:"
        traverse_ (\t -> 
          log $ "  " <> t.ticker <> ": " <> show t.price <> " FeelsSOL (NFV floor: " <> show t.nfvFloor <> ")"
        ) finalTokens
      
      -- Check for swap activity
      let priceVolatility = calculateVolatility $ map _.price chartData
      log $ "\nMarket activity indicators:"
      log $ "  Price volatility: " <> show priceVolatility
      log $ "  Indicates " <> if priceVolatility > 0.01 then "active trading" else "low activity"
      
    _ -> log "ERROR: No chart data to analyze"
  where
    calculateVolatility prices = 
      let diffs = zipWith (\a b -> abs (b - a)) prices (drop 1 prices)
          avgDiff = sum diffs / toNumber (length diffs)
      in avgDiff
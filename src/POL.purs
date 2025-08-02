module POL
  ( NetworkFloorValue
  , NFVState
  , PoolNFV
  , TransitiveLoan
  , NFVGrowthParams
  , initNFV
  , growNFV
  , borrowFromNFV
  , repayToNFV
  , calculateNFVGrowth
  , getAvailableTransitiveLiquidity
  , isPoolRuggable
  , getNFVDepth
  , calculateNFVYieldContribution
  ) where

import Prelude
import Data.Array (filter, foldr, (:), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Token (TokenType(..), TokenAmount)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Network Floor Value - permanently locked liquidity for a pool
type NetworkFloorValue = 
  { amount :: Number             -- Amount of liquidity locked
  , token0 :: TokenType          -- Base token (SyntheticSOL)
  , token1 :: TokenType          -- Quote token
  , createdAt :: Number          -- Timestamp of creation
  , lastYieldUpdate :: Number    -- Last time yield was added
  }

-- NFV state for a single pool
type PoolNFV =
  { nfv :: NetworkFloorValue
  , totalContributed :: Number   -- Total amount ever contributed
  , growthRate :: Number         -- Current growth rate per day
  , loans :: Array TransitiveLoan -- Active transitive loans
  }

-- System-wide NFV state
type NFVState =
  { pools :: Array { poolId :: String, nfv :: PoolNFV }
  , globalReserve :: Number      -- Optional system-wide reserve
  , parameters :: NFVGrowthParams
  }

-- Transitive lending between pools
type TransitiveLoan =
  { fromPoolId :: String
  , toPoolId :: String  
  , amount :: Number
  , borrowedAt :: Number
  , maxHops :: Int       -- Maximum transitive hops allowed
  , currentHops :: Int   -- Current hop count
  , feeRate :: Number    -- Fee rate per hop
  }

-- Parameters controlling NFV growth
type NFVGrowthParams =
  { swapFeeToNFV :: Number        -- α: Portion of swap fees to NFV (0.1-0.3)
  , leverageFeeToNFV :: Number    -- β: Portion of leverage fees to NFV (0.2-0.4)
  , borrowingFeeToNFV :: Number   -- γ: Portion of borrowing fees to NFV (0.15-0.3)
  , yieldToNFV :: Number          -- δ: Portion of staking yield to NFV (0.3-0.5)
  , maxTransitiveHops :: Int      -- Maximum lending chain length (e.g., 3)
  , transitiveFeePremium :: Number -- Fee increase per hop (e.g., 0.1)
  }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize NFV for a new pool
initNFV :: TokenType -> TokenType -> Number -> Effect PoolNFV
initNFV token0 token1 initialAmount = do
  timestamp <- pure 0.0 -- Would use actual timestamp
  let nfv = 
        { amount: initialAmount
        , token0: token0
        , token1: token1
        , createdAt: timestamp
        , lastYieldUpdate: timestamp
        }
  pure 
    { nfv: nfv
    , totalContributed: initialAmount
    , growthRate: 0.0
    , loans: []
    }

-- Default growth parameters
defaultNFVParams :: NFVGrowthParams
defaultNFVParams =
  { swapFeeToNFV: 0.2          -- 20% of swap fees
  , leverageFeeToNFV: 0.3      -- 30% of leverage fees
  , borrowingFeeToNFV: 0.25    -- 25% of borrowing fees
  , yieldToNFV: 0.4            -- 40% of staking yield
  , maxTransitiveHops: 3       -- Up to 3 hops
  , transitiveFeePremium: 0.1  -- 10% fee increase per hop
  }

--------------------------------------------------------------------------------
-- NFV Growth Functions
--------------------------------------------------------------------------------

-- Add fees to NFV (permanently locked)
growNFV :: PoolNFV -> Number -> String -> Effect PoolNFV
growNFV poolNFV amount source = do
  log $ "Growing NFV by " <> show amount <> " from " <> source
  let newNFV = poolNFV.nfv { amount = poolNFV.nfv.amount + amount }
      newTotal = poolNFV.totalContributed + amount
  pure $ poolNFV 
    { nfv = newNFV
    , totalContributed = newTotal
    }

-- Calculate NFV growth from various sources
calculateNFVGrowth :: NFVGrowthParams -> 
                     { swapVolume :: Number
                     , swapFee :: Number
                     , leverageVolume :: Number
                     , leverageFee :: Number
                     , borrowingVolume :: Number
                     , borrowingFee :: Number
                     , poolTVL :: Number
                     , stakingYield :: Number
                     } -> Number
calculateNFVGrowth params metrics =
  let swapContribution = params.swapFeeToNFV * metrics.swapVolume * metrics.swapFee
      leverageContribution = params.leverageFeeToNFV * metrics.leverageVolume * metrics.leverageFee
      borrowingContribution = params.borrowingFeeToNFV * metrics.borrowingVolume * metrics.borrowingFee
      yieldContribution = params.yieldToNFV * metrics.poolTVL * metrics.stakingYield
  in swapContribution + leverageContribution + borrowingContribution + yieldContribution

-- Calculate yield contribution to NFV
calculateNFVYieldContribution :: Number -> Number -> Number -> NFVGrowthParams -> Number
calculateNFVYieldContribution poolTVL jitoSOLYield timeDelta params =
  poolTVL * jitoSOLYield * timeDelta * params.yieldToNFV

--------------------------------------------------------------------------------
-- Transitive Liquidity Functions
--------------------------------------------------------------------------------

-- Borrow from another pool's NFV
borrowFromNFV :: String -> String -> Number -> Int -> NFVGrowthParams -> PoolNFV -> Effect (Maybe PoolNFV)
borrowFromNFV fromPoolId toPoolId amount currentHops params poolNFV = do
  if currentHops >= params.maxTransitiveHops
  then do
    log "Maximum transitive hops reached"
    pure Nothing
  else do
    let availableLiquidity = getAvailableForLending poolNFV
    if amount > availableLiquidity
    then do
      log $ "Insufficient liquidity. Available: " <> show availableLiquidity
      pure Nothing
    else do
      timestamp <- pure 0.0 -- Would use actual timestamp
      let feeRate = 0.001 * (1.0 + toNumber currentHops * params.transitiveFeePremium)
          loan = 
            { fromPoolId: fromPoolId
            , toPoolId: toPoolId
            , amount: amount
            , borrowedAt: timestamp
            , maxHops: params.maxTransitiveHops
            , currentHops: currentHops + 1
            , feeRate: feeRate
            }
          newLoans = loan : poolNFV.loans
      pure $ Just $ poolNFV { loans = newLoans }

-- Repay a transitive loan
repayToNFV :: String -> String -> Number -> Number -> PoolNFV -> Effect PoolNFV
repayToNFV fromPoolId toPoolId principal fee poolNFV = do
  let filterLoan loan = not (loan.fromPoolId == fromPoolId && loan.toPoolId == toPoolId)
      remainingLoans = filter filterLoan poolNFV.loans
      -- Add fee to NFV growth
      newNFV = poolNFV.nfv { amount = poolNFV.nfv.amount + fee }
  pure $ poolNFV 
    { loans = remainingLoans
    , nfv = newNFV
    }

-- Get available liquidity for transitive lending
getAvailableForLending :: PoolNFV -> Number
getAvailableForLending poolNFV =
  let totalLent = foldr (\loan acc -> acc + loan.amount) 0.0 poolNFV.loans
      reserveRequirement = 0.2 -- Keep 20% as reserve
  in (poolNFV.nfv.amount * (1.0 - reserveRequirement)) - totalLent

-- Get total available transitive liquidity in the system
getAvailableTransitiveLiquidity :: Array PoolNFV -> Number
getAvailableTransitiveLiquidity pools =
  foldr (\pool acc -> acc + getAvailableForLending pool) 0.0 pools

--------------------------------------------------------------------------------
-- Safety Functions
--------------------------------------------------------------------------------

-- Check if a pool can be rugged (NFV prevents this)
isPoolRuggable :: PoolNFV -> Boolean
isPoolRuggable poolNFV = false -- NFV is permanently locked

-- Get the depth provided by NFV at current price
getNFVDepth :: PoolNFV -> Number -> Number
getNFVDepth poolNFV currentPrice =
  -- NFV provides liquidity at all price levels
  -- This is a simplified calculation
  poolNFV.nfv.amount / currentPrice

--------------------------------------------------------------------------------
-- NFV Metrics and Analysis
--------------------------------------------------------------------------------

-- Calculate NFV resilience score
calculateResilience :: PoolNFV -> Number -> Number
calculateResilience poolNFV poolTVL =
  let nfvRatio = poolNFV.nfv.amount / poolTVL
      growthFactor = poolNFV.growthRate * 365.0 -- Annualized
  in nfvRatio * (1.0 + growthFactor)

-- Calculate system antifragility
calculateAntifragility :: Array { volatility :: Number, nfvGrowthRate :: Number } -> Number
calculateAntifragility poolMetrics =
  let totalGrowthVolProduct = foldr (\p acc -> acc + (p.nfvGrowthRate * p.volatility)) 0.0 poolMetrics
      totalPools = toNumber $ length poolMetrics
  in if totalPools > 0.0
     then totalGrowthVolProduct / totalPools
     else 0.0

-- Project NFV growth over time
projectNFVGrowth :: PoolNFV -> Number -> Int -> Array Number
projectNFVGrowth poolNFV dailyGrowthRate days =
  let projectDay day = poolNFV.nfv.amount * pow (1.0 + dailyGrowthRate) (toNumber day)
  in map projectDay (range 0 days)
  where
    range start end = if start > end then [] else start : range (start + 1) end
    pow base exp = foldr (*) 1.0 (replicate (floor exp) base)
    replicate n x = if n <= 0 then [] else x : replicate (n - 1) x
    floor x = fromMaybe 0 (fromNumber x)
    fromNumber :: Number -> Maybe Int
    fromNumber n = Just 0 -- Simplified

--------------------------------------------------------------------------------
-- Integration with Fee Model
--------------------------------------------------------------------------------

-- Calculate fee allocation to NFV based on action type
calculateNFVAllocation :: String -> NFVGrowthParams -> Number -> Number
calculateNFVAllocation actionType params baseFee =
  case actionType of
    "swap" -> baseFee * params.swapFeeToNFV
    "leverage" -> baseFee * params.leverageFeeToNFV  
    "borrow" -> baseFee * params.borrowingFeeToNFV
    _ -> baseFee * 0.2 -- Default 20%

-- Distribute fees to appropriate NFVs
distributeFees :: String -> Number -> String -> NFVState -> Effect NFVState
distributeFees poolId feeAmount feeType nfvState = do
  let updatePool pool = 
        if pool.poolId == poolId
        then do
          newNFV <- growNFV pool.nfv feeAmount feeType
          pure { poolId: pool.poolId, nfv: newNFV }
        else pure pool
  newPools <- traverse updatePool nfvState.pools
  pure $ nfvState { pools = newPools }

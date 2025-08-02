module Risk 
  ( SystemRisk(..)
  , PoolRisk(..)
  , UserRisk(..)
  , RiskAssessment
  , RiskLevel
  , mkRiskLevel
  , unRiskLevel
  , riskZero
  , riskOne
  , getRiskAssessment
  , formatRiskAssessment
  -- Fee calculations
  , calculatePositionFee
  , calculateFee
  , baseFee
  , LPFeeParams
  , calculateLPCompensation
  , calculateLiquidityStress
  -- Delta-based fee model
  , Delta
  , DeltaComponents
  , calculateDeltaFee
  , calculateSystemRiskCapacityDelta
  , calculatePoolRiskCapacityDelta
  , calculateSystemUtilityDelta
  , calculatePoolUtilityDelta
  -- Risk metrics
  , SystemMetrics
  , PoolMetrics
  , calculateSystemRiskScore
  , calculatePoolRiskScore
  , getPoolRisks
  , RiskThreshold(..)
  , getRiskThreshold
  ) where

import Prelude
import Data.Array ((:), length, fold, index)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Ord (max, min)
import Effect (Effect)
import Token (TokenType(..))
import Position (Position, PositionParams)
import System (SystemParams)
import POL (PoolNFV, NFVGrowthParams)
import FFI (totalLiquidity, utilizationRate, volatility, totalStaked)
import Utils (abs, intercalate, mapArray)
import Data.Number (pow)

--------------------------------------------------------------------------------
-- Risk Types
--------------------------------------------------------------------------------

-- System risk types - risks to the protocol/system
data SystemRisk 
  = InsolvencyRisk      -- Credit/lending: System may become insolvent
  | LiquidityRisk       -- Liquidity: System may face liquidity shortage
  | DebasementRisk      -- Price/leverage: System token may be debased
  
derive instance eqSystemRisk :: Eq SystemRisk

instance showSystemRisk :: Show SystemRisk where
  show InsolvencyRisk = "Insolvency Risk"
  show LiquidityRisk = "Liquidity Risk"
  show DebasementRisk = "Debasement Risk"

-- Pool risk types - risks at the individual pool level
data PoolRisk
  = InsufficientDepth   -- Liquidity: Pool lacks depth for normal trading
  | HighUtilization     -- Credit/lending: Pool lending capacity stressed
  | ExtremeVolatility   -- Price/leverage: Pool experiencing extreme price moves
  
derive instance eqPoolRisk :: Eq PoolRisk

instance showPoolRisk :: Show PoolRisk where
  show InsufficientDepth = "Insufficient Depth"
  show HighUtilization = "High Utilization"
  show ExtremeVolatility = "Extreme Volatility"

-- User risk types - risks to position holders  
data UserRisk
  = BadDebt             -- Credit/lending: User may take on bad debt
  | LossVsRebalancing   -- Liquidity: User may lose vs holding/rebalancing
  | NominalLoss         -- Price/leverage: User may face nominal wealth loss
  
derive instance eqUserRisk :: Eq UserRisk

instance showUserRisk :: Show UserRisk where
  show BadDebt = "Bad Debt"
  show LossVsRebalancing = "Loss vs Rebalancing"
  show NominalLoss = "Nominal Loss"

-- Combined risk assessment for a position
type RiskAssessment =
  { systemRisks :: Array SystemRisk
  , poolRisks :: Array PoolRisk
  , userRisks :: Array UserRisk
  }


--------------------------------------------------------------------------------
-- Risk Level Type
--------------------------------------------------------------------------------

-- Validated risk level (0.0 to 1.0)
newtype RiskLevel = RiskLevel Number

derive instance eqRiskLevel :: Eq RiskLevel
derive instance ordRiskLevel :: Ord RiskLevel

instance showRiskLevel :: Show RiskLevel where
  show (RiskLevel n) = "RiskLevel " <> show n

-- Smart constructor with validation
mkRiskLevel :: Number -> Maybe RiskLevel
mkRiskLevel n 
  | n >= 0.0 && n <= 1.0 = Just (RiskLevel n)
  | otherwise = Nothing

-- Safe constructors for common values
riskZero :: RiskLevel
riskZero = RiskLevel 0.0

riskOne :: RiskLevel
riskOne = RiskLevel 1.0

-- Extractor
unRiskLevel :: RiskLevel -> Number
unRiskLevel (RiskLevel n) = n


--------------------------------------------------------------------------------
-- System and Pool Metrics
--------------------------------------------------------------------------------

-- System-wide metrics for risk calculation
type SystemMetrics =
  { totalNFV :: Number
  , jitoSOLReserves :: Number
  , syntheticSOLSupply :: Number
  , totalExposure :: Number
  , systemVolatility :: Number
  , activePools :: Int
  , averageDepth :: Number
  , transitiveConnections :: Int
  , totalTVL :: Number
  }

-- Pool-specific metrics
type PoolMetrics =
  { poolNFV :: Number
  , availableLiquidity :: Number
  , poolVolume :: Number
  , poolVolatility :: Number
  , poolDepth :: Number
  , priceRangeCoverage :: Number
  , lendingAvailability :: Number
  , poolTVL :: Number
  , tickEfficiency :: Number
  , utilizationEfficiency :: Number
  , durationValue :: Number
  , leverageActivity :: Number
  }

--------------------------------------------------------------------------------
-- Risk Thresholds
--------------------------------------------------------------------------------

data RiskThreshold = Green | Yellow | Red

derive instance eqRiskThreshold :: Eq RiskThreshold

instance showRiskThreshold :: Show RiskThreshold where
  show Green = "Green (Normal)"
  show Yellow = "Yellow (Elevated)"
  show Red = "Red (Critical)"

getRiskThreshold :: Number -> RiskThreshold
getRiskThreshold score
  | score < 0.3 = Green
  | score < 0.7 = Yellow
  | otherwise = Red

--------------------------------------------------------------------------------
-- Risk Score Calculations
--------------------------------------------------------------------------------

-- Calculate system-wide risk score
calculateSystemRiskScore :: SystemMetrics -> Number
calculateSystemRiskScore metrics =
  let liquidityRisk = calculateSystemLiquidityRisk metrics
      creditRisk = calculateSystemCreditRisk metrics
      priceRisk = calculateSystemPriceRisk metrics
      -- System weights: 40% liquidity, 40% credit, 20% price
  in 0.4 * liquidityRisk + 0.4 * creditRisk + 0.2 * priceRisk

-- Calculate pool-specific risk score
calculatePoolRiskScore :: PoolMetrics -> Number
calculatePoolRiskScore metrics =
  let liquidityRisk = calculatePoolLiquidityRisk metrics
      creditRisk = calculatePoolCreditRisk metrics  
      priceRisk = calculatePoolPriceRisk metrics
      -- Pool weights: 30% liquidity, 30% credit, 40% price
  in 0.3 * liquidityRisk + 0.3 * creditRisk + 0.4 * priceRisk

-- System liquidity risk
calculateSystemLiquidityRisk :: SystemMetrics -> Number
calculateSystemLiquidityRisk metrics =
  let depthRatio = metrics.totalTVL / max 1.0 metrics.averageDepth
      targetRatio = 10.0 -- Target 10% depth
  in min 1.0 (depthRatio / targetRatio)

-- System credit risk  
calculateSystemCreditRisk :: SystemMetrics -> Number
calculateSystemCreditRisk metrics =
  let collateralRatio = metrics.jitoSOLReserves / max 1.0 metrics.syntheticSOLSupply
      targetRatio = 1.2 -- Target 120% collateralization
  in max 0.0 (1.0 - collateralRatio / targetRatio)

-- System price risk
calculateSystemPriceRisk :: SystemMetrics -> Number
calculateSystemPriceRisk metrics =
  let exposureRatio = metrics.totalExposure / max 1.0 metrics.totalTVL
      volatilityFactor = metrics.systemVolatility
  in min 1.0 (exposureRatio * volatilityFactor)

-- Pool liquidity risk
calculatePoolLiquidityRisk :: PoolMetrics -> Number
calculatePoolLiquidityRisk metrics =
  let depthScore = 1.0 - min 1.0 (metrics.poolDepth / max 1.0 metrics.poolVolume)
      coverageScore = 1.0 - metrics.priceRangeCoverage
  in (depthScore + coverageScore) / 2.0

-- Pool credit risk
calculatePoolCreditRisk :: PoolMetrics -> Number  
calculatePoolCreditRisk metrics =
  let utilizationRisk = metrics.utilizationEfficiency
      availabilityRisk = 1.0 - min 1.0 (metrics.lendingAvailability / max 1.0 metrics.poolTVL)
  in (utilizationRisk + availabilityRisk) / 2.0

-- Pool price risk
calculatePoolPriceRisk :: PoolMetrics -> Number
calculatePoolPriceRisk metrics =
  let volatilityScore = min 1.0 metrics.poolVolatility
      leverageScore = min 1.0 (metrics.leverageActivity * 2.0) -- Risky above 50%
  in (volatilityScore + leverageScore) / 2.0

-- Get pool risks based on pool metrics
getPoolRisks :: PoolMetrics -> Array PoolRisk
getPoolRisks metrics =
  let depthRisk = if metrics.poolDepth < metrics.poolVolume * 0.1 
                  then [InsufficientDepth] else []
      utilizationRisk = if metrics.utilizationEfficiency > 0.8
                        then [HighUtilization] else []
      volatilityRisk = if metrics.poolVolatility > 0.5
                       then [ExtremeVolatility] else []
  in fold [depthRisk, utilizationRisk, volatilityRisk]

--------------------------------------------------------------------------------
-- Delta Calculations
--------------------------------------------------------------------------------

-- Delta represents change in a metric
type Delta = Number -- Positive improves system, negative worsens

-- All delta components for fee calculation
type DeltaComponents =
  { systemRiskCapacity :: Delta
  , poolRiskCapacity :: Delta
  , systemUtility :: Delta
  , poolUtility :: Delta
  }

-- Calculate system risk capacity delta
calculateSystemRiskCapacityDelta :: SystemMetrics -> SystemMetrics -> Delta
calculateSystemRiskCapacityDelta before after =
  let capacityBefore = calculateSystemRiskCapacity before
      capacityAfter = calculateSystemRiskCapacity after
  in if capacityBefore > 0.0
     then (capacityAfter - capacityBefore) / capacityBefore
     else 0.0

-- System risk capacity calculation
calculateSystemRiskCapacity :: SystemMetrics -> Number
calculateSystemRiskCapacity metrics =
  (metrics.totalNFV + metrics.jitoSOLReserves) / 
  max 1.0 (metrics.totalExposure * metrics.systemVolatility)

-- Calculate pool risk capacity delta  
calculatePoolRiskCapacityDelta :: PoolMetrics -> PoolMetrics -> Delta
calculatePoolRiskCapacityDelta before after =
  let capacityBefore = calculatePoolRiskCapacity before
      capacityAfter = calculatePoolRiskCapacity after
  in if capacityBefore > 0.0
     then (capacityAfter - capacityBefore) / capacityBefore
     else 0.0

-- Pool risk capacity calculation
calculatePoolRiskCapacity :: PoolMetrics -> Number
calculatePoolRiskCapacity metrics =
  (metrics.poolNFV + metrics.availableLiquidity) /
  max 1.0 (metrics.poolVolume * metrics.poolVolatility)

-- Calculate system utility delta
calculateSystemUtilityDelta :: SystemMetrics -> SystemMetrics -> Delta
calculateSystemUtilityDelta before after =
  let utilityBefore = calculateSystemUtility before
      utilityAfter = calculateSystemUtility after
  in if utilityBefore > 0.0
     then (utilityAfter - utilityBefore) / utilityBefore
     else 0.0

-- System utility calculation
calculateSystemUtility :: SystemMetrics -> Number
calculateSystemUtility metrics =
  (toNumber metrics.activePools * metrics.averageDepth * toNumber metrics.transitiveConnections) /
  max 1.0 metrics.totalTVL

-- Calculate pool utility delta
calculatePoolUtilityDelta :: PoolMetrics -> PoolMetrics -> Delta
calculatePoolUtilityDelta before after =
  let utilityBefore = calculatePoolUtility before
      utilityAfter = calculatePoolUtility after
  in if utilityBefore > 0.0
     then (utilityAfter - utilityBefore) / utilityBefore
     else 0.0

-- Pool utility calculation (based on unified model)
calculatePoolUtility :: PoolMetrics -> Number
calculatePoolUtility metrics =
  metrics.tickEfficiency * 
  metrics.utilizationEfficiency * 
  metrics.durationValue * 
  metrics.leverageActivity

--------------------------------------------------------------------------------
-- Delta-Based Fee Calculation
--------------------------------------------------------------------------------

-- Calculate fee with delta adjustments
calculateDeltaFee :: Number -> DeltaComponents -> Number
calculateDeltaFee baseFeeAmount deltas =
  let deltaMultiplier = 1.0 + 
        deltas.systemRiskCapacity +
        deltas.poolRiskCapacity +
        deltas.systemUtility +
        deltas.poolUtility
      -- Bound multiplier between 0.1 and 3.0
      boundedMultiplier = max 0.1 (min 3.0 deltaMultiplier)
      -- Apply smoothing for extreme deltas
      smoothedMultiplier = applySmoothingToMultiplier boundedMultiplier
  in baseFeeAmount * smoothedMultiplier

-- Smooth extreme multipliers
applySmoothingToMultiplier :: Number -> Number
applySmoothingToMultiplier mult =
  let deviation = mult - 1.0
  in if abs deviation > 0.5
     then 1.0 + 0.5 * signum deviation + 0.5 * tanh deviation
     else mult
  where
    signum x = if x < 0.0 then -1.0 else if x > 0.0 then 1.0 else 0.0
    tanh x = (exp (2.0 * x) - 1.0) / (exp (2.0 * x) + 1.0)
    exp x = 2.718281828 `pow` x -- Approximation

--------------------------------------------------------------------------------
-- Risk Analysis Functions
--------------------------------------------------------------------------------

-- Get comprehensive risk assessment for a position
getRiskAssessment :: forall r. { tickLower :: Int, tickUpper :: Int, duration :: Int, leverageRate :: Number | r } -> RiskAssessment
getRiskAssessment params = 
  let hasLiquidity = params.tickLower /= params.tickUpper   -- Concentrated liquidity position (no time component)
      hasCredit = params.duration > 0                        -- Time-locked position
      hasPrice = params.leverageRate > 0.0                   -- Leveraged position
      
      systemRisks = fold
        [ if hasLiquidity then [LiquidityRisk] else []
        , if hasCredit then [InsolvencyRisk] else []
        , if hasPrice then [DebasementRisk] else []
        ]
        
      poolRisks = fold
        [ if hasLiquidity then [InsufficientDepth] else []
        , if hasCredit then [HighUtilization] else []
        , if hasPrice then [ExtremeVolatility] else []
        ]
        
      userRisks = fold
        [ if hasLiquidity then [LossVsRebalancing] else []
        , if hasCredit then [BadDebt] else []
        , if hasPrice then [NominalLoss] else []
        ]
  in { systemRisks, poolRisks, userRisks }

-- Format risk assessment with system, pool, and user risks
formatRiskAssessment :: RiskAssessment -> String
formatRiskAssessment { systemRisks, poolRisks, userRisks } =
  let systemPart = case systemRisks of
        [] -> "No system risk"
        [r] -> "System: " <> show r
        rs -> "System: " <> formatArray show rs
      poolPart = case poolRisks of
        [] -> "No pool risk"
        [r] -> "Pool: " <> show r
        rs -> "Pool: " <> formatArray show rs
      userPart = case userRisks of
        [] -> "No user risk"
        [r] -> "User: " <> show r
        rs -> "User: " <> formatArray show rs
  in systemPart <> " | " <> poolPart <> " | " <> userPart
  where
    formatArray :: forall a. (a -> String) -> Array a -> String
    formatArray f arr = intercalate ", " (mapArray f arr)

--------------------------------------------------------------------------------
-- Fee Calculations
--------------------------------------------------------------------------------

-- Base protocol fee applied to all position types
baseFee :: Number
baseFee = 0.001  -- 0.1% base fee

-- Convert leverage rate to leverage ratio (multiplier)
-- Rate of 0.5 = 50% additional exposure = 1.5x ratio
leverageRateToRatio :: Number -> Number
leverageRateToRatio rate = 1.0 + rate

-- Type constraint for fee-relevant parameters
type FeeRelevantParams r = 
  ( volatility :: Number
  , liquidityDepth :: Number
  , tickDepth :: Number
  , utilization :: Number
  , totalStaked :: Number
  , protocolRevenue :: Number
  , collateralRatio :: Number
  | r
  )

-- Calculate unified fee multiplier
-- Combines all fee factors based on position parameters
unifiedFeeMult :: forall r. PositionParams -> Record (FeeRelevantParams r) -> Number
unifiedFeeMult params sysParams =
  let baseFeeAmount = baseFee
      -- Tick fee component (if position has a narrow range)
      -- Calculate range width as a proxy for concentration
      rangeWidth = toNumber (params.tickUpper - params.tickLower)
      maxRange = toNumber (887272 * 2)  -- Full range width
      concentration = 1.0 - (rangeWidth / maxRange)  -- 0 = full range, 1 = single tick
      tickComponent = if concentration > 0.01  -- More than 1% concentrated
                      then (1.0 + concentration / sysParams.tickDepth) * (1.0 + sysParams.utilization)
                      else 1.0
      -- Duration premium (if duration > 0)
      durationComponent = if params.duration > 0
                          then 1.0 + toNumber params.duration / 180.0
                          else 1.0
      -- Leverage multiplier (if leverageRate > 0)
      leverageComponent = if params.leverageRate > 0.0
                          then (1.0 + sysParams.volatility) * leverageRateToRatio params.leverageRate
                          else 1.0
  in baseFeeAmount * tickComponent * durationComponent * leverageComponent

-- Calculate position fee based on unified parameters and system state
-- This is the primary fee calculation used in the UI
calculatePositionFee :: PositionParams -> SystemParams -> Number
calculatePositionFee posParams sysParams = 
  let -- Base fee calculation using unified multiplier
      baseFeeCalc = unifiedFeeMult posParams sysParams
      -- Count risk types for complexity premium
      hasLiquidity = posParams.tickLower /= posParams.tickUpper
      hasCredit = posParams.duration > 0
      hasPrice = posParams.leverageRate > 0.0
      numRisks = (if hasLiquidity then 1 else 0) + 
                 (if hasCredit then 1 else 0) + 
                 (if hasPrice then 1 else 0)
      -- Complexity premium for positions with multiple risk types
      complexityPremium = case numRisks of
        0 -> 1.0    -- No risk (shouldn't happen)
        1 -> 1.0    -- Single risk type
        2 -> 1.1    -- Two risk types: 10% premium
        _ -> 1.2    -- Three risk types: 20% premium
  in baseFeeCalc * complexityPremium

-- Calculate fee for a position based on its parameters and system state
-- Fees reflect the risk profile and resource consumption of each position
calculateFee :: Position -> Effect Number
calculateFee position = do
  -- Get system parameters
  depth <- totalLiquidity
  utilization <- utilizationRate
  vol <- volatility position.inputToken.tokenType position.params.pair
  total <- totalStaked
  
  -- Build complete system params with current values
  let sysParams = 
        { volatility: vol
        , liquidityDepth: depth
        , tickDepth: 1000.0  -- Default tick depth
        , utilization: utilization
        , totalStaked: total
        , protocolRevenue: 1.0
        , collateralRatio: 0.75
        , depositState: { jitoSOLBalance: 0.0
                        , syntheticSOLSupply: 0.0
                        , exchangeRate: 1.02
                        , lastUpdate: 0.0
                        }
        }
  
  -- Calculate unified fee
  pure $ calculatePositionFee position.params sysParams

--------------------------------------------------------------------------------
-- LP Fee Compensation Model (Dual Risk Mitigation)
--------------------------------------------------------------------------------

-- Parameters for LP fee compensation
type LPFeeParams =
  { vwbd :: Number          -- Volume-Weighted Book Depth
  , lvr :: Number           -- Loss vs Rebalancing
  , liquidityStress :: Number -- System liquidity stress (0 to 1)
  }

-- Calculate liquidity stress based on system state
calculateLiquidityStress :: Number -> Number -> Number -> Number
calculateLiquidityStress targetDepth currentDepth volatilityMultiplier =
  let depthStress = max 0.0 ((targetDepth - currentDepth) / targetDepth)
      beta = 0.7  -- Weight between depth and volatility factors
  in depthStress * beta + volatilityMultiplier * (1.0 - beta)

-- Dynamic fee budget function
-- λ(S) = λ_base * (1 + α * LiquidityStress(S))
dynamicFeeBudget :: Number -> Number -> Number -> Number
dynamicFeeBudget baseFeeBudget alpha liquidityStress =
  baseFeeBudget * (1.0 + alpha * liquidityStress)

-- Calculate LP compensation based on VWBD/LVR ratio and system state
-- This addresses both system risk (liquidity crisis) and user risk (LVR)
calculateLPCompensation :: Array LPFeeParams -> Number -> Number -> Number -> Int -> Number
calculateLPCompensation lpParams baseFeeBudget alpha liquidityStress lpIndex =
  let epsilon = 0.0001  -- Small constant to prevent division by zero
      -- Calculate dynamic fee budget based on system stress
      totalBudget = dynamicFeeBudget baseFeeBudget alpha liquidityStress
      
      -- Calculate VWBD/LVR ratio for each LP
      lpRatios = mapArray (\lp -> lp.vwbd / (lp.lvr + epsilon)) lpParams
      
      -- Sum of all ratios for normalization
      totalRatio = sum lpRatios
      
      -- Get this LP's ratio
      thisLPRatio = case index lpRatios lpIndex of
        Just ratio -> ratio
        Nothing -> 0.0
        
      -- Calculate proportional fee compensation
  in if totalRatio > 0.0
     then totalBudget * (thisLPRatio / totalRatio)
     else 0.0


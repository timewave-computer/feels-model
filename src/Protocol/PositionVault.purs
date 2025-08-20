-- | Position Vault - Base liquidity layer for the loan pool
-- |
-- | This module implements the two-layer liquidity model:
-- | 1. Base layer: PositionVault provides continuous liquidity
-- | 2. Order layer: Direct P2P loan matching in the pool
-- |
-- | Key features:
-- | - Vault deposits create permanent market-making liquidity
-- | - Monthly deposits lock shares for 28 days
-- | - Positions redenominate based on loan performance (no liquidations)
-- | - Vault continuously refreshes lending offers across rate ranges
-- |
-- | File Structure:
-- | 1. Balance Sheet - Asset and liability types
-- | 2. State - Vault state types and metrics
-- | 3. Functions - Core operations and business logic
module Protocol.PositionVault
  ( -- Balance Sheet exports
    VaultPosition
  , AllocationStrategy
  , DurationWeights
  , LeverageWeights
  , DepositParams
  , WithdrawParams
  , QuoteResult
  , PoolStrategy
  -- State exports
  , PositionVault
  , VaultConfig
  , VaultMetrics
  , PoolMetrics
  -- Function exports
  , initPositionVault
  , getQuote
  , withdraw
  , rebalance
  , getVaultMetrics
  , processExpiredVaultPositions
  , getPoolMetrics
  , createVaultPosition
  , swapDuration
  , monthlyDuration
  , blocksPerMonth
  , isExpired
  , isSwap
  , handleExpiredVaultPosition
  ) where

import Prelude hiding (max)
import Data.Ord (max)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array (filter, foldl, length, sortBy, head, mapMaybe)
import Data.Foldable (sum)
import Effect.Ref (Ref, read)
import Data.Int (toNumber, round)
import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import FFI (generateId, currentTime)
import Data.Number (log)
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Protocol.Token (TokenType(..), PositionToken, createPositionToken)
import Protocol.Pool (Pool, TickCoordinate, PoolPosition, LiquidityCube3D,
                       addLiquidity3D, removeLiquidity3D, swap3D, SwapResult3D,
                       calculateSwapPrice3D, calculateInvariant3D, getVirtualBalances,
                       Duration(..), Leverage(..), leverageMultiplier,
                       durationToTick, leverageToTick)
import Protocol.Common (BlockNumber, PositionId, PoolId, ShareAmount)
import Protocol.Vault (Vault, VaultState, BalanceSheet)
import Data.Map (Map)
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array (groupBy, sortWith)
import Data.Array as Array
import Protocol.Config (defaultProtocolConfig)

-- Time constants (12 second blocks)  
blocksPerMonth :: Int
blocksPerMonth = defaultProtocolConfig.time.blocksPerMonth

-- Yield constants
monthlyConvergenceYield :: Number
monthlyConvergenceYield = defaultProtocolConfig.positions.monthlyConvergenceYield

-- Rate adjustment constants
highUtilizationThreshold :: Number
highUtilizationThreshold = defaultProtocolConfig.risk.highUtilizationThreshold

lowUtilizationThreshold :: Number
lowUtilizationThreshold = defaultProtocolConfig.risk.lowUtilizationThreshold

rateAdjustmentFactor :: Number
rateAdjustmentFactor = defaultProtocolConfig.risk.rateAdjustmentFactor

-- Risk premium constants
juniorRiskPremium :: Number
juniorRiskPremium = defaultProtocolConfig.risk.juniorRiskPremium

-- Market spread constant
borrowLendSpread :: Number
borrowLendSpread = defaultProtocolConfig.risk.borrowLendSpread

--------------------------------------------------------------------------------
-- BALANCE SHEET
--------------------------------------------------------------------------------

-- | Vault position structure - tokenized multidimensional shares issued by the vault
type VaultPosition =
  { id :: PositionId
  , owner :: String
  , amount :: Number               -- Initial capital invested
  , price :: Number                -- Price level for liquidity
  , duration :: Duration           -- Time commitment
  , leverage :: Leverage           -- Risk/reward tier
  , lendAsset :: TokenType         -- Asset being lent
  , collateralAsset :: TokenType   -- Asset provided as collateral
  , rollover :: Boolean            -- Whether position auto-rolls
  , shares :: ShareAmount          -- Vault shares owned
  , createdAt :: BlockNumber       -- Creation block
  , value :: Number                -- Current value
  , lockedAmount :: Number         -- Amount locked below floor
  , accumulatedYield :: Number     -- Total yield earned
  , lastYieldClaim :: BlockNumber  -- Last yield claim block
  , feeGrowthInside0 :: Number     -- Fee growth (token0)
  , feeGrowthInside1 :: Number     -- Fee growth (token1)
  , positionToken :: PositionToken -- Fungible token representing position
  }

-- | Allocation strategy across the 3D space
type AllocationStrategy =
  { priceRange :: { min :: Number, max :: Number }     -- Price range to operate in
  , durationWeights :: DurationWeights                  -- Allocation across durations
  , leverageWeights :: LeverageWeights                  -- Allocation across leverage tiers
  }

type DurationWeights =
  { flash :: Number      -- Weight for flash duration (must sum to 1.0)
  , swap :: Number       -- Weight for swap duration
  , monthly :: Number    -- Weight for monthly duration
  }

type LeverageWeights =
  { senior :: Number     -- Weight for senior (1x) positions
  , junior :: Number     -- Weight for junior (3x) positions
  }

-- | Deposit parameters
type DepositParams =
  { depositor :: String
  , tokenIn :: TokenType
  , amountIn :: Number
  , minPositionTokens :: Number  -- Minimum position tokens to receive
  }

-- | Withdraw parameters
type WithdrawParams =
  { withdrawer :: String
  , positionTokensIn :: Number
  , tokenOut :: TokenType
  , minAmountOut :: Number
  }

-- | Quote result for deposits/withdrawals
type QuoteResult =
  { price :: Number              -- Effective price
  , positionTokensOut :: Number  -- For deposits
  , tokensOut :: Number          -- For withdrawals
  , slippage :: Number           -- Expected slippage
  , executionPath :: Array { price :: Number, duration :: Duration, leverage :: Leverage }  -- How liquidity will be placed
  }

-- | Pool-level strategy and state
type PoolStrategy =
  { poolId :: String
  , maxDeploymentRatio :: Number       -- Max % of POL to deploy
  , minLiquidityBuffer :: Number       -- Min POL to keep undeployed
  , targetUtilization :: Number        -- Target POL utilization
  -- Performance tracking
  , totalFeesCollected :: Number       -- Lifetime fees
  , deployedAmount :: Number           -- Currently deployed POL
  , performance :: Number              -- Performance score (0-1)
  }

--------------------------------------------------------------------------------
-- STATE
--------------------------------------------------------------------------------

-- | Position Vault that aggregates liquidity and manages positions
type PositionVault =
  { id :: String
  , config :: VaultConfig
  , totalDeposits :: Number               -- Total value deposited
  , activePositions :: Array PositionId   -- Active liquidity positions
  , positionTokenSupply :: Number         -- Total position tokens issued
  , lastRebalanceBlock :: BlockNumber     -- Last rebalancing
  , metrics :: VaultMetrics               -- Performance metrics
  , pool :: Pool                          -- Reference to 3D AMM pool
  , positions :: Map PositionId VaultPosition  -- All vault positions issued to depositors
  , liquidityCubes :: Array String        -- Active liquidity cube IDs in pool
  }

-- | Vault configuration parameters
type VaultConfig =
  { name :: String
  , targetAllocation :: AllocationStrategy     -- How to allocate across dimensions
  , rebalanceFrequency :: Int                  -- Blocks between rebalances
  , minOrderSize :: Number                     -- Minimum order size for efficiency
  , maxSlippage :: Number                      -- Maximum acceptable slippage
  , feeRate :: Number                          -- Vault management fee (basis points)
  }

-- | Vault performance metrics
type VaultMetrics =
  { totalValueLocked :: Number
  , utilizationRate :: Number     -- Fraction of capital deployed
  , avgExecutionPrice :: Number   -- Volume-weighted avg price
  , totalFeesEarned :: Number
  , sharpeRatio :: Number         -- Risk-adjusted returns
  }

-- | Pool metrics for protocol-level aggregation
type PoolMetrics = 
  { totalPOL :: Number                 -- Total POL allocated to pool
  , deployedAmount :: Number           -- Amount deployed in pool
  , totalFeesCollected :: Number       -- Total fees collected
  , performance :: Number              -- Performance score
  , tvl :: Number                      -- Total value locked
  , apy :: Number                      -- Annual percentage yield
  }

--------------------------------------------------------------------------------
-- FUNCTIONS
--------------------------------------------------------------------------------

-- | Initialize a new position vault
initPositionVault :: String -> VaultConfig -> Pool -> PositionVault
initPositionVault vaultId config pool =
  { id: vaultId
  , config: config
  , totalDeposits: 0.0
  , activePositions: []
  , positionTokenSupply: 0.0
  , lastRebalanceBlock: 0
  , metrics: initMetrics
  , pool: pool
  , positions: Map.empty
  , liquidityCubes: []
  }

initMetrics :: VaultMetrics
initMetrics =
  { totalValueLocked: 0.0
  , utilizationRate: 0.0
  , avgExecutionPrice: 0.0
  , totalFeesEarned: 0.0
  , sharpeRatio: 0.0
  }

-- Quoting

-- | Get quote for deposit or withdrawal
-- | This is where we provide CFMM-like instant quotes
getQuote :: PositionVault -> Either DepositParams WithdrawParams -> Effect QuoteResult
getQuote vault params = do
  case params of
    Left depositParams -> getDepositQuote vault depositParams
    Right withdrawParams -> getWithdrawQuote vault withdrawParams

-- | Get quote for deposit
getDepositQuote :: PositionVault -> DepositParams -> Effect QuoteResult
getDepositQuote vault params = do
  -- For vault deposits, we provide liquidity across 3D ranges
  let targetRateRange = vault.config.targetAllocation.priceRange  -- Rate range
      
      -- Position tokens issued based on share of vault
      newShare = params.amountIn / (vault.totalDeposits + params.amountIn)
      positionTokensOut = if vault.positionTokenSupply > 0.0
                          then newShare * vault.positionTokenSupply / (1.0 - newShare)
                          else params.amountIn  -- First deposit gets 1:1
      
      -- Calculate spot price in 3D space
      currentBalances = getVirtualBalances vault.pool { rateTick: 0, durationTick: 1, leverageTick: 0 }
      swapPrice = calculateSwapPrice3D currentBalances vault.pool.weights 
                    vault.pool.globalState.currentTick 
                    { rateTick: round (targetRateRange.min * 10000.0), durationTick: 1, leverageTick: 0 }
      
      -- Vault deposits don't have slippage - they're providing liquidity
      avgRate = (targetRateRange.min + targetRateRange.max) / 2.0
  
  pure { price: avgRate  -- Using "price" field to represent interest rate
       , positionTokensOut: positionTokensOut
       , tokensOut: 0.0
       , slippage: 0.0  -- No slippage for vault deposits
       , executionPath: []  -- Vault manages allocation internally
       }

-- | Get quote for withdrawal
getWithdrawQuote :: PositionVault -> WithdrawParams -> Effect QuoteResult
getWithdrawQuote vault params = do
  -- Calculate share being withdrawn
  let shareWithdrawn = params.positionTokensIn / vault.positionTokenSupply
      amountOut = shareWithdrawn * vault.totalDeposits * (1.0 - vault.config.feeRate / 10000.0)
      
      -- Simple quote for now - in practice would unwind positions
      price = vault.metrics.avgExecutionPrice
      
  pure { price: price
       , positionTokensOut: 0.0  -- Not applicable for withdrawals
       , tokensOut: amountOut
       , slippage: 0.0  -- No slippage on withdrawals in this model
       , executionPath: []
       }

-- Deposit/Withdraw Operations

-- | Withdraw from the vault
withdraw :: PositionVault -> WithdrawParams -> BlockNumber -> Effect (Either String PositionVault)
withdraw vault params currentBlock = do
  -- Get quote
  quote <- getWithdrawQuote vault params
  
  -- Check minimum output
  if quote.tokensOut < params.minAmountOut
    then pure $ Left "Output too low"
    else do
      -- In practice, would unwind positions proportionally
      -- For now, simplified withdrawal
      let updatedVault = vault
            { totalDeposits = vault.totalDeposits - quote.tokensOut
            , positionTokenSupply = vault.positionTokenSupply - params.positionTokensIn
            }
      
      pure $ Right updatedVault

-- Rebalancing Operations

-- | Rebalance vault positions based on market conditions
rebalance :: PositionVault -> BlockNumber -> Effect PositionVault
rebalance vault currentBlock =
  if currentBlock - vault.lastRebalanceBlock < vault.config.rebalanceFrequency
    then pure vault  -- Too soon to rebalance
    else do
      -- First, redenominate positions based on pool performance
      vaultAfterRedenomination <- redenominateVaultPositions vault currentBlock
      
      -- Analyze 3D market conditions
      let marketConditions = analyzeMarket3D vault.pool
          adjustedRateRange = adjustRateRange vault.config.targetAllocation.priceRange marketConditions
          totalValue = vault.totalDeposits  -- Use current deposits
          
          -- Recalculate allocation with new rates
          newConfig = vault.config { targetAllocation = vault.config.targetAllocation { priceRange = adjustedRateRange } }
          vaultWithNewConfig = vaultAfterRedenomination { config = newConfig }
          newAllocations = calculateOptimalAllocation vaultWithNewConfig totalValue
      
      -- Remove old liquidity and add new (would update pool in practice)
      newCubeIds <- placeVaultLiquidity3D vaultWithNewConfig newAllocations currentBlock
      
      pure vaultAfterRedenomination { lastRebalanceBlock = currentBlock, liquidityCubes = newCubeIds }

-- Internal Helpers

-- | Calculate optimal allocation for vault liquidity
calculateOptimalAllocation :: PositionVault -> Number -> Array { amount :: Number, rate :: Number, duration :: Duration, leverage :: Leverage }
calculateOptimalAllocation vault totalAmount =
  let
    config = vault.config.targetAllocation
    
    -- Split by duration (only Flash and Monthly for lending)
    flashAmount = totalAmount * config.durationWeights.flash
    monthlyAmount = totalAmount * config.durationWeights.monthly
    
    -- Rate range for lending offers
    minRate = config.priceRange.min  -- Minimum acceptable rate
    maxRate = config.priceRange.max  -- Maximum rate to offer
    midRate = (minRate + maxRate) / 2.0
    
    -- Create allocations for loan durations only
    allocations = 
      [ { amount: flashAmount * config.leverageWeights.senior
        , rate: midRate, duration: Flash, leverage: Senior
        }
      , { amount: flashAmount * config.leverageWeights.junior
        , rate: midRate * juniorRiskPremium, duration: Flash, leverage: Junior  -- Higher rate for junior
        }
      , { amount: monthlyAmount * config.leverageWeights.senior
        , rate: midRate, duration: Monthly, leverage: Senior
        }
      , { amount: monthlyAmount * config.leverageWeights.junior
        , rate: midRate * juniorRiskPremium, duration: Monthly, leverage: Junior
        }
      ]
    
    -- Filter out allocations below minimum order size
    validAllocations = filter (\a -> a.amount >= vault.config.minOrderSize) allocations
    
  in validAllocations

-- | Redenominate vault positions based on 3D pool performance
-- TODO: Implement actual performance tracking and redenomination
redenominateVaultPositions :: PositionVault -> BlockNumber -> Effect PositionVault
redenominateVaultPositions vault currentBlock = 
  -- Currently a no-op - positions maintain their value
  -- In production, would track pool performance and adjust position values
  pure vault

-- | Place vault liquidity in 3D AMM
placeVaultLiquidity3D :: PositionVault -> Array { amount :: Number, rate :: Number, duration :: Duration, leverage :: Leverage } -> BlockNumber -> Effect (Array String)
placeVaultLiquidity3D vault allocations currentBlock = do
  -- Place liquidity in 3D ranges
  cubeIds <- traverse placeOne allocations
  pure cubeIds
  where
    placeOne alloc = do
      -- Convert allocation to tick coordinates
      let centerTick = { rateTick: round (alloc.rate * 10000.0)
                       , durationTick: durationToTick alloc.duration
                       , leverageTick: leverageToTick alloc.leverage
                       }
          -- Create range around center tick
          lowerTick = { rateTick: centerTick.rateTick - 100  -- +/- 1% range
                      , durationTick: centerTick.durationTick
                      , leverageTick: centerTick.leverageTick
                      }
          upperTick = { rateTick: centerTick.rateTick + 100
                      , durationTick: centerTick.durationTick
                      , leverageTick: centerTick.leverageTick
                      }
      
      -- Add liquidity to pool (would update pool state in practice)
      let cubeId = vault.id <> "-" <> show currentBlock <> "-" <> show centerTick.rateTick
      pure cubeId

-- Duration and leverage to tick conversions are imported from Pool.purs

-- | Analyze 3D market conditions
analyzeMarket3D :: Pool -> { avgLendRate :: Number, avgBorrowRate :: Number, utilization :: Number }
analyzeMarket3D pool =
  let
    -- Calculate utilization from virtual balances
    balances = pool.globalState.virtualBalances
    totalBalance = balances.r + balances.d + balances.l
    avgBalance = totalBalance / 3.0
    
    -- Estimate rates from current tick position
    currentRate = toNumber pool.globalState.currentTick.rateTick / 10000.0
    
  in { avgLendRate: currentRate      -- Current rate
     , avgBorrowRate: currentRate * (1.0 + borrowLendSpread)  -- 20% spread
     , utilization: pool.globalState.totalLiquidity / totalBalance
     }

-- | Adjust rate range based on market conditions
adjustRateRange :: { min :: Number, max :: Number } -> { avgLendRate :: Number, avgBorrowRate :: Number, utilization :: Number } -> { min :: Number, max :: Number }
adjustRateRange baseRange market =
  -- Adjust rates based on market conditions
  if market.utilization > highUtilizationThreshold
  then { min: baseRange.min * (1.0 + rateAdjustmentFactor), max: baseRange.max * (1.0 + rateAdjustmentFactor) }  -- Increase rates if high utilization
  else if market.utilization < lowUtilizationThreshold
  then { min: baseRange.min * (1.0 - rateAdjustmentFactor), max: baseRange.max * (1.0 - rateAdjustmentFactor) }  -- Decrease rates if low utilization
  else baseRange

-- | Get term expiry for vault positions
getTermExpiry :: VaultPosition -> Maybe Int
getTermExpiry pos = case pos.duration of
  Flash -> Just (pos.createdAt + 1)
  Monthly -> Just (pos.createdAt + blocksPerMonth)
  Swap -> Nothing

-- | Process expired vault positions
processExpiredVaultPositions :: BlockNumber -> Array VaultPosition -> Array VaultPosition
processExpiredVaultPositions currentBlock = map processOne
  where
    processOne pos = 
      if isExpired currentBlock pos
      then handleExpiredVaultPosition pos currentBlock
      else pos

-- | Get vault metrics
getVaultMetrics :: PositionVault -> VaultMetrics
getVaultMetrics vault = vault.metrics

-- | Price to tick conversion
priceToTick :: Number -> Int
priceToTick price = round (log price / log 1.0001)

-- Position Management Functions

-- | Create a new vault position with specified parameters
createVaultPosition :: PositionId -> String -> Number -> Number -> Duration -> Leverage -> TokenType -> TokenType -> Boolean -> ShareAmount -> BlockNumber -> Effect VaultPosition
createVaultPosition id owner amount price duration leverage lendAsset collateralAsset rollover shares currentBlock = do
  -- Create the fungible position token
  let poolId = "POOL-" <> show id
      tickLower = round (price * 100.0) - 50
      tickUpper = round (price * 100.0) + 50
      leverageTier = show leverage
      durationStr = show duration
  
  posToken <- createPositionToken poolId tickLower tickUpper leverageTier durationStr
  
  pure
    { id
    , owner
    , amount
    , price
    , duration
    , leverage
    , lendAsset
    , collateralAsset
    , rollover
    , shares
    , createdAt: currentBlock
    , value: amount
    , lockedAmount: 0.0
    , accumulatedYield: 0.0
    , lastYieldClaim: currentBlock
    , feeGrowthInside0: 0.0
    , feeGrowthInside1: 0.0
    , positionToken: posToken
    }

-- | Get current vault position value
getVaultPositionValue :: VaultPosition -> Number
getVaultPositionValue pos = pos.value

-- | Check if vault position is spot (cross spread immediately)
isSwap :: VaultPosition -> Boolean
isSwap pos = pos.duration == Swap

-- | Check if vault position has a term commitment
isTermVaultPosition :: VaultPosition -> Boolean
isTermVaultPosition pos = pos.duration == Flash || pos.duration == Monthly

-- | Duration constructors
swapDuration :: Duration
swapDuration = Swap

monthlyDuration :: Duration  
monthlyDuration = Monthly


-- | Calculate monthly yield using time-to-maturity convergence
calculateMonthlyYield :: VaultPosition -> BlockNumber -> Number
calculateMonthlyYield vaultPos currentBlock =
  let
    blocksHeld = currentBlock - vaultPos.createdAt
    blocksIntoMonth = blocksHeld `mod` blocksPerMonth
    progressToMaturity = toNumber blocksIntoMonth / toNumber blocksPerMonth
    
    -- Time-based yield: value converges to par as maturity approaches
    -- This convergence is yield for the position holder
    convergenceYield = progressToMaturity * monthlyConvergenceYield  -- 0.1% convergence yield
    
  in convergenceYield

-- Term Expiry Functions

-- | Get the next expiry block for a given duration
getNextExpiry :: Duration -> Int -> Int
getNextExpiry duration currentBlock = case duration of
  Flash -> currentBlock + 1
  Monthly -> getNextMonthlyExpiry currentBlock
  Swap -> 2147483647  -- MaxInt32

-- | Calculate the next monthly boundary block
getNextMonthlyExpiry :: Int -> Int
getNextMonthlyExpiry currentBlock =
  let currentPeriod = currentBlock / blocksPerMonth
      nextPeriod = currentPeriod + 1
  in nextPeriod * blocksPerMonth

-- | Calculate blocks remaining until expiry
blocksUntilExpiry :: VaultPosition -> Int -> Int
blocksUntilExpiry vaultPosition currentBlock = case vaultPosition.duration of
  Flash -> max 0 ((vaultPosition.createdAt + 1) - currentBlock)
  Monthly -> max 0 ((vaultPosition.createdAt + blocksPerMonth) - currentBlock)
  Swap -> 2147483647

-- | Check if a vault position has expired
isExpired :: Int -> VaultPosition -> Boolean
isExpired currentBlock position = case position.duration of
  Flash -> false
  Monthly -> currentBlock >= (position.createdAt + blocksPerMonth)
  Swap -> false

-- | Handle an expired vault position
handleExpiredVaultPosition :: VaultPosition -> Int -> VaultPosition
handleExpiredVaultPosition position currentBlock =
  if position.rollover && position.duration == Monthly
    then position { createdAt = currentBlock }
    else rollToFlash position

-- | Convert a vault position to flash loan
rollToFlash :: VaultPosition -> VaultPosition
rollToFlash position = position { duration = Flash }

-- | Track upcoming term expiry dates
type TermSchedule =
  { nextMonthly :: BlockNumber
  }

-- | A batch of vault positions expiring at the same block
type ExpiryBatch =
  { expiryBlock :: BlockNumber
  , positions :: Array VaultPosition
  }

-- | Get the next term expiry blocks
getNextTermExpiries :: BlockNumber -> TermSchedule
getNextTermExpiries currentBlock =
  { nextMonthly: getNextMonthlyExpiry currentBlock
  }

-- | Group vault positions by their expiry block
groupVaultPositionsByExpiry :: Array VaultPosition -> Array ExpiryBatch
groupVaultPositionsByExpiry positions =
  let
    termPositionsWithExpiry = Array.mapMaybe getPositionWithExpiry positions
    sorted = sortWith _.expiry termPositionsWithExpiry
    grouped = groupBy (\a b -> a.expiry == b.expiry) sorted
    
    toBatch :: NonEmptyArray { position :: VaultPosition, expiry :: BlockNumber } -> ExpiryBatch
    toBatch group = 
      let positions' = NEA.toArray $ map _.position group
          expiry = (NEA.head group).expiry
      in { expiryBlock: expiry, positions: positions' }
  in
    map toBatch grouped
  where
    getPositionWithExpiry :: VaultPosition -> Maybe { position :: VaultPosition, expiry :: BlockNumber }
    getPositionWithExpiry pos = 
      getTermExpiry pos >>= \expiry -> 
        Just { position: pos, expiry: expiry }

-- Pool Vault Functions

-- | Get pool metrics from a vault
getPoolMetrics :: forall d e. Ref (Vault d { poolId :: String, totalFeesCollected :: Number, deployedAmount :: Number, performance :: Number | e }) -> Effect PoolMetrics
getPoolMetrics poolRef = do
  vault <- read poolRef
  let strategy = vault.state.strategyState
      totalPOL = case vault.state.balanceSheet.assets of
        [] -> 0.0
        assets -> sum $ map (\a -> a.amount) assets
      
  pure { totalPOL: totalPOL
       , deployedAmount: strategy.deployedAmount
       , totalFeesCollected: strategy.totalFeesCollected
       , performance: strategy.performance
       , tvl: totalPOL + strategy.deployedAmount  -- Simplified TVL calculation
       , apy: strategy.performance * 0.2  -- Simplified APY calculation (20% max APY at perfect performance)
       }

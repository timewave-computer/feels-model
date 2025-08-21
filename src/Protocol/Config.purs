-- | Protocol configuration parameters
-- |
-- | This module centralizes all configurable protocol parameters including:
-- | - Fee rates and pricing parameters
-- | - Risk management thresholds  
-- | - Position and pool parameters
-- | - Time constants and block parameters
-- |
-- | By centralizing these values, governance can easily adjust protocol behavior
-- | without modifying core business logic across multiple files.
module Protocol.Config
  ( -- Fee Configuration
    ProtocolFees(..)
  , defaultProtocolFees
  -- Risk Management
  , RiskParameters(..)
  , defaultRiskParameters
  -- Pool Configuration
  , PoolParameters(..)
  , defaultPoolParameters
  -- Position Configuration
  , PositionParameters(..)
  , defaultPositionParameters
  -- Time Constants
  , TimeConstants(..)
  , defaultTimeConstants
  -- Oracle Configuration
  , OracleParameters(..)
  , defaultOracleParameters
  -- Launch Configuration
  , LaunchParameters(..)
  , defaultLaunchParameters
  -- FeelsSOL Configuration
  , FeelsSOLParameters(..)
  , defaultFeelsSOLParameters
  -- Buffer Configuration
  , BufferParameters(..)
  , defaultBufferParameters
  -- Protocol Configuration
  , ProtocolParameters(..)
  , defaultProtocolParameters
  -- Complete Protocol Configuration
  , ProtocolConfig(..)
  , defaultProtocolConfig
  ) where

import Prelude

--------------------------------------------------------------------------------
-- FEE CONFIGURATION
--------------------------------------------------------------------------------

-- | Protocol fee configuration
type ProtocolFees =
  { entryFeeRate :: Number              -- Fee for entering positions/vaults
  , exitFeeRate :: Number               -- Fee for exiting positions/vaults
  , managementFeeRate :: Number         -- Ongoing management fee (basis points)
  , performanceFeeRate :: Number        -- Performance-based fee
  , polAllocationRate :: Number         -- Portion of fees allocated to POL
  }

-- | Default protocol fees
defaultProtocolFees :: ProtocolFees
defaultProtocolFees =
  { entryFeeRate: 0.001                -- 0.1% entry fee
  , exitFeeRate: 0.001                 -- 0.1% exit fee
  , managementFeeRate: 200.0           -- 200 basis points (2% annually)
  , performanceFeeRate: 0.15           -- 15% performance fee
  , polAllocationRate: 0.5             -- 50% of fees to POL
  }

--------------------------------------------------------------------------------
-- RISK MANAGEMENT PARAMETERS
--------------------------------------------------------------------------------

-- | Risk management configuration
type RiskParameters =
  { highUtilizationThreshold :: Number  -- High utilization warning level
  , lowUtilizationThreshold :: Number   -- Low utilization rebalance trigger
  , rateAdjustmentFactor :: Number      -- Rate adjustment sensitivity
  , juniorRiskPremium :: Number         -- Risk premium for junior positions
  , borrowLendSpread :: Number          -- Spread between borrow/lend rates
  , minLiquidityBuffer :: Number        -- Minimum liquidity buffer ratio
  , concentrationRiskLimit :: Number    -- Maximum concentration in single asset
  , leverageVoLatilityFactor :: Number  -- Leverage-volatility risk adjustment
  }

-- | Default risk parameters
defaultRiskParameters :: RiskParameters
defaultRiskParameters =
  { highUtilizationThreshold: 0.8      -- 80% utilization
  , lowUtilizationThreshold: 0.3       -- 30% utilization
  , rateAdjustmentFactor: 0.1          -- 10% rate adjustment
  , juniorRiskPremium: 1.5             -- 50% higher rate for junior positions
  , borrowLendSpread: 0.2              -- 20% spread between borrow and lend rates
  , minLiquidityBuffer: 0.005          -- 0.5% minimum buffer
  , concentrationRiskLimit: 0.3        -- 30% max concentration
  , leverageVoLatilityFactor: 0.0025   -- 0.25% per sqrt(day) at 15% vol
  }

--------------------------------------------------------------------------------
-- POOL CONFIGURATION
--------------------------------------------------------------------------------

-- | Pool parameter configuration
type PoolParameters =
  { minLiquidity :: Number              -- Minimum liquidity for pool creation
  , seniorLeverageMultiplier :: Number  -- Senior position leverage
  , juniorLeverageMultiplier :: Number  -- Junior position leverage
  , monthlyConvergenceRate :: Number    -- Monthly position convergence rate
  , defaultPoolVolatility :: Number     -- Default volatility assumption
  , maxDeploymentRatio :: Number        -- Max POL deployment ratio
  , flashFeeBase :: Number              -- Base fee for flash loans (tick 30 = 0.3%)
  , swapPriceBase :: Number             -- Base price multiplier (tick 100 = 1.01)
  -- 3D AMM constants
  , initialVirtualBalance :: Number     -- Initial virtual balance for AMM
  , baseRateYield :: Number             -- Base yield rate per block
  , baseDurationYield :: Number         -- Base duration yield per block
  , baseLeverageYield :: Number         -- Base leverage yield per block
  , defaultSwapBuffer :: Number         -- Buffer for swap calculations
  , tickBase :: Number                  -- Base for tick-to-price conversion
  , ticksPerPercent :: Number           -- Ticks per percentage point
  -- Duration multipliers
  , flashDurationMultiplier :: Number   -- Flash duration multiplier
  , monthlyDurationMultiplier :: Number -- Monthly duration multiplier
  , swapDurationMultiplier :: Number    -- Swap duration multiplier
  -- Leverage risk multipliers
  , seniorLeverageRiskMultiplier :: Number -- Senior leverage risk multiplier
  , juniorLeverageRiskMultiplier :: Number -- Junior leverage risk multiplier
  , juniorYieldPremium :: Number        -- Junior yield premium multiplier
  -- Swap solver constants
  , swapSolverBufferRatio :: Number     -- Buffer ratio for swap solver
  , swapSolverPremiumRatio :: Number    -- Premium ratio for swap solver
  , maxOracleDeviation :: Number        -- Maximum allowed price deviation from oracle
  -- Rebalancing parameters
  , rebalanceThreshold :: Number        -- Minimum deviation to trigger rebalancing
  , maxRebalanceSlippage :: Number      -- Maximum slippage per rebalance
  }

-- | Default pool parameters
defaultPoolParameters :: PoolParameters
defaultPoolParameters =
  { minLiquidity: 1000.0               -- Minimum liquidity amount
  , seniorLeverageMultiplier: 1.0      -- 1x leverage for senior
  , juniorLeverageMultiplier: 3.0      -- 3x leverage for junior
  , monthlyConvergenceRate: 0.001      -- 0.1% convergence per month
  , defaultPoolVolatility: 0.15        -- 15% default volatility
  , maxDeploymentRatio: 0.8            -- 80% max deployment
  , flashFeeBase: 0.003                -- 0.3% base flash fee
  , swapPriceBase: 1.01                -- 1% base price change
  -- 3D AMM constants
  , initialVirtualBalance: 1000.0      -- Initial virtual balance for AMM
  , baseRateYield: 0.0001              -- 0.01% base rate yield per block
  , baseDurationYield: 0.00005         -- 0.005% base duration yield per block
  , baseLeverageYield: 0.00008         -- 0.008% base leverage yield per block
  , defaultSwapBuffer: 1.1             -- 10% buffer for swap calculations
  , tickBase: 1.0001                   -- Base for tick-to-price conversion
  , ticksPerPercent: 100.0             -- 100 ticks per percentage point
  -- Duration multipliers
  , flashDurationMultiplier: 0.8       -- 20% discount for flash
  , monthlyDurationMultiplier: 1.0     -- Base rate for monthly
  , swapDurationMultiplier: 1.2        -- 20% premium for swap
  -- Leverage risk multipliers
  , seniorLeverageRiskMultiplier: 1.0  -- Senior leverage risk multiplier
  , juniorLeverageRiskMultiplier: 3.0  -- Junior leverage risk multiplier
  , juniorYieldPremium: 1.5            -- 50% higher yield for junior
  -- Swap solver constants
  , swapSolverBufferRatio: 0.8         -- Buffer ratio for dimension adjustment
  , swapSolverPremiumRatio: 0.9        -- Premium ratio for dimension adjustment
  , maxOracleDeviation: 0.1             -- 10% max deviation from oracle price
  -- Rebalancing parameters
  , rebalanceThreshold: 0.05             -- 5% deviation triggers rebalancing
  , maxRebalanceSlippage: 0.02           -- 2% max slippage per rebalance
  }

--------------------------------------------------------------------------------
-- POSITION CONFIGURATION
--------------------------------------------------------------------------------

-- | Position parameter configuration
type PositionParameters =
  { monthlyConvergenceYield :: Number   -- Yield from time-to-maturity convergence
  , monthlyPositionMultiplier :: Number -- Multiplier for monthly positions
  , minOrderSize :: Number              -- Minimum position size
  , maxSlippage :: Number               -- Maximum acceptable slippage
  , rolloverGracePeriod :: Int          -- Grace period for position rollovers (blocks)
  }

-- | Default position parameters
defaultPositionParameters :: PositionParameters
defaultPositionParameters =
  { monthlyConvergenceYield: 0.001     -- 0.1% convergence yield
  , monthlyPositionMultiplier: 2.0     -- 2x multiplier for monthly positions
  , minOrderSize: 100.0                -- Minimum order size
  , maxSlippage: 0.05                  -- 5% maximum slippage
  , rolloverGracePeriod: 1200          -- ~2.5 hours grace period
  }

--------------------------------------------------------------------------------
-- TIME CONSTANTS
--------------------------------------------------------------------------------

-- | Time-related configuration (all in blocks at 12 second intervals)
type TimeConstants =
  { blocksPerMonth :: Int               -- Blocks in a 28-day month
  , blocksPerYear :: Number             -- Blocks in a year (for APY calculations)
  , rebalanceFrequency :: Int           -- Default rebalance frequency
  , oracleCacheTimeout :: Number        -- Oracle cache timeout (milliseconds)
  , priceHistoryLimit :: Int            -- Maximum price history records
  }

-- | Default time constants
defaultTimeConstants :: TimeConstants
defaultTimeConstants =
  { blocksPerMonth: 201600             -- 28 days * 24 hours * 60 minutes * 60 seconds / 12 seconds
  , blocksPerYear: 2628000.0           -- Approximate blocks per year
  , rebalanceFrequency: 7200           -- Rebalance every ~24 hours
  , oracleCacheTimeout: 60000.0        -- 1 minute oracle cache
  , priceHistoryLimit: 100             -- Keep last 100 price records
  }

--------------------------------------------------------------------------------
-- ORACLE CONFIGURATION
--------------------------------------------------------------------------------

-- | Oracle parameter configuration
type OracleParameters =
  { fiveMinuteWindow :: Number          -- 5-minute window in milliseconds
  , fifteenMinuteWindow :: Number       -- 15-minute window in milliseconds
  , oneHourWindow :: Number             -- 1-hour window in milliseconds
  , priceValidityPeriod :: Number       -- Price validity period
  , maxPriceDeviation :: Number         -- Maximum allowed price deviation
  }

-- | Default oracle parameters
defaultOracleParameters :: OracleParameters
defaultOracleParameters =
  { fiveMinuteWindow: 300000.0         -- 5 * 60 * 1000
  , fifteenMinuteWindow: 900000.0      -- 15 * 60 * 1000
  , oneHourWindow: 3600000.0           -- 60 * 60 * 1000
  , priceValidityPeriod: 300000.0      -- 5 minutes
  , maxPriceDeviation: 0.1             -- 10% max deviation
  }

--------------------------------------------------------------------------------
-- LAUNCH CONFIGURATION
--------------------------------------------------------------------------------

-- | Token launch parameter configuration
type LaunchParameters =
  { phaseAllocationTolerance :: Number  -- Tolerance for phase allocation validation
  , monthlyLockBlocks :: Int            -- Lock duration for monthly launch phase
  , discountedPhasePrice :: Number      -- Price for monthly commitment phase
  , standardPhasePrice :: Number        -- Price for swap phase
  , maxPhasesPerLaunch :: Int           -- Maximum phases per launch
  }

-- | Default launch parameters
defaultLaunchParameters :: LaunchParameters
defaultLaunchParameters =
  { phaseAllocationTolerance: 0.01     -- 1% tolerance
  , monthlyLockBlocks: 216000          -- ~30 days at 12s blocks
  , discountedPhasePrice: 0.15         -- Discounted price for monthly commitment
  , standardPhasePrice: 0.30           -- Standard price for swap purchases
  , maxPhasesPerLaunch: 5              -- Maximum 5 phases per launch
  }

--------------------------------------------------------------------------------
-- FEELSSOL CONFIGURATION
--------------------------------------------------------------------------------

-- | FeelsSOL system parameter configuration
type FeelsSOLParameters =
  { defaultOraclePrice :: Number        -- Default price if oracle unavailable
  , entryFee :: Number                  -- Fee for JitoSOL → FeelsSOL conversion
  , exitFee :: Number                   -- Fee for FeelsSOL → JitoSOL conversion
  , polAllocationRate :: Number         -- Portion of fees allocated to POL system
  , solvencyCheckFrequency :: Int       -- How often to check solvency (blocks)
  }

-- | Default FeelsSOL parameters
defaultFeelsSOLParameters :: FeelsSOLParameters
defaultFeelsSOLParameters =
  { defaultOraclePrice: 1.05           -- 5% premium default price
  , entryFee: 0.001                    -- 0.1% entry fee
  , exitFee: 0.001                     -- 0.1% exit fee
  , polAllocationRate: 0.5             -- 50% of fees to POL
  , solvencyCheckFrequency: 1200       -- Check every ~4 hours
  }

--------------------------------------------------------------------------------
-- BUFFER CONFIGURATION
--------------------------------------------------------------------------------

-- | Buffer management configuration
type BufferParameters =
  { targetRatio :: Number              -- Target buffer as ratio of total backing
  , minRatio :: Number                 -- Minimum buffer ratio before health warning
  , maxRatio :: Number                 -- Maximum buffer ratio before rebalancing
  , rebalanceThreshold :: Number       -- Threshold for automatic rebalancing
  }

-- | Default buffer parameters
defaultBufferParameters :: BufferParameters
defaultBufferParameters =
  { targetRatio: 0.1                   -- 10% target buffer
  , minRatio: 0.05                     -- 5% minimum buffer
  , maxRatio: 0.2                      -- 20% maximum buffer
  , rebalanceThreshold: 0.02           -- 2% rebalance threshold
  }

--------------------------------------------------------------------------------
-- PROTOCOL CONFIGURATION
--------------------------------------------------------------------------------

-- | Protocol-level configuration
type ProtocolParameters =
  { maxPoolAllocation :: Number        -- Max % per pool
  , minPoolAllocation :: Number        -- Min % per pool
  , rebalanceThreshold :: Number       -- Min delta to trigger rebalance
  , rebalanceFrequency :: Int          -- Blocks between rebalances
  , protocolFeeShare :: Number         -- % of fees to protocol
  , feeToStakers :: Number            -- % of fees to stakers
  , feeToPOL :: Number                -- % of fees to POL
  , feeToTreasury :: Number           -- % of fees to treasury
  , maxConcentrationRisk :: Number    -- Max acceptable concentration
  , minHealthScore :: Number          -- Min acceptable health
  }

-- | Default protocol parameters
defaultProtocolParameters :: ProtocolParameters
defaultProtocolParameters =
  { maxPoolAllocation: 0.3             -- 30% max per pool
  , minPoolAllocation: 0.05            -- 5% min per pool
  , rebalanceThreshold: 0.02           -- 2% delta triggers rebalance
  , rebalanceFrequency: 100            -- Every 100 blocks
  , protocolFeeShare: 0.15             -- 15% of fees to protocol
  , feeToStakers: 0.4                  -- 40% to stakers
  , feeToPOL: 0.4                      -- 40% to POL
  , feeToTreasury: 0.2                 -- 20% to treasury
  , maxConcentrationRisk: 0.7          -- 70% max risk
  , minHealthScore: 0.6                -- 60% min health
  }

--------------------------------------------------------------------------------
-- COMPLETE PROTOCOL CONFIGURATION
--------------------------------------------------------------------------------

-- | Complete protocol configuration
type ProtocolConfig =
  { fees :: ProtocolFees
  , risk :: RiskParameters
  , pools :: PoolParameters
  , positions :: PositionParameters
  , time :: TimeConstants
  , oracle :: OracleParameters
  , launch :: LaunchParameters
  , feelsSOL :: FeelsSOLParameters
  , buffer :: BufferParameters
  , protocol :: ProtocolParameters
  }

-- | Default protocol configuration
defaultProtocolConfig :: ProtocolConfig
defaultProtocolConfig =
  { fees: defaultProtocolFees
  , risk: defaultRiskParameters
  , pools: defaultPoolParameters
  , positions: defaultPositionParameters
  , time: defaultTimeConstants
  , oracle: defaultOracleParameters
  , launch: defaultLaunchParameters
  , feelsSOL: defaultFeelsSOLParameters
  , buffer: defaultBufferParameters
  , protocol: defaultProtocolParameters
  }
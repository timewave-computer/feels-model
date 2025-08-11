-- | Pool Module - Core pool-centric architecture for Feels Protocol
-- |
-- | This module implements the unified tick system with vertically integrated issuance.
-- | Each pool is a self-contained market for a FeelsSOL/Token pair with:
-- | - Living ticks that maintain local state
-- | - Synchronized term commitments
-- | - Two-tranche system
-- | - Dual token issuance control
module Pool
  ( -- Newtypes
    JuniorShares(..)
  , SeniorShares(..)
  , unwrapJuniorShares
  , unwrapSeniorShares
  -- Pool types
  , PoolState
  , TickBook
  , AggregateMetrics
  , ManagedLiquidity
  , IssuanceState
  , TrancheMetrics
  , TokenPair
  , TrancheBucket
  , TermBucket
  , IssuanceMetrics
  , IssuanceController
  , PositionBook
  , Distribution
  -- Functions
  , distributeManagedLiquidity
  , calculateTrancheReturns
  , calculateJuniorMultiplier
  , updateTranches
  , TrancheReturns
  , PoolPnL
  ) where

import Prelude
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Array (uncons, cons)
import Data.Tuple (Tuple(..))
import Data.Number (abs)
-- Array type is in Prelude
import Token (TokenType)
import Position (TermCommitment)
import Common (PositionId, BlockNumber)
import Tick (Tick)

--------------------------------------------------------------------------------
-- Newtype Wrappers
--------------------------------------------------------------------------------

-- | Junior tranche shares with type safety
newtype JuniorShares = JuniorShares Number

derive instance eqJuniorShares :: Eq JuniorShares
derive instance ordJuniorShares :: Ord JuniorShares
derive newtype instance semiringJuniorShares :: Semiring JuniorShares
derive newtype instance ringJuniorShares :: Ring JuniorShares

instance showJuniorShares :: Show JuniorShares where
  show (JuniorShares n) = "JuniorShares " <> show n

-- | Senior tranche shares with type safety
newtype SeniorShares = SeniorShares Number

derive instance eqSeniorShares :: Eq SeniorShares
derive instance ordSeniorShares :: Ord SeniorShares
derive newtype instance semiringseniorShares :: Semiring SeniorShares
derive newtype instance ringseniorShares :: Ring SeniorShares

instance showSeniorShares :: Show SeniorShares where
  show (SeniorShares n) = "SeniorShares " <> show n

-- | Extract the Number from JuniorShares
unwrapJuniorShares :: JuniorShares -> Number
unwrapJuniorShares (JuniorShares n) = n

-- | Extract the Number from SeniorShares
unwrapSeniorShares :: SeniorShares -> Number
unwrapSeniorShares (SeniorShares n) = n

--------------------------------------------------------------------------------
-- Core Pool Structure
--------------------------------------------------------------------------------

-- | Complete state for a single pool (FeelsSOL/Token pair)
type PoolState =
  { pair :: TokenPair                -- (FeelsSOL, Token)
  , tickBook :: TickBook            -- Living ticks with metrics
  , aggregate :: AggregateMetrics   -- Pool-wide metrics
  , positions :: PositionBook       -- ALL positions (unified)
  , managed :: ManagedLiquidity     -- Protocol-managed liquidity with tranches
  , issuance :: IssuanceState       -- Dual token minting/burning
  , trancheMetrics :: TrancheMetrics -- Pool-wide tranche health
  }

-- | Token pair for the pool
type TokenPair =
  { base :: TokenType               -- Always FeelsSOL
  , quote :: TokenType              -- The paired Feels token
  }

-- | Book of living ticks
type TickBook =
  { ticks :: Array Tick             -- Ordered array of active ticks
  , tickSpacing :: Number           -- Minimum tick spacing
  , activeTicks :: Int              -- Count of non-empty ticks
  }

-- | Pool-wide aggregate metrics
type AggregateMetrics =
  { spot :: Number                  -- Current spot price
  , twap :: Number                  -- Time-weighted average price
  , volatility :: Number            -- Price volatility
  , volume24h :: Number             -- 24h trading volume
  , depth :: Number                 -- Total liquidity depth
  , feeRate :: Number               -- Current fee rate
  }

-- | Position tracking - stores only IDs, actual positions in PoolRegistry
type PositionBook =
  { positionIds :: Array PositionId  -- Position IDs in this pool
  , totalManaged :: Number           -- Sum of managed positions
  , totalSpot :: Number              -- Sum of spot positions
  , lockedValue :: Number            -- Value locked below floor
  }

--------------------------------------------------------------------------------
-- Managed Liquidity & Tranches
--------------------------------------------------------------------------------

-- | Protocol-managed liquidity with tranches
type ManagedLiquidity =
  { total :: Number                -- Total protocol-managed liquidity
  , senior :: TrancheBucket        -- Senior positions (1x exposure)
  , junior :: TrancheBucket        -- Junior positions (up to 3x exposure)
  , polAllocation :: Number        -- POL allocated to this pool (from global POL)
  , polUtilized :: Number          -- POL currently deployed in positions
  , distribution :: Distribution   -- How it's distributed across ticks
  , termBuckets :: Map TermCommitment TermBucket  -- Organize by term
  }

-- | Tranche bucket for tracking senior/junior positions
type TrancheBucket =
  { amount :: Number               -- Total capital in this tranche
  , shares :: Number               -- Total shares issued
  , multiplier :: Number           -- Current exposure multiplier
  }

-- | Term bucket for organizing positions by term
type TermBucket =
  { termCommitment :: TermCommitment
  , amount :: Number
  , positions :: Array PositionId  -- Position IDs
  , expiryBlock :: Maybe BlockNumber -- Expiry block for term positions
  }


-- | Placeholder for distribution strategy
type Distribution = Number  -- Simplified for MVP

-- | Pool-wide tranche health metrics
type TrancheMetrics =
  { seniorTotal :: Number           -- Total senior capital
  , juniorTotal :: Number           -- Total junior capital
  , juniorMultiplier :: Number      -- Current junior multiplier (1.5x to 3x)
  , totalExposure :: Number         -- Total market exposure
  , juniorHealth :: Number          -- Junior tranche health (0 to 1)
  }


--------------------------------------------------------------------------------
-- Issuance System
--------------------------------------------------------------------------------

-- | State for dual token issuance
type IssuanceState =
  { feelsSolOracle :: Number       -- External JitoSOL/SOL rate
  , tokenSupply :: Number          -- Current supply of quote token
  , lastIssuance :: Number         -- Last issuance timestamp
  , issuanceMetrics :: IssuanceMetrics
  , controller :: IssuanceController
  }

-- | Metrics for issuance decisions
type IssuanceMetrics =
  { demandPressure :: Number       -- Buy vs sell imbalance
  , velocity :: Number             -- Trading volume / supply
  , depthRatio :: Number          -- Liquidity / market cap
  , priceDeviation :: Number      -- Distance from target
  }

-- | Issuance control parameters
type IssuanceController =
  { maxSupplyGrowth :: Number      -- Max % supply increase per day
  , minLiquidity :: Number         -- Min liquidity before issuance
  , targetUtilization :: Number    -- Target capital efficiency
  , priceStabilization :: Boolean  -- Whether to stabilize price
  }

--------------------------------------------------------------------------------
-- Tranche System
--------------------------------------------------------------------------------

-- | Pool profit/loss
type PoolPnL =
  { amount :: Number  -- Positive for profit, negative for loss
  }

-- | Returns allocated to each tranche
type TrancheReturns =
  { senior :: Number
  , junior :: Number
  }

--------------------------------------------------------------------------------
-- Liquidity Distribution
--------------------------------------------------------------------------------

-- | Distribute managed liquidity across ticks based on tranches
distributeManagedLiquidity :: ManagedLiquidity -> AggregateMetrics -> Array Tick -> Array Tick
distributeManagedLiquidity managed metrics ticks =
  let -- Junior tranche provides up to 3x their capital in exposure
      juniorExposure = managed.junior.amount * managed.junior.multiplier
      seniorExposure = managed.senior.amount * 1.0
      
      -- Total liquidity to distribute
      totalLiquidity = juniorExposure + seniorExposure + managed.polAllocation
      
      -- Simple distribution: concentrate around spot price
      distribution = calculateDistribution metrics.spot totalLiquidity
      
  in applyDistribution distribution ticks

-- | Calculate simple normal distribution around spot
calculateDistribution :: Number -> Number -> Array (Tuple Number Number)
calculateDistribution spotPrice totalLiquidity =
  -- MVP: Simple 5-tick distribution
  let tickPrices = 
        [ spotPrice * 0.98
        , spotPrice * 0.99
        , spotPrice * 1.00
        , spotPrice * 1.01
        , spotPrice * 1.02
        ]
      
      -- Normal-like distribution
      weights = [0.1, 0.25, 0.3, 0.25, 0.1]
      
  in zipWith (\price weight -> Tuple price (totalLiquidity * weight)) tickPrices weights

-- | Apply distribution to ticks
applyDistribution :: Array (Tuple Number Number) -> Array Tick -> Array Tick
applyDistribution distribution ticks =
  -- MVP: Just return ticks unchanged
  -- TODO: In full implementation, would update tick liquidity
  ticks

--------------------------------------------------------------------------------
-- Tranche Returns
--------------------------------------------------------------------------------

-- | Calculate returns for each tranche based on P&L
calculateTrancheReturns :: PoolPnL -> ManagedLiquidity -> TrancheReturns
calculateTrancheReturns pnl managed =
  if pnl.amount > 0.0 then
    -- Profits: Junior gets enhanced share (3x weight)
    let juniorWeight = managed.junior.amount * 3.0
        seniorWeight = managed.senior.amount * 1.0
        totalWeight = juniorWeight + seniorWeight
        
    in { senior: pnl.amount * (seniorWeight / totalWeight)
       , junior: pnl.amount * (juniorWeight / totalWeight)
       }
  else
    -- Losses: Junior takes first loss up to 90% of their capital
    let maxJuniorLoss = managed.junior.amount * 0.9
        juniorLoss = min maxJuniorLoss (abs pnl.amount)
        seniorLoss = max 0.0 (abs pnl.amount - juniorLoss)
        
    in { senior: -seniorLoss
       , junior: -juniorLoss
       }

--------------------------------------------------------------------------------
-- Dynamic Adjustment
--------------------------------------------------------------------------------

-- | Calculate dynamic junior multiplier based on pool health
calculateJuniorMultiplier :: PoolState -> Number
calculateJuniorMultiplier pool =
  let baseMultiplier = 3.0
      juniorRatio = pool.managed.junior.amount / 
                   (pool.managed.junior.amount + pool.managed.senior.amount)
      
      -- Reduce multiplier as junior tranche depletes
      healthAdjustment = 
        if juniorRatio < 0.1 then 0.5      -- Minimum 1.5x when depleted
        else if juniorRatio < 0.2 then 0.7  -- Reduced to 2.1x
        else 1.0                            -- Full 3x when healthy
      
  in baseMultiplier * healthAdjustment

-- | Update tranches after market moves
updateTranches :: PoolState -> PoolState
updateTranches pool =
  let currentValue = calculatePoolValue pool
      initialValue = pool.managed.senior.amount + pool.managed.junior.amount
      pnl = { amount: currentValue - initialValue }
      
      returns = calculateTrancheReturns pnl pool.managed
      
      -- Update tranche values (junior never goes to zero)
      newSenior = pool.managed.senior 
        { amount = pool.managed.senior.amount + returns.senior }
      
      newJunior = pool.managed.junior
        { amount = max (pool.managed.junior.amount * 0.1)  -- Keep minimum 10%
                      (pool.managed.junior.amount + returns.junior)
        , multiplier = calculateJuniorMultiplier pool
        }
      
  in pool { managed = pool.managed { senior = newSenior, junior = newJunior } }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Calculate total pool value (simplified)
calculatePoolValue :: PoolState -> Number
calculatePoolValue pool =
  -- MVP: Just return current managed amount
  -- In full implementation, would calculate from tick values
  pool.managed.total

-- | Helper to zip two arrays
zipWith :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith f = go
  where
    go [] _ = []
    go _ [] = []
    go as bs = case uncons as, uncons bs of
      Just { head: a, tail: as' }, Just { head: b, tail: bs' } -> 
        cons (f a b) (go as' bs')
      _, _ -> []


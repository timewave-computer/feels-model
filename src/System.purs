module System where

import Prelude
import Deposit (DepositState, initDepositState)

--------------------------------------------------------------------------------
-- System Parameters Type
--------------------------------------------------------------------------------

-- System parameters for fee calculations
-- These parameters represent global system state that affects risk pricing
type SystemParams =
  { volatility :: Number         -- Market volatility coefficient (0-1): higher values indicate more price instability
  , liquidityDepth :: Number     -- Total liquidity available in system (in base units): affects slippage and price impact
  , tickDepth :: Number          -- Average tick depth for AMM positions: deeper ticks mean more stable pricing
  , utilization :: Number        -- System utilization rate (0-1): ratio of borrowed to available capital
  , totalStaked :: Number        -- Total value staked in the system: affects scarcity premium for staking positions
  , protocolRevenue :: Number    -- Protocol revenue multiplier (typically 1.0): adjusts fees for protocol sustainability
  , collateralRatio :: Number    -- Collateral ratio for Synthetic SOL->feels asset creation (0.75 = 75% collateralization)
  , depositState :: DepositState -- Deposit layer state for jitoSOL/synthetic SOL conversions
  }

-- Default system parameters
defaultSystemParams :: SystemParams
defaultSystemParams = 
  { volatility: 0.25
  , liquidityDepth: 5000000.0
  , tickDepth: 1000.0
  , utilization: 0.65
  , totalStaked: 1000000.0
  , protocolRevenue: 1.0
  , collateralRatio: 0.75  -- 75% collateralization for Synthetic SOL->feels asset creation
  , depositState: initDepositState
  }
-- | Incentives Module - Fee mechanism for pool-based system
-- |
-- | In the pool-centric system, incentives are handled through
-- | tranche returns and pool fees, not lending rates.
module Protocol.Incentive
  ( MarketDynamics
  , DynamicsConfig
  , initMarketDynamics
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new)
import Protocol.Oracle (Oracle)
import Protocol.POL (POLState)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Configuration for market dynamics
type DynamicsConfig =
  { baseRate :: Number    -- Base fee rate for the protocol
  , spread :: Number      -- Protocol spread between tranches
  }

-- | Market dynamics state for fee calculations
type MarketDynamics =
  { config :: Ref DynamicsConfig
  , oracle :: Oracle
  , polState :: POLState
  }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Initialize market dynamics with default configuration
initMarketDynamics :: Oracle -> POLState -> Effect MarketDynamics
initMarketDynamics oracle polState = do
  config <- new { baseRate: 0.05, spread: 0.01 }
  pure { config, oracle, polState }
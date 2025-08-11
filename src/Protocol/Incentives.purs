-- | Incentives Module - Fee mechanism for pool-based system
-- |
-- | In the pool-centric system, incentives are handled through
-- | tranche returns and pool fees, not lending rates.
module Protocol.Incentives
  ( MarketDynamics
  , DynamicsConfig
  , RateComponents
  , DynamicsResult
  , initMarketDynamics
  , calculateDynamics
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read)
import Protocol.Oracle (Oracle, takeMarketSnapshot)
import Protocol.POL (POLState)
import Protocol.Position (TermCommitment, spotTerm)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Components of rate calculation (kept for compatibility)
type RateComponents =
  { baseRate :: Number
  , riskPremium :: Number
  , marketAdjustment :: Number
  , protocolSpread :: Number
  }

-- Result of dynamics calculation (kept for compatibility)
type DynamicsResult =
  { lenderRate :: Number
  , borrowerRate :: Number
  , effectiveSpread :: Number
  , polFlow :: Number
  , components :: RateComponents
  , operationType :: TermCommitment
  }

-- Simplified config
type DynamicsConfig =
  { baseRate :: Number
  , spread :: Number
  }

-- Market dynamics state
type MarketDynamics =
  { config :: Ref DynamicsConfig
  , oracle :: Oracle
  , polState :: POLState
  }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Initialize with default config
initMarketDynamics :: Oracle -> POLState -> Effect MarketDynamics
initMarketDynamics oracle polState = do
  config <- new { baseRate: 0.05, spread: 0.01 }
  pure { config, oracle, polState }

-- | Calculate dynamics (simplified for new system)
calculateDynamics :: forall r1 r2. MarketDynamics -> { leverageConfig :: { targetLeverage :: Number | r1 } | r2 } -> Effect DynamicsResult
calculateDynamics dynamics _ = do
  config <- read dynamics.config
  metrics <- takeMarketSnapshot dynamics.oracle
  
  let components = 
        { baseRate: config.baseRate
        , riskPremium: 0.0
        , marketAdjustment: metrics.volatility * 0.1
        , protocolSpread: config.spread
        }
      
      lenderRate = config.baseRate
      borrowerRate = lenderRate + config.spread
  
  pure 
    { lenderRate
    , borrowerRate
    , effectiveSpread: config.spread
    , polFlow: config.spread * 0.2
    , components
    , operationType: spotTerm
    }
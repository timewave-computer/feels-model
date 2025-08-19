-- | Protocol Metrics Stub - Minimal implementation to satisfy imports
module Protocol.Metric where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)

-- | POL metrics type
type POLMetrics = 
  { totalValue :: Number
  , growthRate :: Number
  }

-- | Get POL metrics
getPOLMetrics :: Effect POLMetrics
getPOLMetrics = pure { totalValue: 0.0, growthRate: 0.0 }

-- | Get total fees collected
getProtocolTotalFeesCollected :: Effect Number
getProtocolTotalFeesCollected = pure 0.0

-- | Calculate 24h growth rate
calculateGrowthRate24h :: Number -> Number -> Number
calculateGrowthRate24h _ _ = 0.0

-- | Process monthly position compensation (stub)
processMonthlyPositionCompensation :: forall a. a -> Effect Unit
processMonthlyPositionCompensation _ = pure unit
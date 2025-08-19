-- | Common Position Types
module Protocol.PositionTypes
  ( Duration(..)
  , Leverage(..)
  , leverageMultiplier
  ) where

import Prelude

-- | Duration options - discrete time commitments
data Duration
  = Flash     -- Single block loan, capital returned immediately
  | Monthly   -- 28-day term loan, capital returned at expiry
  | Spot      -- Cross the spread immediately (not a loan duration)

derive instance eqDuration :: Eq Duration
derive instance ordDuration :: Ord Duration

instance showDuration :: Show Duration where
  show Flash = "Flash"
  show Monthly = "Monthly"
  show Spot = "Spot"

-- | Leverage tiers - discrete exposure levels
data Leverage  
  = Senior    -- 1x exposure, protected
  | Junior    -- 3x exposure, higher risk/reward

derive instance eqLeverage :: Eq Leverage
derive instance ordLeverage :: Ord Leverage

instance showLeverage :: Show Leverage where
  show Senior = "Senior (1x)"
  show Junior = "Junior (3x)"

-- | Get numeric multiplier for leverage tier
leverageMultiplier :: Leverage -> Number
leverageMultiplier Senior = 1.0
leverageMultiplier Junior = 3.0
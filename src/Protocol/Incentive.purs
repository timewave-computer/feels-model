-- | Protocol Incentive Stub - Minimal implementation to satisfy imports
module Protocol.Incentive where

import Prelude
import Effect (Effect)

-- | Pool fee data type
type PoolFeeData = 
  { poolId :: String
  , liquidity :: Number
  , feeGrowthGlobal0X128 :: Number
  , feeGrowthGlobal1X128 :: Number
  }

-- | Collect fees from pools (stub)
collectFeesFromPools :: forall a. a -> Array PoolFeeData -> Effect Unit
collectFeesFromPools _ _ = pure unit
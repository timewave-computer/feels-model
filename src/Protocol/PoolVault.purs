-- | Pool Vault Stub - Minimal implementation to satisfy imports
module Protocol.PoolVault where

import Prelude

-- | Pool vault type
type PoolVault = 
  { id :: String
  , totalValue :: Number
  }

-- | Pool metrics type
type PoolMetrics = 
  { tvl :: Number
  , apy :: Number
  }

-- | Get pool metrics
getPoolMetrics :: PoolVault -> PoolMetrics
getPoolMetrics _ = { tvl: 0.0, apy: 0.0 }

-- | Update pool metrics
updatePoolMetrics :: PoolVault -> PoolMetrics -> PoolVault
updatePoolMetrics vault _ = vault
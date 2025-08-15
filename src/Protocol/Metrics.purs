-- | Protocol Metrics Module
-- |
-- | This module provides comprehensive metrics and analytics for the protocol.
-- | It extracts key performance indicators, calculates protocol health metrics,
-- | and provides insights into system performance and utilization.
-- |
-- | Key Metrics:
-- | - Total Value Locked (TVL) across the protocol
-- | - POL reserves and utilization
-- | - Fee collection and distribution
-- | - Token-specific metrics including POL floors
-- | - System-wide performance indicators
module Protocol.Metrics
  ( getProtocolMetrics
  , ProtocolMetrics
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (read)
import Data.Either (Either(..))
import Data.Array (fromFoldable)
import Unsafe.Coerce (unsafeCoerce)

-- Import protocol types
import UI.ProtocolState (ProtocolState, IndexerQuery(..))
import Protocol.POL (POLState)
import Protocol.Common (QueryResult(..))
import UI.Query (executeQuery)

--------------------------------------------------------------------------------
-- TYPES
--------------------------------------------------------------------------------

-- | Comprehensive protocol metrics
type ProtocolMetrics =
  { totalValueLocked :: Number
  , polReserves :: Number
  , totalFeesCollected :: Number
  , tokenMetrics :: Array { ticker :: String, supply :: Number, polFloor :: Number }
  }

--------------------------------------------------------------------------------
-- PROTOCOL METRICS EXTRACTION
--------------------------------------------------------------------------------

-- | Extract key metrics from protocol state
-- | Used for analysis and monitoring of protocol performance
getProtocolMetrics :: ProtocolState -> Effect ProtocolMetrics
getProtocolMetrics state = do
  -- Get POL state
  polState <- read state.polState
  
  -- Get all tokens
  tokensResult <- executeQuery GetAllTokens state
  let tokens = case tokensResult of
        Right (TokenList ts) -> ts
        _ -> []
  
  -- Calculate metrics for each token
  let totalSupply = 1000000.0  -- Placeholder for total FeelsSOL supply
      tokenMetrics = map (\foreignToken ->
        let token = unsafeCoerce foreignToken :: { ticker :: String, supply :: Number, live :: Boolean }
            tokenShare = token.supply / totalSupply
            -- POL floor is the permanent POL backing for this token
            -- Only permanent POL contributes to floor, not borrowed POL
            polFloor = if token.supply > 0.0 
                       then (polState.permanentPOL * tokenShare) / token.supply
                       else 0.0
        in { ticker: token.ticker
           , supply: token.supply
           , polFloor: polFloor
           }
      ) tokens
  
  -- Calculate total value locked
  -- TVL includes all POL plus liquidity in pools
  let tvl = polState.totalPOL * 10.0  -- Simplified calculation - would include pool liquidity
      totalFees = polState.totalPOL    -- POL represents accumulated fees
  
  pure 
    { totalValueLocked: tvl
    , polReserves: polState.totalPOL
    , totalFeesCollected: totalFees
    , tokenMetrics: tokenMetrics
    }
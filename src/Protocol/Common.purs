-- | Common Module - Shared types and type aliases
-- |
-- | This module contains shared types used across Protocol and UI layers
module Protocol.Common
  ( -- Type aliases
    PoolId
  , PositionId
  , BlockNumber
  , ShareAmount
  -- Result types
  , CommandResult(..)
  , QueryResult(..)
  ) where

import Prelude
import Data.Maybe (Maybe)

-- Forward declarations to avoid circular dependencies
foreign import data Position :: Type
foreign import data TokenMetadata :: Type
foreign import data OfferingResult :: Type

-- TokenType import is safe as it doesn't create cycles
data TokenType = JitoSOL | FeelsSOL | Token String

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- | Unique identifier for a pool
type PoolId = String

-- | Unique identifier for a position
type PositionId = Int

-- | Block number on the blockchain
type BlockNumber = Int

-- | Amount of shares
type ShareAmount = Number

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Results from protocol commands (on-chain operations)
data CommandResult
  = TokenCreated TokenMetadata
  | PositionCreated Position
  | TokensTransferred { from :: String, to :: String, token :: TokenType, amount :: Number }
  | GatewayEntered { user :: String, feelsSOLMinted :: Number }
  | GatewayExited { user :: String, jitoSOLReceived :: Number }
  | UnbondingInitiated Int
  | PositionWithdrawn Int
  | OfferingCreated String String  -- poolId, ticker
  | OfferingPhaseStarted String String -- poolId, phase
  | OfferingPhaseCompleted String String -- poolId, phase

derive instance eqCommandResult :: Eq CommandResult

-- | Results from indexer queries (off-chain reads)
data QueryResult
  = TokenList (Array TokenMetadata)
  | PositionList (Array Position)
  | Balance Number
  | TokenInfo (Maybe TokenMetadata)
  | LenderOfferList (Array Position)
  | SystemStatsResult 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , polBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
  | POLMetricsResult 
    { balance :: Number
    , growthRate24h :: Number
    }
  | TargetTokenInfo (Maybe String)
  | ActiveOfferingsList (Array { poolId :: String, phase :: String })
  | OfferingStatusResult (Maybe OfferingResult)

-- Eq instance would require manual implementation due to complex types
-- | Common types and aliases shared across the Feels Protocol
-- |
-- | This module provides fundamental types used throughout the protocol:
-- | - Type aliases for identifiers and blockchain primitives
-- | - Command results for on-chain operations
-- | - Query results for off-chain data retrieval
-- |
-- | Note: Uses forward declarations to prevent circular dependency issues
module Protocol.Common
  ( -- Type aliases
    PoolId
  , PositionId
  , BlockNumber
  , ShareAmount
  -- Result types
  , CommandResult(..)
  , QueryResult(..)
  -- Foreign types (needed for CommandResult)
  , TokenMetadata
  , Position
  , OfferingResult
  ) where

import Prelude
import Data.Maybe (Maybe)
import Protocol.Token (TokenType(..))

--------------------------------------------------------------------------------
-- FORWARD DECLARATIONS
--------------------------------------------------------------------------------
-- Avoid circular dependencies by forward-declaring complex types

foreign import data Position :: Type
foreign import data TokenMetadata :: Type
foreign import data OfferingResult :: Type

--------------------------------------------------------------------------------
-- TYPE ALIASES
--------------------------------------------------------------------------------
-- Simple type aliases for better code clarity and type safety

-- | Unique identifier for a trading pool (e.g., "FeelsSOL/FEELS")
type PoolId = String

-- | Unique identifier for a user position
type PositionId = Int

-- | Block number on the blockchain (Solana slot number)
type BlockNumber = Int

-- | Amount of shares in a position or pool
type ShareAmount = Number

--------------------------------------------------------------------------------
-- COMMAND RESULTS
--------------------------------------------------------------------------------
-- Results returned from protocol commands (on-chain state modifications)

-- | Results from executing protocol commands
-- | These represent successful on-chain operations and their outcomes
data CommandResult
  = TokenCreated TokenMetadata                   -- New token was created
  | PositionCreated Position                     -- New position was opened
  | TokensTransferred                            -- Tokens were transferred between accounts
      { from :: String
      , to :: String
      , token :: TokenType
      , amount :: Number
      }
  | FeelsSOLMinted                               -- JitoSOL was converted to FeelsSOL
      { user :: String
      , feelsSOLMinted :: Number
      }
  | FeelsSOLBurned                               -- FeelsSOL was converted back to JitoSOL
      { user :: String
      , jitoSOLReceived :: Number
      }
  | UnbondingInitiated Int                       -- Position unbonding started (positionId)
  | PositionWithdrawn Int                        -- Position was fully withdrawn (positionId)
  | OfferingCreated String String                -- New token offering created (poolId, ticker)
  | OfferingPhaseStarted String String           -- Offering phase began (poolId, phase)
  | OfferingPhaseCompleted String String         -- Offering phase ended (poolId, phase)

--------------------------------------------------------------------------------
-- QUERY RESULTS
--------------------------------------------------------------------------------
-- Results returned from indexer queries (off-chain data reads)

-- | Results from querying protocol state
-- | These provide read-only access to protocol data for UI and analytics
data QueryResult
  = TokenList (Array TokenMetadata)              -- List of token metadata
  | PositionList (Array Position)                -- List of user positions
  | Balance Number                               -- Token balance amount
  | TokenInfo (Maybe TokenMetadata)              -- Single token information
  | LenderOfferList (Array Position)             -- Available lending offers
  | SystemStatsResult                            -- Overall protocol statistics
      { totalValueLocked :: Number
      , totalUsers :: Int
      , activePositions :: Int
      , liveTokens :: Int
      , totalLenderOffers :: Int
      , polBalance :: Number
      , feelsSOLSupply :: Number
      , jitoSOLLocked :: Number
      }
  | POLMetricsResult                             -- Protocol-Owned Liquidity metrics
      { balance :: Number
      , growthRate24h :: Number
      }
  | TargetTokenInfo (Maybe String)               -- Target token for position
  | ActiveOfferingsList                          -- Currently active token offerings
      (Array { poolId :: String, phase :: String })
  | OfferingStatusResult (Maybe OfferingResult)  -- Current offering status
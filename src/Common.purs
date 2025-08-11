-- | Common Module - Shared type aliases
-- |
-- | This module contains minimal shared type aliases to avoid circular dependencies
module Common
  ( PoolId
  , PositionId
  , BlockNumber
  , ShareAmount
  ) where

-- | Unique identifier for a pool
type PoolId = String

-- | Unique identifier for a position
type PositionId = Int

-- | Block number on the blockchain
type BlockNumber = Int

-- | Amount of shares
type ShareAmount = Number
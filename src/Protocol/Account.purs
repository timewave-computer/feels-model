-- | Account types for the Feels Protocol
-- |
-- | Defines the core account structures that would exist on-chain in Solana.
-- | In the actual implementation, these would be Program Derived Addresses (PDAs)
-- | with associated account data structures for efficient on-chain storage.
module Protocol.Account
  ( TokenBalance
  ) where

import Prelude
import Protocol.Token (TokenType)

--------------------------------------------------------------------------------
-- CORE ACCOUNT TYPES
--------------------------------------------------------------------------------
-- Essential account structures for on-chain token accounting

-- | Balance of a specific token held in an account
-- | In Solana implementation:
-- | - Would be part of account data structure
-- | - Stored as u64 for precision with decimal conversion
-- | - Associated with Program Derived Address (PDA)
type TokenBalance = 
  { token :: TokenType    -- Token type identifier
  , amount :: Number      -- Token amount (converted from u64 on-chain)
  }
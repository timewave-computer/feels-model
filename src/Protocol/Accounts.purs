-- | Account types for the Feels Protocol.
-- | Defines the core account structures that would exist on-chain.
-- | In Solana, these would be Program Derived Addresses (PDAs).
module Protocol.Accounts
  ( TokenBalance
  ) where

import Prelude
import Protocol.Token (TokenType)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Balance of a specific token
-- | In Solana, this would be part of the account data structure
type TokenBalance = 
  { token :: TokenType
  , amount :: Number
  }
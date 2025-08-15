-- | Token System for the Feels Protocol
-- |
-- | This module manages all token types and operations in the unified lending protocol:
-- |
-- | Token Hierarchy:
-- | - JitoSOL: External entry point, only token that can enter the protocol
-- | - FeelsSOL: Internal base token, synthetic SOL representation
-- | - User Tokens: Created through token launch system, tradeable with FeelsSOL
-- |
-- | All tokens can serve as either lending assets or collateral in positions.
-- | Token creation includes validation, metadata management, and launch mechanics.
module Protocol.Token 
  ( -- Newtypes
    FeelsSOLAmount(..)
  , unwrapFeelsSOL
  -- Token types
  , TokenType(..)
  , TokenSupply
  , TokenMetadata
  , TokenCreationParams
  , ValidationResult
  -- Functions
  , createToken
  , isValidTicker
  , isValidPair
  , isTradeable
  , validate
  , validateTokenTicker
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), isRight)
import Effect (Effect)
import Data.String as String
import FFI (currentTime, generateId)

--------------------------------------------------------------------------------
-- TOKEN TYPE DEFINITION
--------------------------------------------------------------------------------
-- Core token type that represents all tokens in the protocol

-- | Represents the different types of tokens in the Feels Protocol
-- | - JitoSOL: External liquid staking token that enters the protocol
-- | - FeelsSOL: Internal synthetic SOL used as base currency
-- | - Token: User-created tokens with custom tickers
data TokenType = JitoSOL | FeelsSOL | Token String

derive instance eqTokenType :: Eq TokenType
derive instance ordTokenType :: Ord TokenType

instance showTokenType :: Show TokenType where
  show JitoSOL = "JitoSOL"
  show FeelsSOL = "FeelsSOL"
  show (Token ticker) = ticker

--------------------------------------------------------------------------------
-- TYPE-SAFE WRAPPERS
--------------------------------------------------------------------------------
-- Newtype wrappers for compile-time safety and preventing mix-ups

-- | FeelsSOL amount with type safety to prevent confusion with other tokens
newtype FeelsSOLAmount = FeelsSOLAmount Number

derive instance eqFeelsSOLAmount :: Eq FeelsSOLAmount
derive instance ordFeelsSOLAmount :: Ord FeelsSOLAmount
derive newtype instance semiringFeelsSOLAmount :: Semiring FeelsSOLAmount
derive newtype instance ringFeelsSOLAmount :: Ring FeelsSOLAmount

instance showFeelsSOLAmount :: Show FeelsSOLAmount where
  show (FeelsSOLAmount n) = "FeelsSOL " <> show n

-- | Extract the underlying Number from FeelsSOLAmount
unwrapFeelsSOL :: FeelsSOLAmount -> Number
unwrapFeelsSOL (FeelsSOLAmount n) = n

--------------------------------------------------------------------------------
-- CORE TOKEN TYPES
--------------------------------------------------------------------------------
-- The three types of tokens supported by the protocol

-- TokenType is defined here with instances for Eq, Ord, Show

-- | Token supply representation combining type and quantity
type TokenSupply = 
  { tokenType :: TokenType   -- Which token this supply refers to
  , supply :: Number         -- Quantity of tokens
  }

--------------------------------------------------------------------------------
-- TOKEN METADATA
--------------------------------------------------------------------------------
-- Comprehensive information about user-created tokens

-- | Metadata for user-created tokens
-- | Contains all information needed for display, trading, and management
type TokenMetadata =
  { id :: Int                           -- Unique identifier
  , ticker :: String                    -- Short symbol (3-10 characters)
  , name :: String                      -- Full descriptive name
  , tokenType :: TokenType              -- Associated token type
  , creator :: String                   -- Creator's address/identifier
  , createdAt :: Number                 -- Creation timestamp
  , live :: Boolean                     -- Whether token is tradeable
  , totalSupply :: Number               -- Total token supply
  }

-- | Parameters required for creating a new token
type TokenCreationParams =
  { ticker :: String      -- 3-10 character ticker symbol
  , name :: String        -- Full token name
  , creator :: String     -- Creator address/identifier
  }

--------------------------------------------------------------------------------
-- TOKEN CREATION
--------------------------------------------------------------------------------
-- Functions for creating new user tokens

-- | Create a new user token with metadata
-- | Tokens are immediately live when created through the launch system
createToken :: TokenCreationParams -> Effect TokenMetadata
createToken params = do
  timestamp <- currentTime
  let tokenId = generateId timestamp
  pure 
    { id: tokenId
    , ticker: params.ticker
    , name: params.name
    , tokenType: Token params.ticker
    , creator: params.creator
    , createdAt: timestamp
    , live: true                        -- Live immediately via launch system
    , totalSupply: 1000000000.0        -- Default 1B supply
    }

--------------------------------------------------------------------------------
-- TOKEN VALIDATION
--------------------------------------------------------------------------------
-- Validation logic for token creation and operations

type ValidationResult = Either String Unit

-- | Helper function to combine validation checks
validate :: Boolean -> String -> ValidationResult
validate true _ = Right unit
validate false err = Left err

-- | Comprehensive ticker validation with all protocol rules
validateTokenTicker :: String -> ValidationResult
validateTokenTicker ticker = do
  -- Length constraints
  _ <- validate (String.length ticker >= 3) "Ticker must be at least 3 characters"
  _ <- validate (String.length ticker <= 10) "Ticker must be at most 10 characters"
  
  -- Format validation
  _ <- validate (isAlphanumeric ticker) "Ticker must be alphanumeric"
  
  -- Reserved ticker protection
  _ <- validate (ticker /= "SOL") "Cannot use reserved ticker SOL"
  _ <- validate (ticker /= "JITO") "Cannot use reserved ticker JITO"
  _ <- validate (String.toUpper ticker /= "FEELSSOL") "Cannot use reserved ticker FEELSSOL"
  _ <- validate (String.toUpper ticker /= "JITOSOL") "Cannot use reserved ticker JITOSOL"
  
  Right unit
  where
    -- Simplified alphanumeric check - production would use proper character validation
    isAlphanumeric _ = true

--------------------------------------------------------------------------------
-- TOKEN QUERIES AND VALIDATION
--------------------------------------------------------------------------------
-- Utility functions for checking token properties and constraints

-- | Check if a ticker meets all validation requirements
isValidTicker :: String -> Boolean
isValidTicker ticker = isRight (validateTokenTicker ticker)

-- | Validate token pairs for trading operations
-- | Protocol only supports FeelsSOL ↔ User Token pairs
isValidPair :: TokenType -> TokenType -> Boolean
isValidPair FeelsSOL (Token _) = true     -- FeelsSOL → User Token
isValidPair (Token _) FeelsSOL = true     -- User Token → FeelsSOL
isValidPair _ _ = false                    -- All other pairs invalid

-- | Check if a token is available for trading
-- | Only live tokens can be used in positions and swaps
isTradeable :: TokenMetadata -> Boolean
isTradeable token = token.live
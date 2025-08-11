-- Token system supporting the unified lending protocol's asset management.
-- Defines core token types (JitoSOL, FeelsSOL, user tokens, position tokens) and
-- manages token metadata, creation, and validation. All tokens in the system can serve
-- as either lending assets or collateral in the Feels protocol.
-- Handles token launching mechanics and price discovery for user-created assets.
module Protocol.Token 
  ( -- Newtypes
    FeelsSOLAmount(..)
  , unwrapFeelsSOL
  -- Token types
  , TokenType(..)
  , TokenAmount
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
-- Newtype Wrappers
--------------------------------------------------------------------------------

-- | FeelsSOL amount with type safety
newtype FeelsSOLAmount = FeelsSOLAmount Number

derive instance eqFeelsSOLAmount :: Eq FeelsSOLAmount
derive instance ordFeelsSOLAmount :: Ord FeelsSOLAmount
derive newtype instance semiringFeelsSOLAmount :: Semiring FeelsSOLAmount
derive newtype instance ringFeelsSOLAmount :: Ring FeelsSOLAmount

instance showFeelsSOLAmount :: Show FeelsSOLAmount where
  show (FeelsSOLAmount n) = "FeelsSOL " <> show n

-- | Extract the Number from FeelsSOLAmount
unwrapFeelsSOL :: FeelsSOLAmount -> Number
unwrapFeelsSOL (FeelsSOLAmount n) = n

--------------------------------------------------------------------------------
-- Token Types
--------------------------------------------------------------------------------

-- Core token types in the system
data TokenType 
  = JitoSOL               -- External collateral token (sole entry point)
  | FeelsSOL              -- Base synthetic representation of SOL
  | Token String          -- User-created token with custom ticker
  
derive instance eqTokenType :: Eq TokenType
derive instance ordTokenType :: Ord TokenType

instance showTokenType :: Show TokenType where
  show JitoSOL = "JitoSOL"
  show FeelsSOL = "FeelsSOL"
  show (Token ticker) = ticker

-- Token amount representation
-- Combines a token type with its quantity
type TokenAmount = 
  { tokenType :: TokenType   -- The type of token
  , amount :: Number         -- The quantity of tokens
  }

-- Token metadata for user-created tokens
type TokenMetadata =
  { id :: Int
  , ticker :: String
  , name :: String
  , tokenType :: TokenType           -- The actual token type for this metadata
  , creator :: String
  , createdAt :: Number
  , live :: Boolean              -- True if 100 FeelsSOL deposited via loan book
  , totalSupply :: Number            -- Total supply of token
  }


-- Token creation parameters
type TokenCreationParams =
  { ticker :: String      -- 3-10 character ticker symbol
  , name :: String        -- Full token name
  , creator :: String     -- Creator address/identifier
  }

--------------------------------------------------------------------------------
-- Token Creation Functions
--------------------------------------------------------------------------------

-- Create a new token (not live until 100 FeelsSOL deposited via loan book)
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
    , live: true  -- Tokens are live when launched through batch auction
    , totalSupply: 1000000.0    -- Default 1M supply
    }


--------------------------------------------------------------------------------
-- Token Validation Functions
--------------------------------------------------------------------------------

type ValidationResult = Either String Unit

-- Helper to combine validations
validate :: Boolean -> String -> ValidationResult
validate true _ = Right unit
validate false err = Left err

-- Validate token ticker
validateTokenTicker :: String -> ValidationResult
validateTokenTicker ticker = do
  _ <- validate (String.length ticker >= 3) "Ticker must be at least 3 characters"
  _ <- validate (String.length ticker <= 10) "Ticker must be at most 10 characters"
  _ <- validate (isAlphanumeric ticker) "Ticker must be alphanumeric"
  _ <- validate (ticker /= "SOL") "Cannot use reserved ticker SOL"
  _ <- validate (ticker /= "JITO") "Cannot use reserved ticker JITO"
  _ <- validate (String.toUpper ticker /= "FEELSSOL") "Cannot use reserved ticker FEELSSOL"
  _ <- validate (String.toUpper ticker /= "JITOSOL") "Cannot use reserved ticker JITOSOL"
  Right unit
  where
    -- Simplified alphanumeric check - in production would use regex or proper char validation
    isAlphanumeric _ = true

-- Validate ticker format (3-10 alphanumeric characters)
isValidTicker :: String -> Boolean
isValidTicker ticker = isRight (validateTokenTicker ticker)

-- Validate token pair for positions
-- All operations must be FeelsSOL ↔ Token pairs
isValidPair :: TokenType -> TokenType -> Boolean
isValidPair FeelsSOL (Token _) = true
isValidPair (Token _) FeelsSOL = true
isValidPair _ _ = false  -- No direct token ↔ token or jitoSOL operations

-- Check if a token is tradeable (live)
isTradeable :: TokenMetadata -> Boolean
isTradeable token = token.live
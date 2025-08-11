-- Token system supporting the unified lending protocol's asset management.
-- Defines core token types (JitoSOL, FeelsSOL, user tokens, position tokens) and
-- manages token metadata, creation, and validation. All tokens in the system can serve
-- as either lending assets or collateral in the Feels protocol.
-- Handles token launching mechanics and price discovery for user-created assets.
module Token 
  ( -- Newtypes
    FeelsSOLAmount(..)
  , unwrapFeelsSOL
  -- Token types
  , TokenType(..)
  , TokenAmount
  , TokenMetadata
  , TokenCreationParams
  , ValidationResult
  , TokenRegistry
  -- Functions
  , createToken
  , createAndRegisterToken
  , isValidTicker
  , isValidPair
  , isTradeable
  , getTokenByTicker
  , validate
  , validateTokenTicker
  , initTokenRegistry
  , registerToken
  , getTokenFromRegistry
  , getTokenByType
  , getAllTokens
  , getSystemTokens
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), isRight)
import Data.Array (filter, (:), find)
import Data.String as String
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
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

-- Create a token and automatically register it in the global registry
createAndRegisterToken :: TokenRegistry -> TokenCreationParams -> Effect TokenMetadata
createAndRegisterToken registry params = do
  token <- createToken params
  registerToken registry token
  pure token

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

-- Get token by ticker from registry
getTokenByTicker :: String -> Array TokenMetadata -> Maybe TokenMetadata
getTokenByTicker ticker tokens = 
  case filter (\t -> t.ticker == ticker) tokens of
    [token] -> Just token
    _ -> Nothing


--------------------------------------------------------------------------------
-- Global Token Registry
--------------------------------------------------------------------------------

-- Global token registry type
type TokenRegistry = Ref (Array TokenMetadata)

-- Initialize token registry with system tokens
initTokenRegistry :: Effect TokenRegistry
initTokenRegistry = do
  let systemTokens = getSystemTokens
  new systemTokens

-- Get built-in system tokens
getSystemTokens :: Array TokenMetadata
getSystemTokens = 
  [ { id: 1
    , ticker: "JitoSOL"
    , name: "Jito Staked SOL"
    , tokenType: JitoSOL
    , totalSupply: 1000000.0
    , creator: "system"
    , createdAt: 0.0
    , live: true
    }
  , { id: 2
    , ticker: "FeelsSOL"
    , name: "Feels SOL"
    , tokenType: FeelsSOL
    , totalSupply: 1000000.0
    , creator: "system"
    , createdAt: 0.0
    , live: true
    }
  ]

-- Register a new token in the global registry
registerToken :: TokenRegistry -> TokenMetadata -> Effect Unit
registerToken registry token = do
  modify_ (\tokens -> token : tokens) registry

-- Get token from registry by ticker
getTokenFromRegistry :: TokenRegistry -> String -> Effect (Maybe TokenMetadata)
getTokenFromRegistry registry ticker = do
  tokens <- read registry
  pure $ getTokenByTicker ticker tokens

-- Get token from registry by TokenType
getTokenByType :: TokenRegistry -> TokenType -> Effect (Maybe TokenMetadata)
getTokenByType registry tokenType = do
  tokens <- read registry
  pure $ find (\t -> t.tokenType == tokenType) tokens

-- Get all tokens from registry
getAllTokens :: TokenRegistry -> Effect (Array TokenMetadata)
getAllTokens registry = read registry
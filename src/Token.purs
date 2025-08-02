module Token 
  ( TokenType(..)
  , TokenAmount
  , TokenMetadata
  , TokenCreationParams
  , ValidationResult
  , createToken
  , launchToken
  , calculateTokenPrice
  , isValidTicker
  , isValidPair
  , isTradeable
  , getTokenByTicker
  , validate
  , validateTokenTicker
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), isRight)
import Data.String as String
import Effect (Effect)
import FFI (currentTime, generateId)

--------------------------------------------------------------------------------
-- Token Types
--------------------------------------------------------------------------------

-- Core token types in the system
data TokenType 
  = JitoSOL               -- User deposit token (external)
  | SyntheticSOL          -- Internal base currency
  | Token String          -- User-created token with custom ticker
  | PositionToken         -- Represents ownership of any unified position
  
derive instance eqTokenType :: Eq TokenType
derive instance ordTokenType :: Ord TokenType

instance showTokenType :: Show TokenType where
  show JitoSOL = "jitoSOL"
  show SyntheticSOL = "SOL"
  show (Token ticker) = ticker
  show PositionToken = "POSITION"

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
  , creator :: String
  , createdAt :: Number
  , launched :: Boolean              -- True if 100 SyntheticSOL deposited
  , poolBalance :: Number            -- SyntheticSOL in token pool
  , totalSupply :: Number            -- Total supply of token
  , pricePerToken :: Number          -- Current price in SyntheticSOL
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

-- Create a new token (not launched until 100 SyntheticSOL deposited)
createToken :: TokenCreationParams -> Effect TokenMetadata
createToken params = do
  timestamp <- currentTime
  let tokenId = generateId timestamp
  pure 
    { id: tokenId
    , ticker: params.ticker
    , name: params.name
    , creator: params.creator
    , createdAt: timestamp
    , launched: false
    , poolBalance: 0.0
    , totalSupply: 1000000.0  -- Default 1M supply
    , pricePerToken: 0.0001   -- Initial price 0.0001 SyntheticSOL
    }

-- Launch a token by depositing SyntheticSOL
launchToken :: TokenMetadata -> Number -> TokenMetadata
launchToken token depositAmount =
  let newBalance = token.poolBalance + depositAmount
      isLaunched = newBalance >= 100.0
  in token 
    { poolBalance = newBalance
    , launched = isLaunched
    , pricePerToken = if isLaunched 
                      then newBalance / token.totalSupply 
                      else token.pricePerToken
    }

-- Calculate token price based on constant product formula
calculateTokenPrice :: TokenMetadata -> Number
calculateTokenPrice token =
  if token.launched && token.poolBalance > 0.0
  then token.poolBalance / token.totalSupply
  else token.pricePerToken

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
  Right unit
  where
    -- Simplified alphanumeric check - in production would use regex or proper char validation
    isAlphanumeric _ = true

-- Validate ticker format (3-10 alphanumeric characters)
isValidTicker :: String -> Boolean
isValidTicker ticker = isRight (validateTokenTicker ticker)

-- Validate token pair for positions
-- All operations must be Synthetic SOL ↔ Token pairs
isValidPair :: TokenType -> TokenType -> Boolean
isValidPair SyntheticSOL (Token _) = true
isValidPair (Token _) SyntheticSOL = true
isValidPair _ _ = false  -- No direct token ↔ token or jitoSOL operations

-- Check if a token is tradeable (launched)
isTradeable :: TokenMetadata -> Boolean
isTradeable token = token.launched

-- Get token by ticker from registry
getTokenByTicker :: String -> Array TokenMetadata -> Maybe TokenMetadata
getTokenByTicker ticker tokens = 
  case filter (\t -> t.ticker == ticker) tokens of
    [token] -> Just token
    _ -> Nothing
  where
    filter :: forall a. (a -> Boolean) -> Array a -> Array a
    filter _ [] = []
    filter pred xs = [] -- Simplified
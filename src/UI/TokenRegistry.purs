-- | Token registry for managing system and user-created tokens.
-- | Maintains metadata for all tokens available in the protocol.
-- | In production, token metadata would be stored on-chain.
module UI.TokenRegistry
  ( TokenRegistry
  , initTokenRegistry
  , registerToken
  , getTokenFromRegistry
  , getTokenByType
  , getAllTokens
  , getSystemTokens
  , createAndRegisterToken
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array ((:), find, filter)
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Protocol.Token (TokenType(..), TokenMetadata, TokenCreationParams, createToken)

--------------------------------------------------------------------------------
-- Global Token Registry
--------------------------------------------------------------------------------

-- | Global token registry type
type TokenRegistry = Ref (Array TokenMetadata)

-- | Initialize token registry with system tokens
initTokenRegistry :: Effect TokenRegistry
initTokenRegistry = do
  let systemTokens = getSystemTokens
  new systemTokens

-- | System tokens pre-registered in the protocol
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

-- | Register a new token in the global registry
registerToken :: TokenRegistry -> TokenMetadata -> Effect Unit
registerToken registry token = do
  modify_ (\tokens -> token : tokens) registry

-- | Get token from registry by ticker
getTokenFromRegistry :: TokenRegistry -> String -> Effect (Maybe TokenMetadata)
getTokenFromRegistry registry ticker = do
  tokens <- read registry
  pure $ getTokenByTicker ticker tokens

-- | Get token from registry by TokenType
getTokenByType :: TokenRegistry -> TokenType -> Effect (Maybe TokenMetadata)
getTokenByType registry tokenType = do
  tokens <- read registry
  pure $ find (\t -> t.tokenType == tokenType) tokens

-- | Get all tokens from registry
getAllTokens :: TokenRegistry -> Effect (Array TokenMetadata)
getAllTokens registry = read registry

-- | Create a token and automatically register it in the global registry
createAndRegisterToken :: TokenRegistry -> TokenCreationParams -> Effect TokenMetadata
createAndRegisterToken registry params = do
  token <- createToken params
  registerToken registry token
  pure token

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Get token by ticker from array
getTokenByTicker :: String -> Array TokenMetadata -> Maybe TokenMetadata
getTokenByTicker ticker tokens = 
  case filter (\t -> t.ticker == ticker) tokens of
    [token] -> Just token
    _ -> Nothing
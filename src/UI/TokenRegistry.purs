-- | Token registry for managing system and user-created tokens.
-- | Maintains metadata for all tokens available in the protocol.
-- | In production, token metadata would be stored on-chain.
module UI.TokenRegistry
  ( TokenRegistry
  , TokenInfo
  , initTokenRegistry
  , registerToken
  , getTokenFromRegistry
  , getTokenByType
  , getAllTokens
  , getSystemTokens
  , createAndRegisterToken
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:), find, filter, fromFoldable)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, modify_)
import Protocol.Token (TokenType(..), TokenMetadata, TokenCreationParams, FungibleToken, createToken, tokenMetadata)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Global Token Registry
--------------------------------------------------------------------------------

-- | Extended token information for UI/registry purposes
type TokenInfo =
  { id :: Int
  , ticker :: String
  , name :: String
  , tokenType :: TokenType
  , totalSupply :: Number
  , creator :: String
  , createdAt :: Number
  , live :: Boolean
  }

-- | Global token registry type with efficient lookups (Map-based optimization)
type TokenRegistry = Ref TokenRegistryState

-- | Internal registry state with optimized data structures
type TokenRegistryState =
  { tokens :: Array TokenInfo            -- All tokens (for iteration)
  , tickerIndex :: Map String TokenInfo  -- O(log n) ticker lookup
  , typeIndex :: Map TokenType TokenInfo -- O(log n) type lookup
  , nextId :: Int                        -- Auto-incrementing ID
  }

-- | Initialize token registry with system tokens and efficient indexes  
initTokenRegistry :: Effect TokenRegistry
initTokenRegistry = do
  timestamp <- currentTime
  let systemTokens = 
        [ { id: 1
          , ticker: "JitoSOL"
          , name: "Jito Staked SOL"
          , tokenType: JitoSOL
          , totalSupply: 1000000.0
          , creator: "system"
          , createdAt: timestamp
          , live: true
          }
        , { id: 2
          , ticker: "FeelsSOL"
          , name: "Feels SOL"
          , tokenType: FeelsSOL
          , totalSupply: 1000000.0
          , creator: "system"
          , createdAt: timestamp
          , live: true
          }
        ]
      -- Build efficient lookup indexes
      tickerIndex = Map.fromFoldable $ map (\t -> Tuple t.ticker t) systemTokens
      typeIndex = Map.fromFoldable $ map (\t -> Tuple t.tokenType t) systemTokens
      initialState = 
        { tokens: systemTokens
        , tickerIndex: tickerIndex
        , typeIndex: typeIndex
        , nextId: 3 -- Next ID after system tokens
        }
  new initialState

-- | System tokens pre-registered in the protocol
getSystemTokens :: Effect (Array TokenInfo)
getSystemTokens = do
  timestamp <- currentTime
  pure
    [ { id: 1
      , ticker: "JitoSOL"
      , name: "Jito Staked SOL"
      , tokenType: JitoSOL
      , totalSupply: 1000000.0
      , creator: "system"
      , createdAt: timestamp
      , live: true
      }
    , { id: 2
      , ticker: "FeelsSOL"
      , name: "Feels SOL"
      , tokenType: FeelsSOL
      , totalSupply: 1000000.0
      , creator: "system"
      , createdAt: timestamp
      , live: true
      }
    ]

-- | Register a new token in the global registry with efficient indexing
registerToken :: TokenRegistry -> TokenInfo -> Effect Unit
registerToken registry token = do
  modify_ (\state -> 
    { tokens: token : state.tokens
    , tickerIndex: Map.insert token.ticker token state.tickerIndex
    , typeIndex: Map.insert token.tokenType token state.typeIndex
    , nextId: state.nextId + 1
    }) registry

-- | Get token from registry by ticker (O(log n) lookup optimization)
getTokenFromRegistry :: TokenRegistry -> String -> Effect (Maybe TokenInfo)
getTokenFromRegistry registry ticker = do
  state <- read registry
  pure $ Map.lookup ticker state.tickerIndex

-- | Get token from registry by TokenType (O(log n) lookup optimization)
getTokenByType :: TokenRegistry -> TokenType -> Effect (Maybe TokenInfo)
getTokenByType registry tokenType = do
  state <- read registry
  pure $ Map.lookup tokenType state.typeIndex

-- | Get all tokens from registry
getAllTokens :: TokenRegistry -> Effect (Array TokenInfo)
getAllTokens registry = do
  state <- read registry
  pure state.tokens

-- | Create a token and automatically register it in the global registry
createAndRegisterToken :: TokenRegistry -> TokenCreationParams -> Effect (Either String TokenInfo)
createAndRegisterToken registry params = do
  result <- createToken params
  case result of
    Left err -> pure $ Left err
    Right fungibleToken -> do
      -- Get next ID from optimized state
      state <- read registry
      let nextId = state.nextId
          meta = tokenMetadata fungibleToken
          tokenInfo = 
            { id: nextId
            , ticker: params.ticker
            , name: params.name
            , tokenType: Token params.ticker
            , totalSupply: fungibleToken.totalSupply
            , creator: params.creator
            , createdAt: meta.createdAt
            , live: true
            }
      registerToken registry tokenInfo
      pure $ Right tokenInfo

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Get token by ticker from array (legacy helper - consider using Map lookup instead)
getTokenByTicker :: String -> Array TokenInfo -> Maybe TokenInfo
getTokenByTicker ticker tokens = 
  find (\t -> t.ticker == ticker) tokens
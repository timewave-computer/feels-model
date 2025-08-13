-- | State Manager for UI-Protocol State Bridge
-- | Handles queries and updates between UI components and Protocol state
module UI.StateManager
  ( -- Data queries
    getUserTokens
  , getUserPositions  
  , getLenderOffers
  , getProtocolStats
  , getUserBalance
  , getWalletBalances
  -- Chart data
  , getPriceHistory
  -- Protocol commands
  , executeProtocolCommand
  -- Error handling
  , formatError
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Ref (read, write)
import Data.Tuple (Tuple(..))

import UI.ProtocolState (AppRuntime, ProtocolCommand, IndexerQuery(..))
import UI.Query (executeQuery)
import UI.Command (executeCommand)
import Protocol.Common (CommandResult, QueryResult(..), TokenMetadata, Position)
import Protocol.Token (TokenType(..))
import Protocol.Error (ProtocolError)

--------------------------------------------------------------------------------
-- Data Query Functions
--------------------------------------------------------------------------------

-- | Get user's token metadata
getUserTokens :: AppRuntime -> String -> Effect (Array TokenMetadata)
getUserTokens appRuntime user = do
  state <- read appRuntime.state
  result <- executeQuery (GetUserTokens user) state
  pure $ case result of
    Right (TokenList tokens) -> tokens
    _ -> []

-- | Get user's positions  
getUserPositions :: AppRuntime -> String -> Effect (Array Position)
getUserPositions appRuntime user = do
  state <- read appRuntime.state
  result <- executeQuery (GetUserPositions user) state
  pure $ case result of
    Right (PositionList positions) -> positions
    _ -> []

-- | Get lender offers
getLenderOffers :: AppRuntime -> Effect (Array Position)
getLenderOffers appRuntime = do
  state <- read appRuntime.state
  result <- executeQuery GetLenderOffers state
  pure $ case result of
    Right (LenderOfferList offers) -> offers
    _ -> []

-- | Get protocol statistics
getProtocolStats :: AppRuntime -> Effect 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , polBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
getProtocolStats appRuntime = do
  state <- read appRuntime.state
  result <- executeQuery GetSystemStats state
  pure $ case result of
    Right (SystemStatsResult stats) -> stats
    _ -> { totalValueLocked: 0.0, totalUsers: 0, activePositions: 0, liveTokens: 0, totalLenderOffers: 0, polBalance: 0.0, feelsSOLSupply: 0.0, jitoSOLLocked: 0.0 }

-- | Get user balance for specific token
getUserBalance :: AppRuntime -> String -> TokenType -> Effect Number
getUserBalance appRuntime user token = do
  state <- read appRuntime.state
  result <- executeQuery (GetUserBalance user token) state
  pure $ case result of
    Right (Balance balance) -> balance
    _ -> 0.0

-- | Get wallet balances (JitoSOL and FeelsSOL)
getWalletBalances :: AppRuntime -> String -> Effect { jitoSOL :: Number, feelsSOL :: Number }
getWalletBalances appRuntime user = do
  jitoSOL <- getUserBalance appRuntime user JitoSOL
  feelsSOL <- getUserBalance appRuntime user FeelsSOL
  pure { jitoSOL, feelsSOL }

--------------------------------------------------------------------------------
-- Chart Data Functions
--------------------------------------------------------------------------------

-- | Get price history for charts
getPriceHistory :: AppRuntime -> Effect (Array 
    { timestamp :: Number
    , block :: Int
    , price :: Number
    , polValue :: Number
    , tokens :: Array 
        { ticker :: String
        , price :: Number
        , polFloor :: Number
        , live :: Boolean
        }
    })
getPriceHistory appRuntime = do
  state <- read appRuntime.state
  -- Return stored price history, or current state if empty
  case state.priceHistory of
    [] -> pure 
      [ { timestamp: state.timestamp
        , block: state.currentBlock
        , price: state.lastJitoSOLPrice
        , polValue: 1.0
        , tokens: []
        }
      ]
    history -> pure $ map (\h -> 
      { timestamp: h.timestamp
      , block: h.block
      , price: h.price
      , polValue: h.polValue
      , tokens: []
      }) history

--------------------------------------------------------------------------------
-- Command Execution
--------------------------------------------------------------------------------

-- | Execute protocol command with error handling
executeProtocolCommand :: ProtocolCommand -> AppRuntime -> Effect (Either String CommandResult)
executeProtocolCommand cmd appRuntime = do
  state <- read appRuntime.state
  result <- executeCommand cmd state
  case result of
    Left err -> pure $ Left $ formatError err
    Right (Tuple newState cmdResult) -> do
      -- Update state (simplified - no listener notification for now)
      write newState appRuntime.state
      pure $ Right cmdResult

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

-- | Format protocol errors for UI display
formatError :: ProtocolError -> String
formatError error = show error -- Simplified for now
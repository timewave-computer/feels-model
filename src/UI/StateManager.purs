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
import UI.Queries (executeQuery)
import UI.Commands (executeCommand)
import Protocol.Token (TokenType(..), TokenMetadata)
import Protocol.Position (Position)
import Protocol.Common (CommandResult, QueryResult(..))
import Protocol.Errors (ProtocolError)

--------------------------------------------------------------------------------
-- Data Query Functions
--------------------------------------------------------------------------------

-- | Get user's token metadata
getUserTokens :: AppRuntime -> String -> Effect (Array TokenMetadata)
getUserTokens appRuntime user = do
  result <- executeQuery (GetUserTokens user) appRuntime
  pure $ case result of
    Right (TokensResult tokens) -> tokens
    _ -> []

-- | Get user's positions  
getUserPositions :: AppRuntime -> String -> Effect (Array Position)
getUserPositions appRuntime user = do
  result <- executeQuery (GetUserPositions user) appRuntime
  pure $ case result of
    Right (PositionsResult positions) -> positions
    _ -> []

-- | Get lender offers
getLenderOffers :: AppRuntime -> Effect (Array Position)
getLenderOffers appRuntime = do
  result <- executeQuery GetLenderOffers appRuntime
  pure $ case result of
    Right (PositionsResult offers) -> offers
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
  result <- executeQuery GetSystemStats appRuntime
  pure $ case result of
    Right (StatsResult stats) -> stats
    _ -> { totalValueLocked: 0.0, totalUsers: 0, activePositions: 0, liveTokens: 0, totalLenderOffers: 0, polBalance: 0.0, feelsSOLSupply: 0.0, jitoSOLLocked: 0.0 }

-- | Get user balance for specific token
getUserBalance :: AppRuntime -> String -> TokenType -> Effect Number
getUserBalance appRuntime user token = do
  result <- executeQuery (GetUserBalance user token) appRuntime
  pure $ case result of
    Right (BalanceResult balance) -> balance
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
  -- For now, generate mock chart data based on current state
  -- In a real implementation, this would query historical data
  pure 
    [ { timestamp: state.timestamp
      , block: state.currentBlock
      , price: state.lastJitoSOLPrice
      , polValue: 1.0
      , tokens: []
      }
    ]

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
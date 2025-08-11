-- | Gateway-related actions for the Feels Protocol.
-- | This module consolidates gateway and synthetic SOL operations.
module Actions.GatewayActions
  ( enterGateway
  , exitGateway
  , getExchangeRate
  ) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)

-- Import types
import State.Types (AppState)
import Gateway (enterSystem, exitSystem, getOraclePrice)
import POL (contribute)
import Errors (ProtocolError)

--------------------------------------------------------------------------------
-- Gateway Operations
--------------------------------------------------------------------------------

-- | Enter the gateway by converting JitoSOL to FeelsSOL
enterGateway :: String -> Number -> AppState -> Effect (Either ProtocolError { user :: String, feelsSOLMinted :: Number })
enterGateway user jitoAmount state = do
  result <- enterSystem state.gateway user jitoAmount
  case result of
    Left err -> pure $ Left err
    Right txResult -> do
      -- Automatically collect gateway entry fee and contribute to POL
      let feeAmount = jitoAmount * 0.001  -- 0.1% entry fee
      contribute state.polState feeAmount
      
      pure $ Right { user, feelsSOLMinted: txResult.outputAmount.amount }

-- | Exit the gateway by converting FeelsSOL to JitoSOL
exitGateway :: String -> Number -> AppState -> Effect (Either ProtocolError { user :: String, jitoSOLReceived :: Number })
exitGateway user feelsAmount state = do
  result <- exitSystem state.gateway user feelsAmount
  case result of
    Left err -> pure $ Left err
    Right txResult -> do
      -- Automatically collect gateway exit fee and contribute to POL
      let feeAmount = feelsAmount * 0.002  -- 0.2% exit fee
      contribute state.polState feeAmount
      
      pure $ Right { user, jitoSOLReceived: txResult.outputAmount.amount }

-- | Get the current JitoSOL/FeelsSOL exchange rate
getExchangeRate :: AppState -> Effect Number
getExchangeRate state = do
  -- Get the oracle price from the gateway's synthetic SOL state
  priceData <- getOraclePrice state.gateway.syntheticSOL
  pure priceData.price
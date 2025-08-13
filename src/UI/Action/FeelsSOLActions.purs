-- | FeelsSOL-related UI actions.
-- | This module orchestrates FeelsSOL minting and burning operations for the frontend.
module UI.Action.FeelsSOLActions
  ( enterFeelsSOL
  , exitFeelsSOL
  , getExchangeRate
  ) where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.FeelsSOL (enterSystem, exitSystem, getOraclePrice)
import Protocol.POL (contribute)
import Protocol.Error (ProtocolError(..))
import UI.Account (depositFromChain, withdrawToChain, getChainAccountBalance, getFeelsAccountBalance)
import Protocol.Token (TokenType(..))

--------------------------------------------------------------------------------
-- FeelsSOL Operations
--------------------------------------------------------------------------------

-- | Enter the FeelsSOL system by converting JitoSOL to FeelsSOL
enterFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError { user :: String, feelsSOLMinted :: Number })
enterFeelsSOL user jitoAmount state = do
  -- Check user has sufficient JitoSOL
  jitoBalance <- getChainAccountBalance state.accounts user
  if jitoBalance < jitoAmount
    then pure $ Left $ InsufficientBalanceError $ 
      "Insufficient JitoSOL balance. Required: " <> show jitoAmount <> ", Available: " <> show jitoBalance
    else do
      -- Execute the FeelsSOL conversion
      result <- enterSystem state.feelsSOL user jitoAmount
      case result of
        Left err -> pure $ Left err
        Right txResult -> do
          -- Move JitoSOL from chain account to protocol
          depositResult <- depositFromChain state.accounts user jitoAmount
          case depositResult of
            Left err -> pure $ Left $ InvalidCommandError err
            Right _ -> do
              -- POL contribution is calculated by the FeelsSOL system
              let polContribution = txResult.fee * state.feelsSOL.polAllocationRate
              contribute state.polState polContribution
              
              pure $ Right { user, feelsSOLMinted: txResult.feelsSOLMinted }

-- | Exit the FeelsSOL system by converting FeelsSOL to JitoSOL
exitFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError { user :: String, jitoSOLReceived :: Number })
exitFeelsSOL user feelsAmount state = do
  -- Check user has sufficient FeelsSOL
  feelsBalance <- getFeelsAccountBalance state.accounts user FeelsSOL
  if feelsBalance < feelsAmount
    then pure $ Left $ InsufficientBalanceError $ 
      "Insufficient FeelsSOL balance. Required: " <> show feelsAmount <> ", Available: " <> show feelsBalance
    else do
      -- Execute the FeelsSOL conversion
      result <- exitSystem state.feelsSOL user feelsAmount
      case result of
        Left err -> pure $ Left err
        Right txResult -> do
          -- Move FeelsSOL back to JitoSOL in chain account
          withdrawResult <- withdrawToChain state.accounts user txResult.jitoSOLReleased
          case withdrawResult of
            Left err -> pure $ Left $ InvalidCommandError err
            Right _ -> do
              -- POL contribution is calculated by the FeelsSOL system
              let polContribution = txResult.fee * state.feelsSOL.polAllocationRate
              contribute state.polState polContribution
              
              pure $ Right { user, jitoSOLReceived: txResult.jitoSOLReleased }

-- | Get the current JitoSOL/FeelsSOL exchange rate
getExchangeRate :: ProtocolState -> Effect Number
getExchangeRate state = do
  -- Get the oracle price from the FeelsSOL system
  priceData <- getOraclePrice state.feelsSOL
  pure priceData.price
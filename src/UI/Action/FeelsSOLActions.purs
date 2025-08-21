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
import Protocol.FeelsSOLVault (getOraclePrice, depositForFeelsSOL, withdrawFromFeelsSOL)
import Protocol.Token (TokenType(..))
import Protocol.Error (ProtocolError(..))
import UI.Account (depositFromChain, withdrawToChain, getChainAccountBalance, getFeelsAccountBalance)

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
    else if jitoAmount <= 0.0
      then pure $ Left (InvalidAmountError jitoAmount)
      else do
        -- Execute the FeelsSOL conversion through vault
        result <- depositForFeelsSOL state.feelsSOL user jitoAmount state.currentBlock
        case result of
          Left err -> pure $ Left err
          Right feelsSOLMinted -> do
            -- Move JitoSOL from chain account to protocol
            depositResult <- depositFromChain state.accounts user jitoAmount
            case depositResult of
              Left err -> pure $ Left $ InvalidCommandError err
              Right _ -> do
                -- Fee contribution to POL is handled internally by FeelsSOL vault
                pure $ Right { user, feelsSOLMinted }

-- | Exit the FeelsSOL system by converting FeelsSOL to JitoSOL
exitFeelsSOL :: String -> Number -> ProtocolState -> Effect (Either ProtocolError { user :: String, jitoSOLReceived :: Number })
exitFeelsSOL user feelsAmount state = do
  -- Check user has sufficient FeelsSOL
  feelsBalance <- getFeelsAccountBalance state.accounts user FeelsSOL
  if feelsBalance < feelsAmount
    then pure $ Left $ InsufficientBalanceError $ 
      "Insufficient FeelsSOL balance. Required: " <> show feelsAmount <> ", Available: " <> show feelsBalance
    else if feelsAmount <= 0.0
      then pure $ Left (InvalidAmountError feelsAmount)
      else do
        -- Execute the FeelsSOL conversion through vault
        withdrawResult <- withdrawFromFeelsSOL state.feelsSOL user feelsAmount state.currentBlock
        case withdrawResult of
          Left err -> pure $ Left err
          Right jitoSOLReleased -> do
            
            -- Move FeelsSOL back to JitoSOL in chain account
            chainResult <- withdrawToChain state.accounts user jitoSOLReleased
            case chainResult of
              Left err -> pure $ Left $ InvalidCommandError err
              Right _ -> do
                -- Fee contribution to POL is handled internally by FeelsSOL vault
                pure $ Right { user, jitoSOLReceived: jitoSOLReleased }

-- | Get the current JitoSOL/FeelsSOL exchange rate
getExchangeRate :: ProtocolState -> Effect Number
getExchangeRate state = do
  -- Get the oracle price from the vault
  oraclePrice <- getOraclePrice state.feelsSOL
  pure oraclePrice.price
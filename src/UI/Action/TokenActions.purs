-- | Token-related UI actions.
-- | This module orchestrates token creation and management for the frontend.
module UI.Action.TokenActions
  ( createToken
  , validateTokenParams
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (find)
import Effect (Effect)

-- Import types
import UI.ProtocolState (ProtocolState)
import Protocol.Token (TokenMetadata)
import UI.TokenRegistry (TokenRegistry, createAndRegisterToken, getAllTokens)
import Protocol.Error (ProtocolError(..))

--------------------------------------------------------------------------------
-- Token Creation
--------------------------------------------------------------------------------

-- | Validate token creation parameters
validateTokenParams :: String -> String -> TokenRegistry -> Effect (Either ProtocolError Unit)
validateTokenParams ticker name tokenRegistry = do
  if ticker == "" || name == ""
    then pure $ Left $ InvalidCommandError "Ticker and name cannot be empty"
    else do
      -- Check for duplicate ticker or name
      existingTokens <- getAllTokens tokenRegistry
      let duplicateTicker = find (\t -> t.ticker == ticker) existingTokens
          duplicateName = find (\t -> t.name == name) existingTokens
      
      case duplicateTicker of
        Just _ -> pure $ Left $ InvalidCommandError $ "Token with ticker '" <> ticker <> "' already exists"
        Nothing -> case duplicateName of
          Just _ -> pure $ Left $ InvalidCommandError $ "Token with name '" <> name <> "' already exists" 
          Nothing -> pure $ Right unit

-- | Create a new token and set up initial POL
createToken :: String -> String -> String -> ProtocolState -> Effect (Either ProtocolError TokenMetadata)
createToken creator ticker name state = do
  -- Validate parameters
  validationResult <- validateTokenParams ticker name state.tokenRegistry
  case validationResult of
    Left err -> pure $ Left err
    Right _ -> do
      -- Create token
      let tokenParams = { ticker, name, creator }
      newToken <- createAndRegisterToken state.tokenRegistry tokenParams
      
      pure $ Right newToken
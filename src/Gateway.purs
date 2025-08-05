-- Gateway for entering and exiting the Feels Protocol ecosystem.
-- High-level entry/exit functions that handle user balance management, fees, and NFV contributions.
-- Delegates synthetic asset creation/destruction to SyntheticSOL module.
-- This is the sole entry/exit point for users interacting with the protocol.
module Gateway
  ( GatewayState
  , TransformResult
  , TransformDirection(..)
  , initGateway
  , enterSystem
  , exitSystem
  , getExchangeRate
  , getTotalLocked
  , getTotalMinted
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array ((:), find)
import Effect (Effect)
import Effect.Ref (Ref, read, write)
import Token (TokenType(..), TokenAmount)
import FFI (currentTime)
import LendingRecord (LendingTerms(..))
import NFV (NFVState, contributeToNFV)
import SyntheticSOL (SyntheticSOLState, initSyntheticSOL, mintFeelsSOL, burnFeelsSOL, getOraclePrice)

--------------------------------------------------------------------------------
-- Gateway Types
--------------------------------------------------------------------------------

-- Direction of transformation
data TransformDirection
  = Enter  -- JitoSOL -> FeelsSOL
  | Exit   -- FeelsSOL -> JitoSOL

derive instance eqTransformDirection :: Eq TransformDirection

instance showTransformDirection :: Show TransformDirection where
  show Enter = "Enter (JitoSOL -> FeelsSOL)"
  show Exit = "Exit (FeelsSOL -> JitoSOL)"

-- Result of a transformation
type TransformResult =
  { direction :: TransformDirection
  , inputAmount :: TokenAmount
  , outputAmount :: TokenAmount
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- Gateway state for managing system entry/exit
type GatewayState =
  { syntheticSOL :: SyntheticSOLState     -- Synthetic FeelsSOL management
  , entryFee :: Number                    -- Fee for entering (e.g., 0.001 = 0.1%)
  , exitFee :: Number                     -- Fee for exiting (e.g., 0.002 = 0.2%)
  , userBalances :: Ref (Array { owner :: String, token :: TokenType, amount :: Number })
  , nfvAllocationRate :: Number           -- Portion of fees going to NFV (e.g., 0.25 = 25%)
  , nfvState :: NFVState                  -- NFV state for contributions
  }

--------------------------------------------------------------------------------
-- Gateway Initialization
--------------------------------------------------------------------------------

-- Initialize the gateway with oracle and fee parameters
initGateway :: 
  (Effect Number) ->  -- Price oracle function
  Number ->           -- Entry fee
  Number ->           -- Exit fee
  Ref (Array { owner :: String, token :: TokenType, amount :: Number }) ->
  NFVState ->          -- NFV state
  Effect GatewayState
initGateway oracle entryFee exitFee balances nfv = do
  syntheticSOLState <- initSyntheticSOL oracle
  pure { syntheticSOL: syntheticSOLState
       , entryFee: entryFee
       , exitFee: exitFee
       , userBalances: balances
       , nfvAllocationRate: 0.25  -- Default 25% to NFV
       , nfvState: nfv
       }

--------------------------------------------------------------------------------
-- System Entry (JitoSOL -> FeelsSOL)
--------------------------------------------------------------------------------

-- Enter the system by depositing JitoSOL and receiving FeelsSOL
enterSystem :: 
  GatewayState ->
  String ->         -- User address
  Number ->         -- Amount of JitoSOL to deposit
  Effect (Either String TransformResult)
enterSystem state user jitoAmount = do
  -- Validate amount
  if jitoAmount <= 0.0
    then pure $ Left "Amount must be positive"
    else do
      -- Check user has sufficient JitoSOL
      hasBalance <- checkBalance state.userBalances user JitoSOL jitoAmount
      if not hasBalance
        then pure $ Left $ "Insufficient JitoSOL balance. Need " <> show jitoAmount
        else do
          -- Calculate fees
          let feeAmount = jitoAmount * state.entryFee
              netJitoAmount = jitoAmount - feeAmount
          
          -- Mint FeelsSOL using SyntheticSOL module
          mintResult <- mintFeelsSOL state.syntheticSOL netJitoAmount
          case mintResult of
            Left err -> pure $ Left err
            Right result -> do
              -- Calculate NFV contribution from fees
              oraclePrice <- getOraclePrice state.syntheticSOL
              let feeInFeelsSOL = feeAmount * oraclePrice.price
                  nfvContribution = feeInFeelsSOL * state.nfvAllocationRate
              
              -- Update user balances
              _ <- updateBalance state.userBalances user JitoSOL (-jitoAmount)
              _ <- updateBalance state.userBalances user FeelsSOL result.feelsSOLMinted
              
              -- Contribute to NFV
              _ <- contributeToNFV state.nfvState SwapTerms nfvContribution Nothing
              
              -- Return result
              timestamp <- currentTime
              pure $ Right
                { direction: Enter
                , inputAmount: { tokenType: JitoSOL, amount: jitoAmount }
                , outputAmount: { tokenType: FeelsSOL, amount: result.feelsSOLMinted }
                , exchangeRate: result.exchangeRate
                , fee: feeAmount
                , timestamp: timestamp
                }

--------------------------------------------------------------------------------
-- System Exit (FeelsSOL -> JitoSOL)
--------------------------------------------------------------------------------

-- Exit the system by burning FeelsSOL and receiving JitoSOL
exitSystem ::
  GatewayState ->
  String ->         -- User address
  Number ->         -- Amount of FeelsSOL to burn
  Effect (Either String TransformResult)
exitSystem state user feelsAmount = do
  -- Validate amount
  if feelsAmount <= 0.0
    then pure $ Left "Amount must be positive"
    else do
      -- Check user has sufficient FeelsSOL
      hasBalance <- checkBalance state.userBalances user FeelsSOL feelsAmount
      if not hasBalance
        then pure $ Left $ "Insufficient FeelsSOL balance. Need " <> show feelsAmount
        else do
          -- Burn FeelsSOL using SyntheticSOL module
          burnResult <- burnFeelsSOL state.syntheticSOL feelsAmount
          case burnResult of
            Left err -> pure $ Left err
            Right result -> do
              -- Calculate fees on the JitoSOL output
              let feeAmount = result.jitoSOLReleased * state.exitFee
                  jitoSOLAmount = result.jitoSOLReleased - feeAmount
              
              -- Calculate NFV contribution from fees
              oraclePrice <- getOraclePrice state.syntheticSOL
              let feeInFeelsSOL = feeAmount * oraclePrice.price
                  nfvContribution = feeInFeelsSOL * state.nfvAllocationRate
              
              -- Update user balances
              _ <- updateBalance state.userBalances user FeelsSOL (-feelsAmount)
              _ <- updateBalance state.userBalances user JitoSOL jitoSOLAmount
              
              -- Contribute to NFV (exit fees also go to NFV)
              _ <- contributeToNFV state.nfvState SwapTerms nfvContribution Nothing
              
              -- Return result
              timestamp <- currentTime
              pure $ Right
                { direction: Exit
                , inputAmount: { tokenType: FeelsSOL, amount: feelsAmount }
                , outputAmount: { tokenType: JitoSOL, amount: jitoSOLAmount }
                , exchangeRate: result.exchangeRate
                , fee: feeAmount
                , timestamp: timestamp
                }

--------------------------------------------------------------------------------
-- Gateway Information
--------------------------------------------------------------------------------

-- Get current exchange rate
getExchangeRate :: GatewayState -> Effect Number
getExchangeRate state = do
  oraclePrice <- getOraclePrice state.syntheticSOL
  pure oraclePrice.price

-- Get total JitoSOL locked in the gateway
getTotalLocked :: GatewayState -> Effect Number
getTotalLocked state = read state.syntheticSOL.totalJitoSOLBacking

-- Get total FeelsSOL minted
getTotalMinted :: GatewayState -> Effect Number
getTotalMinted state = read state.syntheticSOL.totalFeelsSOLSupply

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Check if user has sufficient balance
checkBalance :: 
  Ref (Array { owner :: String, token :: TokenType, amount :: Number }) -> 
  String -> TokenType -> Number -> Effect Boolean
checkBalance balancesRef owner token requiredAmount = do
  balances <- read balancesRef
  case find (\b -> b.owner == owner && b.token == token) balances of
    Just balance -> pure (balance.amount >= requiredAmount)
    Nothing -> pure false

-- Update user balance
updateBalance :: 
  Ref (Array { owner :: String, token :: TokenType, amount :: Number }) -> 
  String -> TokenType -> Number -> Effect Unit
updateBalance balancesRef owner token delta = do
  balances <- read balancesRef
  let updated = updateBalanceArray balances owner token delta
  write updated balancesRef

-- Helper to update balance array
updateBalanceArray :: 
  Array { owner :: String, token :: TokenType, amount :: Number } ->
  String -> TokenType -> Number -> 
  Array { owner :: String, token :: TokenType, amount :: Number }
updateBalanceArray balances owner token delta =
  case find (\b -> b.owner == owner && b.token == token) balances of
    Just _ ->
      map (\b -> if b.owner == owner && b.token == token 
                 then b { amount = b.amount + delta }
                 else b) balances
    Nothing ->
      if delta > 0.0
      then { owner: owner, token: token, amount: delta } : balances
      else balances

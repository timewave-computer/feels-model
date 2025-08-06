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
import POL (POLState, contributeToPOL)
import SyntheticSOL (SyntheticSOLState, initSyntheticSOL, mintFeelsSOL, burnFeelsSOL, getOraclePrice)
import Accounts (checkAccountBalance, updateAccount)
import ProtocolError (ProtocolError(..))

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
  , userAccounts :: Ref (Array { owner :: String, token :: TokenType, amount :: Number })
  , polAllocationRate :: Number           -- Portion of fees going to POL (e.g., 0.25 = 25%)
  , polState :: POLState                  -- POL state for contributions
  }

--------------------------------------------------------------------------------
-- Gateway Initialization
--------------------------------------------------------------------------------

-- Initialize the gateway with oracle and fee parameters
initGateway :: 
  (Effect Number) ->  -- Price oracle function
  Number ->           -- Entry fee
  Number ->           -- Exit fee
  Ref (Array { owner :: String, token :: TokenType, amount :: Number }) -> -- User accounts registry
  POLState ->          -- POL state
  Effect GatewayState
initGateway oracle entryFee exitFee accounts pol = do
  syntheticSOLState <- initSyntheticSOL oracle
  pure { syntheticSOL: syntheticSOLState
       , entryFee: entryFee
       , exitFee: exitFee
       , userAccounts: accounts
       , polAllocationRate: 0.25  -- Default 25% to POL
       , polState: pol
       }

--------------------------------------------------------------------------------
-- System Entry (JitoSOL -> FeelsSOL)
--------------------------------------------------------------------------------

-- Enter the system by depositing JitoSOL and receiving FeelsSOL
enterSystem :: 
  GatewayState ->
  String ->         -- User address
  Number ->         -- Amount of JitoSOL to deposit
  Effect (Either ProtocolError TransformResult)
enterSystem state user jitoAmount = do
  -- Validate amount
  if jitoAmount <= 0.0
    then pure $ Left (InvalidAmountError jitoAmount)
    else do
      -- Check user has sufficient JitoSOL
      hasBalance <- checkAccountBalance state.userAccounts user JitoSOL jitoAmount
      if not hasBalance
        then pure $ Left (InsufficientBalanceError $ "Insufficient JitoSOL balance. Need " <> show jitoAmount)
        else do
          -- Calculate fees
          let feeAmount = jitoAmount * state.entryFee
              netJitoAmount = jitoAmount - feeAmount
          
          -- Mint FeelsSOL using SyntheticSOL module
          mintResult <- mintFeelsSOL state.syntheticSOL netJitoAmount
          case mintResult of
            Left err -> pure $ Left (SystemError err)
            Right result -> do
              -- Calculate POL contribution from fees
              oraclePrice <- getOraclePrice state.syntheticSOL
              let feeInFeelsSOL = feeAmount * oraclePrice.price
                  polContribution = feeInFeelsSOL * state.polAllocationRate
              
              -- Update user balances
              _ <- updateAccount state.userAccounts user JitoSOL (-jitoAmount)
              _ <- updateAccount state.userAccounts user FeelsSOL result.feelsSOLMinted
              
              -- Contribute to POL
              _ <- contributeToPOL state.polState SwapTerms polContribution Nothing
              
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
  Effect (Either ProtocolError TransformResult)
exitSystem state user feelsAmount = do
  -- Validate amount
  if feelsAmount <= 0.0
    then pure $ Left (InvalidAmountError feelsAmount)
    else do
      -- Check user has sufficient FeelsSOL
      hasBalance <- checkAccountBalance state.userAccounts user FeelsSOL feelsAmount
      if not hasBalance
        then pure $ Left (InsufficientBalanceError $ "Insufficient FeelsSOL balance. Need " <> show feelsAmount)
        else do
          -- Burn FeelsSOL using SyntheticSOL module
          burnResult <- burnFeelsSOL state.syntheticSOL feelsAmount
          case burnResult of
            Left err -> pure $ Left (SystemError err)
            Right result -> do
              -- Calculate fees on the JitoSOL output
              let feeAmount = result.jitoSOLReleased * state.exitFee
                  jitoSOLAmount = result.jitoSOLReleased - feeAmount
              
              -- Calculate POL contribution from fees
              oraclePrice <- getOraclePrice state.syntheticSOL
              let feeInFeelsSOL = feeAmount * oraclePrice.price
                  polContribution = feeInFeelsSOL * state.polAllocationRate
              
              -- Update user balances
              _ <- updateAccount state.userAccounts user FeelsSOL (-feelsAmount)
              _ <- updateAccount state.userAccounts user JitoSOL jitoSOLAmount
              
              -- Contribute to POL (exit fees also go to POL)
              _ <- contributeToPOL state.polState SwapTerms polContribution Nothing
              
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
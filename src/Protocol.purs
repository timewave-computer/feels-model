-- Unified protocol state management providing a clean API for the frontend.
-- Centralizes all state operations and ensures consistency across the system.
module Protocol
  ( ProtocolState
  , ProtocolRuntime
  , ProtocolCommand(..)
  , ProtocolQuery(..)
  , ProtocolResult(..)
  , ProtocolError(..)
  , initProtocol
  , executeCommand
  , executeQuery
  , subscribe
  , unsubscribe
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array ((:), filter, find, length, nub)
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Data.Show (show)
import Control.Monad (when)

-- Import existing types
import Token (TokenType(..), TokenMetadata, TokenRegistry, createAndRegisterToken, getTokenFromRegistry, initTokenRegistry, getAllTokens)
import LendingBook (LendingBook, initLendingBook, createLendOffer, getUserRecords, getBorrowerRecords, getLenderRecords, getActiveRecords, takeLoan)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), createLenderRecord, createBorrowerRecord)
import Gateway (GatewayState, initGateway, enterSystem, exitSystem)
import NFV (NFVState, initNFV, getNFVBalance, getNFVMetrics, contributeToTokenNFV, createNFVTicks)
import Oracle (Oracle, initOracle)
import Incentives (MarketDynamics, initMarketDynamics)
import BalanceManager (BalanceRegistry)
import FFI (currentTime, generateRecordId)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Complete protocol state
type ProtocolState =
  { tokenRegistry :: TokenRegistry
  , lendingBook :: LendingBook
  , gateway :: GatewayState
  , nfvState :: NFVState
  , oracle :: Oracle
  , marketDynamics :: MarketDynamics
  , balances :: BalanceRegistry
  , positionTokenMap :: Array { positionId :: Int, tokenTicker :: String }
  , currentUser :: String  -- For demo purposes, in production would be wallet-based
  , timestamp :: Number
  }

-- Runtime wrapper for protocol state
type ProtocolRuntime =
  { state :: Ref ProtocolState
  , listeners :: Ref (Array { id :: Int, callback :: ProtocolState -> Effect Unit })
  , nextListenerId :: Ref Int
  }

-- Commands that modify state
data ProtocolCommand
  = CreateToken String String String  -- creator, ticker, name
  | CreateLendingPosition String TokenType Number TokenType Number LendingTerms (Maybe String)  -- user, lendAsset, amount, collateralAsset, collateralAmount, terms, targetToken
  | TransferTokens String String TokenType Number  -- from, to, token, amount
  | EnterGateway String Number  -- user, jitoSOLAmount
  | ExitGateway String Number   -- user, feelsSOLAmount
  | InitiateUnbonding String Int  -- user, positionId
  | WithdrawPosition String Int   -- user, positionId

-- Queries that read state
data ProtocolQuery
  = GetUserTokens String
  | GetAllTokens
  | GetUserPositions String
  | GetUserBalance String TokenType
  | GetTokenByTicker String
  | GetLenderOffers
  | GetProtocolStats
  | GetNFVMetrics
  | GetPositionTargetToken Int

-- Results from operations
data ProtocolResult
  = TokenCreated TokenMetadata
  | PositionCreated LendingRecord
  | TokensTransferred { from :: String, to :: String, token :: TokenType, amount :: Number }
  | GatewayEntered { user :: String, feelsSOLMinted :: Number }
  | GatewayExited { user :: String, jitoSOLReceived :: Number }
  | UnbondingInitiated Int
  | PositionWithdrawn Int
  | TokenList (Array TokenMetadata)
  | PositionList (Array LendingRecord)
  | Balance Number
  | TokenInfo (Maybe TokenMetadata)
  | LenderOfferList (Array LendingRecord)
  | ProtocolStatsResult 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , nfvBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
  | NFVMetricsResult 
    { balance :: Number
    , growthRate24h :: Number
    , utilizationRate :: Number
    }
  | TargetTokenInfo (Maybe String)

-- Errors
data ProtocolError
  = InvalidCommand String
  | InsufficientBalance String
  | TokenNotFound String
  | PositionNotFound Int
  | UserNotFound String
  | InvalidAmount Number
  | SystemError String

derive instance eqProtocolError :: Eq ProtocolError

instance showProtocolError :: Show ProtocolError where
  show (InvalidCommand msg) = "Invalid command: " <> msg
  show (InsufficientBalance msg) = "Insufficient balance: " <> msg
  show (TokenNotFound ticker) = "Token not found: " <> ticker
  show (PositionNotFound id) = "Position not found: " <> show id
  show (UserNotFound user) = "User not found: " <> user
  show (InvalidAmount amount) = "Invalid amount: " <> show amount
  show (SystemError msg) = "System error: " <> msg

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize protocol with empty state
initProtocol :: Effect ProtocolRuntime
initProtocol = do
  -- Initialize all subsystems
  tokenRegistry <- initTokenRegistry
  lendingBook <- initLendingBook
  nfvState <- initNFV
  oracle <- initOracle lendingBook nfvState
  marketDynamics <- initMarketDynamics oracle nfvState
  
  -- Initialize balances with some demo data for the user
  balances <- new 
    [ { owner: "main-user", token: JitoSOL, amount: 5000.0 }
    , { owner: "main-user", token: FeelsSOL, amount: 5000.0 }
    ]
  
  -- Initialize gateway
  let priceOracle = pure 1.05  -- JitoSOL/SOL = 1.05
  gateway <- initGateway priceOracle 0.001 0.002 balances nfvState
  
  -- Initialize NFV for system tokens only (JitoSOL and FeelsSOL)
  -- These are the base tokens that exist from the start
  contributeToTokenNFV nfvState JitoSOL 100.0    -- Initial 100 FeelsSOL NFV for JitoSOL
  contributeToTokenNFV nfvState FeelsSOL 50.0     -- Initial 50 FeelsSOL NFV for FeelsSOL
  
  -- Deploy initial NFV liquidity for system tokens
  _ <- createNFVTicks nfvState lendingBook
  
  -- Note: No demo trades - the market starts clean
  -- User tokens will deploy their own NFV liquidity when created
  
  -- Get current timestamp
  timestamp <- currentTime
  
  -- Create initial state
  let initialState =
        { tokenRegistry
        , lendingBook
        , gateway
        , nfvState
        , oracle
        , marketDynamics
        , balances
        , positionTokenMap: []
        , currentUser: "user1"
        , timestamp
        }
  
  stateRef <- new initialState
  listenersRef <- new []
  nextListenerIdRef <- new 0
  
  pure { state: stateRef, listeners: listenersRef, nextListenerId: nextListenerIdRef }

--------------------------------------------------------------------------------
-- Command Processing
--------------------------------------------------------------------------------

-- Execute a command that modifies state
executeCommand :: ProtocolRuntime -> ProtocolCommand -> Effect (Either ProtocolError ProtocolResult)
executeCommand runtime cmd = do
  state <- read runtime.state
  result <- processCommand state cmd
  
  case result of
    Right (Tuple newState cmdResult) -> do
      -- Update state
      write newState runtime.state
      
      -- Notify listeners
      listeners <- read runtime.listeners
      _ <- traverse (\l -> l.callback newState) listeners
      
      pure $ Right cmdResult
    Left err -> pure $ Left err

-- Process a command and return new state
processCommand :: ProtocolState -> ProtocolCommand -> Effect (Either ProtocolError (Tuple ProtocolState ProtocolResult))
processCommand state = case _ of
  CreateToken creator ticker name -> do
    -- Validate inputs
    if ticker == "" || name == ""
      then pure $ Left $ InvalidCommand "Ticker and name cannot be empty"
      else do
        -- Check for duplicate ticker or name
        existingTokens <- getAllTokens state.tokenRegistry
        let duplicateTicker = find (\t -> t.ticker == ticker) existingTokens
            duplicateName = find (\t -> t.name == name) existingTokens
        
        case duplicateTicker of
          Just _ -> pure $ Left $ InvalidCommand $ "Token with ticker '" <> ticker <> "' already exists"
          Nothing -> case duplicateName of
            Just _ -> pure $ Left $ InvalidCommand $ "Token with name '" <> name <> "' already exists" 
            Nothing -> do
              -- Create token
              let tokenParams = { ticker, name, creator }
              newToken <- createAndRegisterToken state.tokenRegistry tokenParams
              
              -- Contribute initial NFV for the new token (simulating launch staking)
              -- This represents the initial liquidity provided during token launch
              let initialLiquidity = 100.0  -- 100 FeelsSOL initial liquidity
              contributeToTokenNFV state.nfvState (Token ticker) initialLiquidity
              
              -- Deploy NFV liquidity for the new token
              -- This creates automated market maker offers across different price ranges
              _ <- createNFVTicks state.nfvState state.lendingBook
              
              -- Update timestamp
              timestamp <- currentTime
              let newState = state { timestamp = timestamp }
              
              pure $ Right $ Tuple newState (TokenCreated newToken)
  
  CreateLendingPosition user lendAsset amount collateralAsset collateralAmount terms targetToken -> do
    -- Validate amount
    if amount <= 0.0
      then pure $ Left $ InvalidAmount amount
      else do
        -- Create lending record
        id <- generateRecordId
        timestamp <- currentTime
        
        let record = case terms of
              StakingTerms period -> 
                createLenderRecord id user lendAsset amount collateralAsset 
                  (collateralAmount / amount) terms timestamp
              _ -> 
                createBorrowerRecord id user lendAsset amount collateralAsset 
                  collateralAmount terms timestamp
        
        -- Add to lending book
        result <- createLendOffer state.lendingBook user lendAsset amount 
                    collateralAsset collateralAmount terms
        
        case result of
          Left err -> pure $ Left $ SystemError err
          Right lendingRecord -> do
            -- Automatically collect protocol fees and contribute to NFV
            let feeRate = case terms of
                  StakingTerms _ -> 0.0005  -- 0.05% for staking
                  LeverageTerms _ -> 0.001   -- 0.1% for leverage
                  SwapTerms -> 0.002         -- 0.2% for swaps
                feeAmount = amount * feeRate
                -- Contribute to the collateral asset's NFV (or lend asset if collateral is FeelsSOL)
                tokenForNFV = if collateralAsset /= FeelsSOL then collateralAsset else lendAsset
            
            -- Contribute fee to token-specific NFV
            contributeToTokenNFV state.nfvState tokenForNFV feeAmount
            
            -- Update position-token mapping if target token specified
            let newMapping = case targetToken of
                  Just ticker -> { positionId: lendingRecord.id, tokenTicker: ticker } : state.positionTokenMap
                  Nothing -> state.positionTokenMap
            
            -- Update token staking if this is a staking position with target token
            case terms, targetToken of
              StakingTerms _, Just ticker -> do
                -- Find and update the token's staked amount
                maybeToken <- getTokenFromRegistry state.tokenRegistry ticker
                case maybeToken of
                  Just token -> do
                    let newStakedAmount = token.stakedFeelsSOL + amount
                        shouldGoLive = newStakedAmount >= 100.0 && not token.live
                    
                    -- If token is going live, contribute initial NFV
                    when shouldGoLive $ do
                      -- Contribute 10% of launch threshold to NFV as initial floor
                      let initialNFV = 10.0  -- 10 FeelsSOL (10% of 100)
                      contributeToTokenNFV state.nfvState (Token ticker) initialNFV
                    
                    -- TODO: Update token in registry with new staked amount and live status
                    pure unit
                  Nothing -> pure unit
              _, _ -> pure unit
            
            let newState = state 
                  { positionTokenMap = newMapping
                  , timestamp = timestamp 
                  }
            
            pure $ Right $ Tuple newState (PositionCreated lendingRecord)
  
  EnterGateway user jitoAmount -> do
    result <- enterSystem state.gateway user jitoAmount
    case result of
      Left err -> pure $ Left $ SystemError err
      Right txResult -> do
        -- Automatically collect gateway entry fee and contribute to JitoSOL NFV
        let feeAmount = jitoAmount * 0.001  -- 0.1% entry fee
        contributeToTokenNFV state.nfvState JitoSOL feeAmount
        
        timestamp <- currentTime
        let newState = state { timestamp = timestamp }
        pure $ Right $ Tuple newState (GatewayEntered 
          { user, feelsSOLMinted: txResult.outputAmount.amount })
  
  ExitGateway user feelsAmount -> do
    result <- exitSystem state.gateway user feelsAmount
    case result of
      Left err -> pure $ Left $ SystemError err
      Right txResult -> do
        -- Automatically collect gateway exit fee and contribute to JitoSOL NFV
        let feeAmount = feelsAmount * 0.002  -- 0.2% exit fee
        contributeToTokenNFV state.nfvState JitoSOL feeAmount
        
        timestamp <- currentTime
        let newState = state { timestamp = timestamp }
        pure $ Right $ Tuple newState (GatewayExited 
          { user, jitoSOLReceived: txResult.outputAmount.amount })
  
  _ -> pure $ Left $ InvalidCommand "Command not implemented"

--------------------------------------------------------------------------------
-- Query Processing
--------------------------------------------------------------------------------

-- Execute a query that reads state
executeQuery :: ProtocolRuntime -> ProtocolQuery -> Effect (Either ProtocolError ProtocolResult)
executeQuery runtime query = do
  state <- read runtime.state
  processQuery state query

-- Process a query and return result
processQuery :: ProtocolState -> ProtocolQuery -> Effect (Either ProtocolError ProtocolResult)
processQuery state = case _ of
  GetUserTokens user -> do
    -- In a real implementation, we'd filter tokens by owner
    -- For now, return empty array as we don't track ownership in TokenRegistry
    pure $ Right $ TokenList []
  
  GetAllTokens -> do
    -- Get all tokens from the registry
    allTokens <- read state.tokenRegistry
    pure $ Right $ TokenList allTokens
  
  GetUserPositions user -> do
    -- Get all user records (both lender and borrower positions)
    positions <- getUserRecords state.lendingBook user
    pure $ Right $ PositionList positions
  
  GetUserBalance user tokenType -> do
    balances <- read state.balances
    let balance = case find (\b -> b.owner == user && b.token == tokenType) balances of
          Just b -> b.amount
          Nothing -> 0.0
    pure $ Right $ Balance balance
  
  GetLenderOffers -> do
    offers <- getLenderRecords state.lendingBook
    pure $ Right $ LenderOfferList offers
  
  GetProtocolStats -> do
    activeRecords <- getActiveRecords state.lendingBook
    lenderRecords <- getLenderRecords state.lendingBook
    let allRecords = activeRecords <> lenderRecords
    -- Calculate total value locked from all lending amounts
    let totalValueLocked = sum (map (\r -> r.lendAmount) allRecords)
    -- Count unique users from all records
    let uniqueUsers = nub (map (\r -> r.owner) allRecords)
    let userCount = if length uniqueUsers == 0 then 1 else length uniqueUsers
    -- Count live tokens from token registry
    tokenList <- read state.tokenRegistry
    let liveCount = length (filter (\t -> t.live) tokenList)
    -- Get NFV balance
    nfvBalance <- getNFVBalance state.nfvState
    -- Get total lender offers
    let totalLenderOffers = length lenderRecords
    -- Calculate FeelsSOL supply and JitoSOL locked
    balances <- read state.balances
    let feelsSOLSupply = sum $ map (\b -> if b.token == FeelsSOL then b.amount else 0.0) balances
        jitoSOLLocked = sum $ map (\b -> if b.token == JitoSOL then b.amount else 0.0) balances
    pure $ Right $ ProtocolStatsResult
      { totalValueLocked: totalValueLocked  
      , totalUsers: userCount  
      , activePositions: length activeRecords
      , liveTokens: liveCount
      , totalLenderOffers: totalLenderOffers
      , nfvBalance: nfvBalance
      , feelsSOLSupply: feelsSOLSupply
      , jitoSOLLocked: jitoSOLLocked
      }
  
  GetNFVMetrics -> do
    balance <- getNFVBalance state.nfvState
    metrics <- getNFVMetrics state.nfvState
    pure $ Right $ NFVMetricsResult
      { balance
      , growthRate24h: metrics.growthRate24h
      , utilizationRate: metrics.utilizationRate
      }
  
  GetPositionTargetToken positionId -> do
    let targetToken = case find (\m -> m.positionId == positionId) state.positionTokenMap of
          Just mapping -> Just mapping.tokenTicker
          Nothing -> Nothing
    pure $ Right $ TargetTokenInfo targetToken
  
  _ -> pure $ Left $ InvalidCommand "Query not implemented"

--------------------------------------------------------------------------------
-- Event Subscription
--------------------------------------------------------------------------------

-- Subscribe to state changes
subscribe :: ProtocolRuntime -> (ProtocolState -> Effect Unit) -> Effect Int
subscribe runtime callback = do
  id <- read runtime.nextListenerId
  _ <- write (id + 1) runtime.nextListenerId
  
  _ <- modify_ (\listeners -> { id, callback } : listeners) runtime.listeners
  
  pure id

-- Unsubscribe from state changes
unsubscribe :: ProtocolRuntime -> Int -> Effect Unit
unsubscribe runtime listenerId = do
  _ <- modify_ (filter (\l -> l.id /= listenerId)) runtime.listeners
  pure unit
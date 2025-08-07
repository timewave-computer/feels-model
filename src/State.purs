module State
  ( AppState
  , AppRuntime
  , AppCommand(..)
  , AppQuery(..)
  , AppResult(..)
  , initState
  , executeCommand
  , executeQuery
  , captureRebaseDifferential
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
import LendingBook (LendingBook, initLendingBook, getUserPositions, getActivePositions)
import LendingBook as LB
import Position (TermCommitment(..), LeverageMode(..), BandTier(..), PriceStrategy(..), TrackingMode(..), createPosition)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), createLenderRecord, createBorrowerRecord)
import Gateway (GatewayState, initGateway, enterSystem, exitSystem)
import SyntheticSOL (getTotalSupply)
import POL (POLState, initPOL, getPOLBalance, getPOLMetrics, getUtilizationRate, contributeToTokenPOL, createPOLTicks, captureStakingRewards)
import Oracle (Oracle, initOracle, takeMarketSnapshot)
import Incentives (MarketDynamics, initMarketDynamics)
import Accounts (AccountRegistry, initAccountRegistry, getFeelsAccountBalance, getAllFeelsAccountBalances, getTotalTokenBalance)
import FFI (currentTime, generateRecordId)
import ProtocolError (ProtocolError(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Complete application state
type AppState =
  { tokenRegistry :: TokenRegistry
  , lendingBook :: LendingBook
  , gateway :: GatewayState
  , polState :: POLState
  , oracle :: Oracle
  , marketDynamics :: MarketDynamics
  , accounts :: AccountRegistry
  , positionTokenMap :: Array { positionId :: Int, tokenTicker :: String }
  , currentUser :: String  -- For demo purposes, in production would be wallet-based
  , timestamp :: Number
  , lastJitoSOLPrice :: Number  -- Track JitoSOL price for rebase capture
  }

-- Runtime wrapper for application state
type AppRuntime =
  { state :: Ref AppState
  , listeners :: Ref (Array { id :: Int, callback :: AppState -> Effect Unit })
  , nextListenerId :: Ref Int
  }

-- Commands that modify state
data AppCommand
  = CreateToken String String String  -- creator, ticker, name
  | CreateLendingPosition String TokenType Number TokenType Number LendingTerms (Maybe String)  -- user, lendAsset, amount, collateralAsset, collateralAmount, terms, targetToken
  | TransferTokens String String TokenType Number  -- from, to, token, amount
  | EnterGateway String Number  -- user, jitoSOLAmount
  | ExitGateway String Number   -- user, feelsSOLAmount
  | InitiateUnbonding String Int  -- user, positionId
  | WithdrawPosition String Int   -- user, positionId

-- Queries that read state
data AppQuery
  = GetUserTokens String
  | GetAllTokens
  | GetUserPositions String
  | GetUserBalance String TokenType
  | GetTokenByTicker String
  | GetLenderOffers
  | GetSystemStats
  | GetPOLMetrics
  | GetPositionTargetToken Int

-- Results from operations
data AppResult
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
  | SystemStatsResult 
    { totalValueLocked :: Number
    , totalUsers :: Int
    , activePositions :: Int
    , liveTokens :: Int
    , totalLenderOffers :: Int
    , polBalance :: Number
    , feelsSOLSupply :: Number
    , jitoSOLLocked :: Number
    }
  | POLMetricsResult 
    { balance :: Number
    , growthRate24h :: Number
    , utilizationRate :: Number
    }
  | TargetTokenInfo (Maybe String)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize state with empty state
initState :: Effect AppRuntime
initState = do
  -- Initialize all subsystems
  tokenRegistry <- initTokenRegistry
  lendingBook <- initLendingBook
  polState <- initPOL
  oracle <- initOracle
  marketDynamics <- initMarketDynamics oracle polState
  
  -- Initialize accounts with some demo data for the user
  accounts <- initAccountRegistry
  
  -- Initialize gateway
  let priceOracle = pure 1.22  -- JitoSOL/SOL = 1.22 (current market price)
  gateway <- initGateway priceOracle 0.001 0.002 accounts polState
  
  -- Initialize POL for system tokens only (JitoSOL and FeelsSOL)
  -- These are the base tokens that exist from the start
  contributeToTokenPOL polState JitoSOL 100.0    -- Initial 100 FeelsSOL POL for JitoSOL
  contributeToTokenPOL polState FeelsSOL 50.0     -- Initial 50 FeelsSOL POL for FeelsSOL
  
  -- Deploy initial POL liquidity for system tokens
  _ <- createPOLTicks polState lendingBook
  
  -- Note: No demo trades - the market starts clean
  -- User tokens will deploy their own POL liquidity when created
  
  -- Get current timestamp
  timestamp <- currentTime
  
  -- Create initial state
  let initialState =
        { tokenRegistry
        , lendingBook
        , gateway
        , polState
        , oracle
        , marketDynamics
        , accounts
        , positionTokenMap: []
        , currentUser: "user1"
        , timestamp
        , lastJitoSOLPrice: 1.22  -- Initial JitoSOL price
        }
  
  stateRef <- new initialState
  listenersRef <- new []
  nextListenerIdRef <- new 0
  
  pure { state: stateRef, listeners: listenersRef, nextListenerId: nextListenerIdRef }

--------------------------------------------------------------------------------
-- Rebase Capture
--------------------------------------------------------------------------------

-- Capture JitoSOL/FeelsSOL rebase differential for POL
-- Called by the Clock's GatewayRebase event
captureRebaseDifferential :: AppRuntime -> Effect Unit
captureRebaseDifferential runtime = do
  state <- read runtime.state
  
  -- Get current JitoSOL price from oracle
  priceObservations <- takeMarketSnapshot state.oracle
  let jitoObs = find (\obs -> obs.tokenPair.base == JitoSOL) priceObservations
  
  case jitoObs of
    Just obs -> do
      let currentJitoPrice = obs.price
          previousPrice = state.lastJitoSOLPrice
          
      -- Only capture if price has increased (staking rewards)
      when (currentJitoPrice > previousPrice) $ do
        -- Get total FeelsSOL supply from gateway
        totalSupply <- getTotalSupply state.gateway.syntheticSOL
        
        -- Calculate differential value
        let priceAppreciation = currentJitoPrice - previousPrice
            differentialValue = totalSupply * priceAppreciation
        
        -- Capture to POL
        when (differentialValue > 0.0) $ do
          captureStakingRewards state.polState totalSupply currentJitoPrice
          
          -- Update state with new price
          let newState = state { lastJitoSOLPrice = currentJitoPrice }
          write newState runtime.state
          
          -- Notify listeners
          listeners <- read runtime.listeners
          _ <- traverse (\l -> l.callback newState) listeners
          pure unit
    
    Nothing -> pure unit  -- No JitoSOL price data

--------------------------------------------------------------------------------
-- Command Processing
--------------------------------------------------------------------------------

-- Define a record of command handlers
commandHandlers :: AppState -> AppCommand -> Effect (Either ProtocolError (Tuple AppState AppResult))
commandHandlers state = case _ of
  CreateToken creator ticker name -> do
    -- Validate inputs
    if ticker == "" || name == ""
      then pure $ Left $ InvalidCommandError "Ticker and name cannot be empty"
      else do
        -- Check for duplicate ticker or name
        existingTokens <- getAllTokens state.tokenRegistry
        let duplicateTicker = find (\t -> t.ticker == ticker) existingTokens
            duplicateName = find (\t -> t.name == name) existingTokens
        
        case duplicateTicker of
          Just _ -> pure $ Left $ InvalidCommandError $ "Token with ticker '" <> ticker <> "' already exists"
          Nothing -> case duplicateName of
            Just _ -> pure $ Left $ InvalidCommandError $ "Token with name '" <> name <> "' already exists" 
            Nothing -> do
              -- Create token
              let tokenParams = { ticker, name, creator }
              newToken <- createAndRegisterToken state.tokenRegistry tokenParams
              
              -- Contribute initial POL for the new token (simulating launch staking)
              -- This represents the initial liquidity provided during token launch
              let initialLiquidity = 100.0  -- 100 FeelsSOL initial liquidity
              contributeToTokenPOL state.polState (Token ticker) initialLiquidity
              
              -- Deploy POL liquidity for the new token
              -- This creates automated market maker offers across different price ranges
              _ <- createPOLTicks state.polState state.lendingBook
              
              -- Update timestamp
              timestamp <- currentTime
              let newState = state { timestamp = timestamp }
              
              pure $ Right $ Tuple newState (TokenCreated newToken)
  
  CreateLendingPosition user lendAsset amount collateralAsset collateralAmount terms targetToken -> do
    -- Validate amount
    if amount <= 0.0
      then pure $ Left $ InvalidAmountError amount
      else do
        timestamp <- currentTime
        
        -- Get next position ID
        nextId <- LB.getNextId state.lendingBook
        
        -- Create a position in the new system
        let position = createPosition 
              nextId
              user
              amount
              { base: lendAsset, quote: collateralAsset }
              (BandAligned 
                { tier: MediumBand
                , centerTracking: SpotPrice
                , adaptiveWidth: false
                , lastUpdate: timestamp
                , cachedBounds: Nothing
                })
              (case terms of
                SwapTerms -> Spot
                StakingTerms period -> Daily 30.0  -- Map to daily term
                LeverageTerms _ -> Spot)
              { mode: case terms of
                  LeverageTerms lev -> Static
                  _ -> Static
              , targetLeverage: case terms of
                  LeverageTerms lev -> lev
                  _ -> 1.0
              , currentLeverage: 1.0
              , decayAfterTerm: false
              }
              timestamp
        -- Add position to lending book
        LB.addPosition position state.lendingBook
        
        -- Use the position as the lending record (Position = LendingRecord)
        let lendingRecord = position
        do
            -- Fee calculation moved to PositionFees module
            -- Legacy fee logic removed - using new three-factor model
            
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
                    
                    -- If token is going live, contribute initial POL
                    when shouldGoLive $ do
                      -- Contribute 10% of launch threshold to POL as initial floor
                      let initialPOL = 10.0  -- 10 FeelsSOL (10% of 100)
                      contributeToTokenPOL state.polState (Token ticker) initialPOL
                    
                    -- Token registry updates would happen through a separate mechanism
                    -- For now, we've contributed to POL which is the key action
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
      Left err -> pure $ Left err
      Right txResult -> do
        -- Automatically collect gateway entry fee and contribute to JitoSOL POL
        let feeAmount = jitoAmount * 0.001  -- 0.1% entry fee
        contributeToTokenPOL state.polState JitoSOL feeAmount
        
        timestamp <- currentTime
        let newState = state { timestamp = timestamp }
        pure $ Right $ Tuple newState (GatewayEntered 
          { user, feelsSOLMinted: txResult.outputAmount.amount })
  
  ExitGateway user feelsAmount -> do
    result <- exitSystem state.gateway user feelsAmount
    case result of
      Left err -> pure $ Left err
      Right txResult -> do
        -- Automatically collect gateway exit fee and contribute to JitoSOL POL
        let feeAmount = feelsAmount * 0.002  -- 0.2% exit fee
        contributeToTokenPOL state.polState JitoSOL feeAmount
        
        timestamp <- currentTime
        let newState = state { timestamp = timestamp }
        pure $ Right $ Tuple newState (GatewayExited 
          { user, jitoSOLReceived: txResult.outputAmount.amount })
  
  _ -> pure $ Left $ InvalidCommandError "Command not implemented"

-- Execute a command that modifies state
executeCommand :: AppRuntime -> AppCommand -> Effect (Either ProtocolError AppResult)
executeCommand runtime cmd = do
  state <- read runtime.state
  result <- commandHandlers state cmd
  
  case result of
    Right (Tuple newState cmdResult) -> do
      -- Update state
      write newState runtime.state
      
      -- Notify listeners
      listeners <- read runtime.listeners
      _ <- traverse (\l -> l.callback newState) listeners
      
      pure $ Right cmdResult
    Left err -> pure $ Left err

-- Define a record of query handlers
queryHandlers :: AppState -> AppQuery -> Effect (Either ProtocolError AppResult)
queryHandlers state = case _ of
  GetUserTokens _ -> do
    -- In a real implementation, we'd filter tokens by owner
    -- For now, return empty array as we don't track ownership in TokenRegistry
    pure $ Right $ TokenList []
  
  GetAllTokens -> do
    -- Get all tokens from the registry
    allTokens <- read state.tokenRegistry
    pure $ Right $ TokenList allTokens
  
  GetUserPositions user -> do
    -- Get positions from the new lending book (returns Position type = LendingRecord)
    positions <- getUserPositions user state.lendingBook
    pure $ Right $ PositionList positions
  
  GetUserBalance user tokenType -> do
    balance <- getFeelsAccountBalance state.accounts user tokenType
    pure $ Right $ Balance balance
  
  GetLenderOffers -> do
    -- Get all active positions (which function as offers in the new system)
    offers <- getActivePositions state.lendingBook
    pure $ Right $ LenderOfferList offers
  
  GetSystemStats -> do
    activeRecords <- getActivePositions state.lendingBook
    -- All positions serve as both lending and borrowing records in the new system
    let allRecords = activeRecords
    -- Calculate total value locked (sum of all position amounts)
    let totalValueLocked = sum (map (\r -> r.amount) allRecords)
    -- Count unique users from all records
    let uniqueUsers = nub (map (\r -> r.owner) allRecords)
    let userCount = if length uniqueUsers == 0 then 1 else length uniqueUsers
    -- Count live tokens from token registry
    tokenList <- read state.tokenRegistry
    let liveCount = length (filter (\t -> t.live) tokenList)
    -- Get POL balance
    polBalance <- getPOLBalance state.polState
    -- Get total lender offers (all active positions can be offers)
    let totalLenderOffers = length activeRecords
    -- Calculate FeelsSOL supply and JitoSOL locked
    feelsSOLSupply <- getTotalTokenBalance state.accounts FeelsSOL
    jitoSOLLocked <- getTotalTokenBalance state.accounts JitoSOL
    pure $ Right $ SystemStatsResult
      { totalValueLocked: totalValueLocked  
      , totalUsers: userCount  
      , activePositions: length activeRecords
      , liveTokens: liveCount
      , totalLenderOffers: totalLenderOffers
      , polBalance: polBalance
      , feelsSOLSupply: feelsSOLSupply
      , jitoSOLLocked: jitoSOLLocked
      }
  
  GetPOLMetrics -> do
    balance <- getPOLBalance state.polState
    metrics <- getPOLMetrics state.polState
    utilizationRate <- getUtilizationRate state.polState
    pure $ Right $ POLMetricsResult
      { balance
      , growthRate24h: metrics.growthRate24h
      , utilizationRate
      }
  
  GetPositionTargetToken positionId -> do
    let targetToken = case find (\m -> m.positionId == positionId) state.positionTokenMap of
          Just mapping -> Just mapping.tokenTicker
          Nothing -> Nothing
    pure $ Right $ TargetTokenInfo targetToken
  
  _ -> pure $ Left $ InvalidCommandError "Query not implemented"

-- Execute a query that reads state
executeQuery :: AppRuntime -> AppQuery -> Effect (Either ProtocolError AppResult)
executeQuery runtime query = do
  state <- read runtime.state
  queryHandlers state query
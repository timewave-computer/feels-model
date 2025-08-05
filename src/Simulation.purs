-- Simulation engine for the Feels protocol.
-- Generates synthetic trading activity from multiple simulated accounts and executes
-- them serially to create realistic market scenarios. Supports configurable parameters
-- for different market conditions (bull/bear markets, volatility scenarios, etc.).
module Simulation
  ( SimulationConfig
  , SimulationState
  , SimulationResults
  , AccountProfile(..)
  , MarketScenario(..)
  , TradingAction(..)
  , SimulatedAccount
  , initSimulation
  , initSimulationWithLendingBook
  , runSimulation
  , runSimulationWithLendingBook
  , generateMarketScenario
  , generateTradingSequence
  , executeSimulation
  , calculateResults
  , getSimulationStats
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), length, filter, take, drop, range, head, tail, find)
import Data.Foldable (sum, foldl)
import Data.Functor (map)
import Data.Traversable (traverse, sequence)
import Data.Int as Int
import Data.Number ((%))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify)
import Effect.Random (random, randomInt)
import Effect.Console (log)
import Data.Ord (max)
import Data.String as String
import Data.Enum (toEnum)

-- Core system imports
import Token (TokenType(..), TokenAmount)
import LendingRecord (LendingRecord, LendingSide(..), LendingTerms(..), UnbondingPeriod(..), LendingStatus(..), createLenderRecord, createBorrowerRecord)
import LendingBook (LendingBook, initLendingBook, createLendOffer, takeLoan, matchAndExecute, getTotalLiquidity)
import Gateway (GatewayState, initGateway, enterSystem, exitSystem)
import SyntheticSOL (SyntheticSOLState, initSyntheticSOL)
import Utils (formatAmount, formatPercentage)
import FFI (currentTime, sqrt)
import NFV (NFVState, initNFV)
import Oracle (Oracle, PriceObservation, observeMarket, observeMarketWithBlock, initOracle, takeMarketSnapshot)

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- Account behavior profiles for different trader types
data AccountProfile
  = Conservative    -- Low-risk, stable positions
  | Moderate       -- Balanced approach
  | Aggressive     -- High-risk, high-leverage
  | Arbitrageur    -- Exploits price differences
  | Whale          -- Large positions, market impact
  | Retail         -- Small, frequent trades

derive instance eqAccountProfile :: Eq AccountProfile

instance showAccountProfile :: Show AccountProfile where
  show Conservative = "Conservative"
  show Moderate = "Moderate"  
  show Aggressive = "Aggressive"
  show Arbitrageur = "Arbitrageur"
  show Whale = "Whale"
  show Retail = "Retail"

-- Market scenario types
data MarketScenario
  = BullMarket      -- Rising prices, high optimism
  | BearMarket      -- Falling prices, high pessimism
  | SidewaysMarket  -- Range-bound, low volatility
  | VolatileMarket  -- High price swings
  | CrashScenario   -- Sudden market collapse
  | RecoveryMarket  -- Post-crash recovery

derive instance eqMarketScenario :: Eq MarketScenario

instance showMarketScenario :: Show MarketScenario where
  show BullMarket = "Bull Market"
  show BearMarket = "Bear Market"
  show SidewaysMarket = "Sideways Market"
  show VolatileMarket = "Volatile Market"
  show CrashScenario = "Market Crash"
  show RecoveryMarket = "Market Recovery"

-- Individual trading actions
data TradingAction
  = EnterProtocol String Number TokenType        -- User, Amount, Asset
  | ExitProtocol String Number TokenType         -- User, Amount, Asset
  | CreateToken String String String              -- User, Ticker, Name
  | CreateLendOffer String TokenType Number TokenType Number LendingTerms (Maybe String)  -- User, LendAsset, Amount, CollateralAsset, Ratio, Terms, TargetToken
  | TakeLoan String TokenType Number TokenType Number LendingTerms         -- User, BorrowAsset, Amount, CollateralAsset, Amount, Terms
  | ClosePosition String Int                      -- User, PositionID
  | WaitBlocks Int                               -- Simulate passage of time

derive instance eqTradingAction :: Eq TradingAction

instance showTradingAction :: Show TradingAction where
  show (EnterProtocol user amount asset) = user <> " enters " <> formatAmount amount <> " " <> show asset
  show (ExitProtocol user amount asset) = user <> " exits " <> formatAmount amount <> " " <> show asset
  show (CreateToken user ticker name) = user <> " creates token " <> ticker <> " (" <> name <> ")"
  show (CreateLendOffer user lendAsset amount collAsset ratio terms targetToken) = 
    user <> " offers " <> formatAmount amount <> " " <> show lendAsset <> 
    " @ " <> formatAmount ratio <> " " <> show collAsset <> " (" <> show terms <> ")" <>
    case targetToken of
      Just ticker -> " staking for " <> ticker
      Nothing -> ""
  show (TakeLoan user borrowAsset amount collAsset collAmount terms) = user <> " borrows " <> formatAmount amount <> " " <> show borrowAsset <> " with " <> formatAmount collAmount <> " " <> show collAsset <> " (" <> show terms <> ")"
  show (ClosePosition user posId) = user <> " closes position #" <> show posId
  show (WaitBlocks blocks) = "Wait " <> show blocks <> " blocks"

-- Simulation configuration
type SimulationConfig =
  { scenario :: MarketScenario
  , numAccounts :: Int                    -- Number of simulated accounts
  , simulationBlocks :: Int               -- How many blocks to simulate
  , initialJitoSOLPrice :: Number         -- Starting price of JitoSOL
  , priceVolatility :: Number             -- Price change volatility (0.0-1.0)
  , accountProfiles :: Array AccountProfile  -- Mix of account types
  , actionFrequency :: Number             -- Actions per block (0.0-10.0)
  , leveragePreference :: Number          -- Preference for leveraged positions (0.0-1.0)
  , stakingPreference :: Number           -- Preference for staking positions (0.0-1.0)
  }

-- Simulated account
type SimulatedAccount =
  { id :: String
  , profile :: AccountProfile
  , jitoSOLBalance :: Number
  , feelsSOLBalance :: Number
  , activePositions :: Array Int
  , totalDeposited :: Number
  , totalWithdrawn :: Number
  , netPnL :: Number
  }

-- Simulation state
type SimulationState =
  { accounts :: Array SimulatedAccount
  , lendingBook :: LendingBook
  , gateway :: GatewayState
  , oracle :: Oracle
  , currentBlock :: Int
  , currentPrice :: Number
  , priceHistory :: Array PriceObservation
  , actionHistory :: Array TradingAction
  , nextPositionId :: Int
  }

-- Simulation results
type SimulationResults =
  { totalVolume :: Number
  , totalFees :: Number
  , activePositions :: Int
  , totalUsers :: Int
  , priceChange :: Number
  , volatility :: Number
  , protocolTVL :: Number
  , averageUtilization :: Number
  , scenarioSuccess :: Boolean
  }

--------------------------------------------------------------------------------
-- Simulation Setup
--------------------------------------------------------------------------------

-- Initialize simulation with config (creates new lending book)
initSimulation :: SimulationConfig -> Effect SimulationState
initSimulation config = do
  lendingBook <- initLendingBook
  nfv <- initNFV
  oracle <- initOracle lendingBook nfv
  initSimulationWithLendingBook config lendingBook oracle

-- Initialize simulation with existing lending book for UI integration
initSimulationWithLendingBook :: SimulationConfig -> LendingBook -> Oracle -> Effect SimulationState
initSimulationWithLendingBook config existingLendingBook oracle = do
  accounts <- generateAccounts config
  nfv <- initNFV
  -- Create a simple oracle function for simulation
  let priceOracle = pure config.initialJitoSOLPrice
  syntheticSOL <- initSyntheticSOL priceOracle
  -- Create empty user balances for simulation
  userBalances <- new []
  -- Initialize gateway with proper parameters (simplified for simulation)
  let gateway = { syntheticSOL: syntheticSOL
                , entryFee: 0.001
                , exitFee: 0.002  
                , nfvAllocationRate: 0.1
                , userBalances: userBalances
                , nfvState: nfv
                }
  
  -- Get initial price observations from market
  snapshot <- takeMarketSnapshot oracle
  let initialPrices = snapshot.priceObservations
  
  pure { accounts: accounts
       , lendingBook: existingLendingBook  -- Use the existing lending book!
       , gateway: gateway
       , oracle: oracle
       , currentBlock: 0
       , currentPrice: config.initialJitoSOLPrice
       , priceHistory: initialPrices
       , actionHistory: []
       , nextPositionId: 1
       }

-- Generate simulated accounts based on config
generateAccounts :: SimulationConfig -> Effect (Array SimulatedAccount)
generateAccounts config = do
  let numAccounts = config.numAccounts
  let profiles = config.accountProfiles
  
  accountIds <- sequence $ map (\i -> pure $ "user" <> show i) (range 1 numAccounts)
  
  traverse (generateAccount profiles) accountIds
  where
    generateAccount profiles userId = do
      profileIndex <- randomInt 0 (length profiles - 1)
      let profile = case head (drop profileIndex profiles) of
            Just p -> p
            Nothing -> Moderate  -- fallback
      
      -- Generate initial balance based on profile
      baseBalance <- random
      let multiplier = case profile of
            Whale -> 10000.0 + baseBalance * 90000.0        -- 10k - 100k JitoSOL
            Aggressive -> 1000.0 + baseBalance * 9000.0     -- 1k - 10k JitoSOL  
            Conservative -> 100.0 + baseBalance * 900.0     -- 100 - 1k JitoSOL
            Arbitrageur -> 5000.0 + baseBalance * 15000.0   -- 5k - 20k JitoSOL
            Moderate -> 500.0 + baseBalance * 2000.0        -- 500 - 2.5k JitoSOL
            Retail -> 50.0 + baseBalance * 200.0            -- 50 - 250 JitoSOL
      
      pure { id: userId
           , profile: profile
           , jitoSOLBalance: multiplier
           , feelsSOLBalance: 0.0
           , activePositions: []
           , totalDeposited: 0.0
           , totalWithdrawn: 0.0
           , netPnL: 0.0
           }

--------------------------------------------------------------------------------
-- Market Scenario Generation
--------------------------------------------------------------------------------

-- Generate price movements for different market scenarios
generateMarketScenario :: SimulationConfig -> Int -> Effect Number
generateMarketScenario config currentBlock = do
  baseVolatility <- random
  let volatility = config.priceVolatility * baseVolatility
  
  -- Log scenario effect periodically
  when (currentBlock <= 5 || currentBlock `mod` 20 == 0) $ do
    log $ "Market scenario " <> show config.scenario <> " at block " <> show currentBlock
  
  case config.scenario of
    BullMarket -> do
      trend <- random
      let movement = 0.002 + trend * 0.01  -- 0.2% to 1.2% upward bias (stronger effect)
      pure movement
    
    BearMarket -> do  
      trend <- random
      let movement = -0.012 - trend * 0.008  -- -1.2% to -2.0% downward bias (stronger effect)
      pure movement
    
    SidewaysMarket -> do
      noise <- random  
      pure $ (noise - 0.5) * 0.003  -- -0.15% to +0.15%
    
    VolatileMarket -> do
      swing <- random
      direction <- random
      let magnitude = 0.03 + swing * 0.07  -- 3% to 10% moves (much stronger)
      pure $ if direction > 0.5 then magnitude else -magnitude
    
    CrashScenario -> do
      if currentBlock < 10 
        then do
          crash <- random
          pure $ -0.05 - crash * 0.15  -- -5% to -20% drops
        else do
          recovery <- random  
          pure $ -0.001 + recovery * 0.003  -- Slow recovery
    
    RecoveryMarket -> do
      recovery <- random
      let phase = Int.toNumber currentBlock / 100.0
      pure $ 0.002 + recovery * 0.008 * (1.0 - phase)  -- Diminishing recovery

--------------------------------------------------------------------------------
-- Trading Pattern Generation  
--------------------------------------------------------------------------------

-- Generate trading actions based on market conditions and account profiles
generateTradingSequence :: SimulationConfig -> SimulationState -> Effect (Array TradingAction)
generateTradingSequence config state = do
  -- Determine number of actions for this block
  baseActions <- random
  let baseNumActions = Int.floor $ config.actionFrequency * (0.5 + baseActions)
  
  -- Ensure minimum activity: at least 3 actions per block
  let numActions = max 3 baseNumActions
  
  -- Count existing tokens created in this simulation
  let existingTokenCount = length $ getRecentlyCreatedTokens state.actionHistory
  
  -- Decide on target token count (5-10) based on simulation seed
  tokenRand <- random
  let targetTokenCount = 5 + Int.floor (tokenRand * 6.0)  -- 5 to 10 tokens
      -- Create tokens early in simulation, spacing them out
      shouldCreateToken = existingTokenCount < targetTokenCount && 
                         state.currentBlock > existingTokenCount * 8 && -- Space out creation
                         state.currentBlock < 80  -- Stop creating after block 80
  
  -- Generate individual actions with a mix of action types
  -- If tokens exist, bias heavily towards token trading to generate fees
  let hasTokens = length (getRecentlyCreatedTokens state.actionHistory) > 0
  
  actions <- sequence $ map (\i -> 
    if i == 1 && shouldCreateToken
      then generateTokenCreationAction config state  -- Conditionally create tokens
      else if hasTokens && i <= (numActions * 2 / 3)  -- 2/3 of actions are token trades when tokens exist
        then do
          -- Generate swap actions for active trading and fee generation
          let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
          case head (drop (i `mod` length eligibleAccounts) eligibleAccounts) of
            Just acc -> generateTokenSwapAction config state acc
            Nothing -> generatePositionCreationAction config state
      else if i <= (numActions * 3 / 4)
        then generatePositionCreationAction config state  -- Some position creation
        else generateRandomAction config state
    ) (range 1 numActions)
  
  pure actions

-- Generate token creation action
generateTokenCreationAction :: SimulationConfig -> SimulationState -> Effect TradingAction
generateTokenCreationAction config state = do
  -- Pick a random user
  accountIndex <- randomInt 0 (length state.accounts - 1)
  case head (drop accountIndex state.accounts) of
    Just acc -> do
      -- Generate random token ticker
      rand1 <- randomInt 65 90  -- A-Z
      rand2 <- randomInt 65 90  -- A-Z
      rand3 <- randomInt 65 90  -- A-Z
      let c1 = case toEnum rand1 of
                 Just c -> String.singleton c
                 Nothing -> "A"
          c2 = case toEnum rand2 of
                 Just c -> String.singleton c
                 Nothing -> "A"
          c3 = case toEnum rand3 of
                 Just c -> String.singleton c
                 Nothing -> "A"
          ticker = c1 <> c2 <> c3
          name = ticker <> " Token"
      -- Ensure we don't create tokens that conflict with system tokens
      if ticker == "FEELSSOL" || ticker == "JITOSOL" || ticker == "SOL"
        then generateTokenCreationAction config state  -- Retry with different ticker
        else pure $ CreateToken acc.id ticker name
    Nothing -> pure $ WaitBlocks 1

-- Generate token swap action for price discovery
generateTokenSwapAction :: SimulationConfig -> SimulationState -> SimulatedAccount -> Effect TradingAction
generateTokenSwapAction config state account = do
  -- Get recently created tokens
  let existingTokens = getRecentlyCreatedTokens state.actionHistory
  
  case head existingTokens of
    Nothing -> generateLendOfferAction config state account  -- Fallback if no tokens
    Just tokenTicker -> do
      -- Generate a realistic price for the token (0.1 to 5.0 FeelsSOL per token)
      priceRand <- random
      let tokenPrice = 0.1 + priceRand * 4.9
      
      -- Decide between creating offer or taking existing offer
      actionTypeRand <- random
      if actionTypeRand < 0.7  -- 70% chance to take existing offers (execute trades)
        then do
          -- Try to take an existing loan (execute a trade)
          directionRand <- random
          if directionRand < 0.5
            then do
              -- Buy token by taking a FeelsSOL loan (collateralized by tokens)
              let feelsAmount = min account.feelsSOLBalance (20.0 + priceRand * 80.0)  -- 20-100 FeelsSOL
                  tokenCollateral = feelsAmount / tokenPrice * 1.1  -- 10% extra for collateral
              pure $ TakeLoan account.id FeelsSOL feelsAmount (Token tokenTicker) tokenCollateral SwapTerms
            else do
              -- Sell token by taking a token loan (collateralized by FeelsSOL)
              let tokenAmount = 5.0 + priceRand * 45.0  -- 5-50 tokens
                  feelsCollateral = tokenAmount * tokenPrice * 1.1  -- 10% extra for collateral
              pure $ TakeLoan account.id (Token tokenTicker) tokenAmount FeelsSOL feelsCollateral SwapTerms
        else do
          -- Create new offers (30% chance)
          directionRand <- random
          if directionRand < 0.5
            then do
              -- Create buy offer
              let feelsAmount = min account.feelsSOLBalance (50.0 + priceRand * 150.0)  -- 50-200 FeelsSOL
                  tokenAmount = feelsAmount / tokenPrice
                  exchangeRate = tokenPrice
              pure $ CreateLendOffer account.id FeelsSOL feelsAmount (Token tokenTicker) exchangeRate SwapTerms Nothing
            else do
              -- Create sell offer
              let tokenAmount = 10.0 + priceRand * 90.0  -- 10-100 tokens
                  feelsAmount = tokenAmount * tokenPrice
                  exchangeRate = 1.0 / tokenPrice
              pure $ CreateLendOffer account.id (Token tokenTicker) tokenAmount FeelsSOL exchangeRate SwapTerms Nothing

-- Generate position creation action
generatePositionCreationAction :: SimulationConfig -> SimulationState -> Effect TradingAction
generatePositionCreationAction config state = do
  -- Pick a user with FeelsSOL balance
  let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
  
  case head eligibleAccounts of
    Just account -> generateLendOfferAction config state account
    Nothing -> do
      -- No one has enough FeelsSOL, pick someone to enter
      accountIndex <- randomInt 0 (length state.accounts - 1)
      case head (drop accountIndex state.accounts) of
        Just acc -> 
          if acc.jitoSOLBalance > 0.0
            then pure $ EnterProtocol acc.id (acc.jitoSOLBalance * 0.4) JitoSOL
            else pure $ WaitBlocks 1
        Nothing -> pure $ WaitBlocks 1

-- Generate a single random action based on current state
generateRandomAction :: SimulationConfig -> SimulationState -> Effect TradingAction
generateRandomAction config state = do
  accountIndex <- randomInt 0 (length state.accounts - 1)
  
  let account = case head (drop accountIndex state.accounts) of
        Just acc -> acc
        Nothing -> { id: "user1", profile: Moderate, jitoSOLBalance: 1000.0, feelsSOLBalance: 0.0, 
                    activePositions: [], totalDeposited: 0.0, totalWithdrawn: 0.0, netPnL: 0.0 }
  
  -- Prioritize actions based on account state
  if account.feelsSOLBalance <= 0.0 && account.jitoSOLBalance > 0.0
    then generateEntryAction config account  -- Need FeelsSOL to participate
    else if length account.activePositions > 5
      then generateCloseAction config account  -- Too many positions, close some
    else do
      -- Check if we should create token swap positions
      let existingTokens = getRecentlyCreatedTokens state.actionHistory
      shouldCreateTokenSwap <- if length existingTokens > 0 && account.feelsSOLBalance > 50.0
        then do
          swapRoll <- random
          pure $ swapRoll < 0.3  -- 30% chance if conditions are met
        else pure false
      
      if shouldCreateTokenSwap
        then generateTokenSwapAction config state account
        else do
          -- Weighted action selection for better balance
          actionRoll <- random
          if actionRoll < 0.15
            then generateEntryAction config account      -- 15% chance
            else if actionRoll < 0.25
              then generateExitAction config account     -- 10% chance
              else if actionRoll < 0.50
                then generateLendOfferAction config state account  -- 25% chance
                else if actionRoll < 0.75
                  then generateBorrowAction config account   -- 25% chance
                  else if actionRoll < 0.90
                    then generateCloseAction config account  -- 15% chance
                    else pure $ WaitBlocks 1                 -- 10% chance to wait

-- Generate entry action (user deposits JitoSOL)
generateEntryAction :: SimulationConfig -> SimulatedAccount -> Effect TradingAction
generateEntryAction config account = do
  portion <- random
  let amount = case account.profile of
        Conservative -> account.jitoSOLBalance * (0.1 + portion * 0.2)  -- 10-30%
        Moderate -> account.jitoSOLBalance * (0.2 + portion * 0.3)      -- 20-50%  
        Aggressive -> account.jitoSOLBalance * (0.4 + portion * 0.5)    -- 40-90%
        Whale -> account.jitoSOLBalance * (0.05 + portion * 0.15)       -- 5-20% (smaller % but large absolute)
        Arbitrageur -> account.jitoSOLBalance * (0.3 + portion * 0.4)   -- 30-70%
        Retail -> account.jitoSOLBalance * (0.5 + portion * 0.4)        -- 50-90%
  
  pure $ EnterProtocol account.id amount JitoSOL

-- Generate exit action (user withdraws FeelsSOL)
generateExitAction :: SimulationConfig -> SimulatedAccount -> Effect TradingAction  
generateExitAction config account = do
  -- Only exit if user has FeelsSOL
  if account.feelsSOLBalance <= 0.0
    then pure $ WaitBlocks 1
    else do
      portion <- random
      let amount = account.feelsSOLBalance * (0.1 + portion * 0.5)  -- 10-60%
      pure $ ExitProtocol account.id amount FeelsSOL

-- Generate lending offer action
generateLendOfferAction :: SimulationConfig -> SimulationState -> SimulatedAccount -> Effect TradingAction
generateLendOfferAction config state account = do
  -- Only create offers if user has FeelsSOL
  if account.feelsSOLBalance <= 0.0
    then pure $ WaitBlocks 1
    else do
      portion <- random
      let amount = account.feelsSOLBalance * (0.2 + portion * 0.6)  -- 20-80%
      
      -- Choose collateral ratio based on risk profile
      ratioRand <- random
      let collateralRatio = case account.profile of
            Conservative -> 1.2 + ratioRand * 0.3   -- 120-150%
            Moderate -> 1.1 + ratioRand * 0.4       -- 110-150%
            Aggressive -> 1.05 + ratioRand * 0.25   -- 105-130%
            Whale -> 1.15 + ratioRand * 0.35        -- 115-150%
            Arbitrageur -> 1.08 + ratioRand * 0.17  -- 108-125%
            Retail -> 1.25 + ratioRand * 0.5        -- 125-175%
      
      -- Choose terms based on preferences
      terms <- generateLendingTerms config
      
      -- If this is a staking term, potentially target a recently created token
      targetToken <- case terms of
        StakingTerms _ -> do
          -- Get recently created tokens from action history
          let recentTokens = getRecentlyCreatedTokens state.actionHistory
          if length recentTokens > 0
            then do
              -- 80% chance to stake to existing tokens to ensure they get activity
              stakeChoice <- random
              if stakeChoice < 0.8
                then do
                  -- Pick a random token to stake
                  tokenIndex <- randomInt 0 (max 0 (length recentTokens - 1))
                  pure $ head (drop tokenIndex recentTokens)
                else pure Nothing
            else pure Nothing
        _ -> pure Nothing
      
      -- When staking for a token, create a position involving that token
      case targetToken of
        Just ticker -> 
          -- Create a FeelsSOL -> Token position for staking
          pure $ CreateLendOffer account.id FeelsSOL amount (Token ticker) collateralRatio terms targetToken
        Nothing ->
          -- Regular FeelsSOL/JitoSOL position
          pure $ CreateLendOffer account.id FeelsSOL amount JitoSOL collateralRatio terms targetToken

-- Generate borrowing action
generateBorrowAction :: SimulationConfig -> SimulatedAccount -> Effect TradingAction
generateBorrowAction config account = do
  -- Only borrow if user has JitoSOL for collateral
  if account.jitoSOLBalance <= 0.0
    then pure $ WaitBlocks 1
    else do
      portion <- random
      -- Base amount on available collateral
      let maxBorrowAmount = account.jitoSOLBalance / 1.5  -- 150% collateralization
      let amount = maxBorrowAmount * (0.1 + portion * 0.4)  -- 10-50% of max
      let collateralAmount = amount * 1.5
      
      terms <- generateLendingTerms config
      
      pure $ TakeLoan account.id FeelsSOL amount JitoSOL collateralAmount terms

-- Generate position closing action
generateCloseAction :: SimulationConfig -> SimulatedAccount -> Effect TradingAction
generateCloseAction config account = do
  case head account.activePositions of
    Just posId -> pure $ ClosePosition account.id posId
    Nothing -> pure $ WaitBlocks 1

-- Generate lending terms based on config preferences
generateLendingTerms :: SimulationConfig -> Effect LendingTerms
generateLendingTerms config = do
  termType <- random
  
  if termType < config.stakingPreference then do
    periodRand <- random
    let period = if periodRand < 0.5 then Days30
                else if periodRand < 0.8 then Days60  
                else Days90
    pure $ StakingTerms period
  else if termType < (config.stakingPreference + config.leveragePreference) then do
    leverageRand <- random
    let leverage = 1.5 + leverageRand * 8.5  -- 1.5x to 10x
    pure $ LeverageTerms leverage
  else
    pure SwapTerms

--------------------------------------------------------------------------------
-- Simulation Execution
--------------------------------------------------------------------------------

-- Run the complete simulation
runSimulation :: SimulationConfig -> Effect SimulationResults
runSimulation config = do
  log $ "Starting simulation: " <> show config.scenario
  initialState <- initSimulation config
  finalState <- executeSimulation config initialState
  results <- calculateResults config finalState
  log $ "Simulation completed. Total volume: " <> formatAmount results.totalVolume
  pure results

-- Run simulation with existing lending book (for UI integration)
runSimulationWithLendingBook :: SimulationConfig -> LendingBook -> Oracle -> Effect SimulationResults
runSimulationWithLendingBook config existingLendingBook oracle = do
  log $ "Starting simulation with existing lending book"
  log $ "Market scenario: " <> show config.scenario
  log $ "Price volatility: " <> show config.priceVolatility
  initialState <- initSimulationWithLendingBook config existingLendingBook oracle
  finalState <- executeSimulation config initialState
  results <- calculateResults config finalState
  log $ "Simulation completed. Total volume: " <> formatAmount results.totalVolume
  pure results

-- Execute simulation step by step
executeSimulation :: SimulationConfig -> SimulationState -> Effect SimulationState
executeSimulation config initialState = do
  -- First, ensure block 0 is observed in the oracle
  _ <- observeMarketWithBlock initialState.oracle 0
  log "Simulation: Observed block 0"
  
  -- Then execute blocks 1 through simulationBlocks
  finalState <- foldl executeBlock (pure initialState) (range 1 config.simulationBlocks)
  
  -- After simulation, ensure all blocks were observed
  -- This is important because some blocks might not have trading activity
  log "Simulation: Ensuring all blocks have observations..."
  _ <- traverse (\blockNum -> observeMarketWithBlock finalState.oracle blockNum) (range 0 config.simulationBlocks)
  
  pure finalState
  where
    executeBlock stateEffect blockNum = do
      state <- stateEffect
      executeSimulationBlock config state blockNum


-- Execute a single simulation block
executeSimulationBlock :: SimulationConfig -> SimulationState -> Int -> Effect SimulationState
executeSimulationBlock config state blockNum = do
  -- Log block execution
  when (blockNum <= 5 || Int.toNumber blockNum `mod` 10.0 == 0.0) $ do
    log $ "Executing simulation block " <> show blockNum
  
  -- Generate market-influenced price movement for this block
  priceMovement <- generateMarketScenario config blockNum
  
  -- Update state with market-influenced price
  let marketInfluencedState = state { currentPrice = state.currentPrice * (1.0 + priceMovement) }
  
  -- Generate trading actions for this block
  actions <- generateTradingSequence config marketInfluencedState
  
  -- Execute each action
  newState <- foldl executeAction (pure marketInfluencedState) actions
  
  -- Get price observations from actual market activity with block number
  _ <- observeMarketWithBlock newState.oracle blockNum  -- Update oracle with block number
  snapshot <- takeMarketSnapshot newState.oracle
  let priceObservations = snapshot.priceObservations
  
  -- Calculate current JitoSOL/FeelsSOL price from observations (if available)
  let jitoSOLObs = find (\obs -> obs.baseAsset == JitoSOL) priceObservations
      newPrice = case jitoSOLObs of
        Just obs -> obs.impliedPrice
        Nothing -> state.currentPrice  -- Keep previous price if no new observations
  
  -- Update state with new price and block
  let finalState = newState
  pure $ finalState { currentBlock = blockNum
                   , currentPrice = newPrice
                   , priceHistory = priceObservations <> state.priceHistory
                   , actionHistory = state.actionHistory <> actions
                   }

-- Execute a single trading action
executeAction :: Effect SimulationState -> TradingAction -> Effect SimulationState
executeAction stateEffect action = do
  state <- stateEffect
  case action of
    EnterProtocol userId amount asset -> do
      log $ "Executing: " <> show action
      -- Update account balances to simulate gateway entry
      -- Deduct JitoSOL and add FeelsSOL (1:1 exchange rate for simplicity)
      let updatedAccounts1 = updateAccountBalances state.accounts userId (-amount) JitoSOL
      let updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount FeelsSOL
      -- Fees are automatically collected by the protocol
      pure $ state { accounts = updatedAccounts2 }
    
    ExitProtocol userId amount asset -> do
      log $ "Executing: " <> show action  
      -- Update account balances to simulate gateway exit
      -- Deduct FeelsSOL and add JitoSOL (1:1 exchange rate for simplicity)
      let updatedAccounts1 = updateAccountBalances state.accounts userId (-amount) FeelsSOL
      let updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount JitoSOL
      pure $ state { accounts = updatedAccounts2 }
    
    CreateToken userId ticker name -> do
      log $ "Executing: " <> show action
      -- In simulation, we don't have direct access to token registry
      -- But the action will be recorded in the action history
      -- When we connect to the actual protocol, it will create the token
      pure state
    
    CreateLendOffer userId lendAsset amount collAsset ratio terms targetToken -> do
      log $ "Executing: " <> show action
      -- Actually create the lending offer in the book
      result <- createLendOffer state.lendingBook userId lendAsset amount collAsset ratio terms
      case result of
        Left err -> do
          log $ "Failed to create lending offer: " <> err
          pure state
        Right lendingRecord -> do
          log $ "Created lending offer with ID: " <> show lendingRecord.id <> 
                " - " <> show lendingRecord.lendAmount <> " " <> show lendingRecord.lendAsset <>
                " (status: " <> show lendingRecord.status <> ")" <>
                case targetToken of
                  Just ticker -> " targeting token: " <> ticker
                  Nothing -> ""
          -- Fees are now automatically collected by the protocol
          -- The state.lendingBook is now updated by reference, so we return the same state
          -- but the lendingBook within it has been modified
          pure state
    
    TakeLoan userId borrowAsset amount collAsset collAmount terms -> do
      log $ "Executing: " <> show action
      -- Actually execute the loan (take existing offers)
      result <- takeLoan state.lendingBook userId borrowAsset amount collAsset collAmount terms
      case result of
        Left err -> do
          log $ "Failed to take loan: " <> err
          pure state
        Right matchResult -> do
          log $ "Successfully took loan - executed " <> show matchResult.executedAmount <> " " <> show borrowAsset
          log $ "Borrower record ID: " <> show matchResult.borrowerRecord.id <> ", Lender record ID: " <> show matchResult.lenderRecord.id
          -- Fees are automatically collected by the protocol
          -- Update account balances based on the swap
          let updatedAccounts1 = updateAccountBalances state.accounts userId matchResult.executedAmount borrowAsset
              updatedAccounts2 = updateAccountBalances updatedAccounts1 userId (-collAmount) collAsset
              updatedAccounts3 = markAccountActive updatedAccounts2 userId amount
          pure $ state { accounts = updatedAccounts3 }
    
    ClosePosition userId posId -> do
      log $ "Executing: " <> show action
      pure state
    
    WaitBlocks blocks -> 
      pure state

-- Calculate final simulation results
calculateResults :: SimulationConfig -> SimulationState -> Effect SimulationResults
calculateResults config finalState = do
  let priceStart = config.initialJitoSOLPrice
  let priceEnd = finalState.currentPrice
  let priceChange = (priceEnd - priceStart) / priceStart
  
  -- Calculate volatility from price history (using JitoSOL prices)
  let jitoSOLPrices = map _.impliedPrice $ filter (\obs -> obs.baseAsset == JitoSOL) finalState.priceHistory
  let prices = if length jitoSOLPrices > 0 then jitoSOLPrices else [config.initialJitoSOLPrice]
  let avgPrice = sum prices / Int.toNumber (length prices)
  let variance = sum (map (\p -> (p - avgPrice) * (p - avgPrice)) prices) / Int.toNumber (length prices)
  let volatility = sqrt variance / avgPrice
  
  -- Calculate other metrics (TODO: Fix when getTotalLiquidity signature is clarified)
  let totalLiquidity = 0.0  -- getTotalLiquidity finalState.lendingBook
  
  pure { totalVolume: 0.0  -- TODO: Track total volume
       , totalFees: 0.0    -- TODO: Track total fees  
       , activePositions: 0 -- TODO: Count active positions
       , totalUsers: length finalState.accounts
       , priceChange: priceChange
       , volatility: volatility
       , protocolTVL: totalLiquidity
       , averageUtilization: 0.0  -- TODO: Calculate utilization
       , scenarioSuccess: true     -- TODO: Define success criteria
       }

--------------------------------------------------------------------------------
-- Account Update Functions
--------------------------------------------------------------------------------

-- Update account balances for protocol entry/exit
updateAccountBalances :: Array SimulatedAccount -> String -> Number -> TokenType -> Array SimulatedAccount
updateAccountBalances accounts userId amount asset =
  map (\acc -> 
    if acc.id == userId 
    then case asset of
      JitoSOL -> acc { jitoSOLBalance = acc.jitoSOLBalance + amount
                     , totalDeposited = acc.totalDeposited + (if amount > 0.0 then amount else 0.0)
                     , totalWithdrawn = acc.totalWithdrawn + (if amount < 0.0 then (-amount) else 0.0)
                     }
      FeelsSOL -> acc { feelsSOLBalance = acc.feelsSOLBalance + amount
                      , totalDeposited = acc.totalDeposited + (if amount > 0.0 then amount else 0.0)
                      , totalWithdrawn = acc.totalWithdrawn + (if amount < 0.0 then (-amount) else 0.0)
                      }
      _ -> acc -- Don't handle other token types in simulation
    else acc
  ) accounts

-- Mark account as active with trading activity
markAccountActive :: Array SimulatedAccount -> String -> Number -> Array SimulatedAccount
markAccountActive accounts userId amount =
  map (\acc -> 
    if acc.id == userId 
    then acc { totalDeposited = acc.totalDeposited + amount
             , netPnL = acc.netPnL + (amount * 0.01) -- Simulate small profit
             }
    else acc
  ) accounts

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- Get recently created tokens from action history
getRecentlyCreatedTokens :: Array TradingAction -> Array String
getRecentlyCreatedTokens actions = 
  foldl extractToken [] actions
  where
    extractToken acc action = case action of
      CreateToken _ ticker _ -> ticker : acc
      _ -> acc

-- Get simulation statistics
getSimulationStats :: SimulationState -> Effect String  
getSimulationStats state = do
  let totalLiquidity = 0.0  -- TODO: Fix when getTotalLiquidity signature is clarified
  
  let stats = "=== Simulation Statistics ===\n" <>
              "Current Block: " <> show state.currentBlock <> "\n" <>
              "Current Price: " <> formatAmount state.currentPrice <> "\n" <>
              "Total Accounts: " <> show (length state.accounts) <> "\n" <>
              "Total Actions: " <> show (length state.actionHistory) <> "\n" <>
              "Protocol TVL: " <> formatAmount totalLiquidity
  
  pure stats
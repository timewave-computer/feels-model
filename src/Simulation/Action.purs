-- | Trading Actions and Sequence Generation for Simulation Engine
-- |
-- | This module implements comprehensive trading action generation based on
-- | market conditions, agent behavior profiles, and historical activity patterns.
-- | It creates realistic trading sequences that drive protocol interactions.
-- |
-- | Key Features:
-- | - Profile-driven action generation matching agent behavioral patterns
-- | - Market scenario-aware trading decisions (bull/bear/volatile markets)
-- | - Token creation and discovery mechanics for protocol fee generation
-- | - Sophisticated swap generation with realistic volume patterns
-- | - Position lifecycle management from creation to closure
-- |
-- | Action Types:
-- | - Protocol Entry/Exit: JitoSOL ↔ FeelsSOL conversions with fees
-- | - Token Creation: New token launches with staking opportunities
-- | - Trading Actions: Token swaps for price discovery and fee generation
-- | - Position Management: Creation, modification, and closure of positions
-- | - Timing Actions: Wait blocks for realistic pacing and sequencing
module Simulation.Action
  ( TradingAction(..)
  , generateTradingSequence
  , getRecentlyCreatedTokens
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array ((:), head, drop, range, length, filter)
import Data.Foldable (foldl)
import Data.Traversable (sequence)
import Data.Int as Int
import Effect (Effect)
import Effect.Random (random, randomInt)
import Data.String as String
import Data.Enum (toEnum)

-- Core protocol system imports
import Protocol.Token (TokenType(..))
import Protocol.Position (Duration(..), monthlyDuration, spotDuration)
import Utils (formatAmount)

-- Import simulation subsystem dependencies
import Simulation.Agent (AccountProfile(..), SimulatedAccount)
import Simulation.Market (SimulationConfig, MarketScenario(..), generateMarketScenario)

-- Oracle integration for market data
import Protocol.Oracle (Oracle, takeMarketSnapshot)

-- | Minimal simulation state interface for action generation
-- | Contains essential state needed for realistic action generation
type ActionSimulationState =
  { accounts :: Array SimulatedAccount   -- All agent accounts with current balances
  , currentBlock :: Int                  -- Current simulation block for timing
  , actionHistory :: Array TradingAction -- Historical actions for context
  , oracle :: Oracle                     -- Price oracle for market data
  }

--------------------------------------------------------------------------------
-- TRADING ACTION TYPE DEFINITIONS
--------------------------------------------------------------------------------
-- Comprehensive action types representing all possible protocol interactions

-- | Complete set of trading actions representing all protocol interactions
-- | Each action type captures specific user behavior patterns and economic activity
data TradingAction
  = EnterProtocol String Number TokenType        -- Protocol entry: User converts assets to FeelsSOL
  | ExitProtocol String Number TokenType         -- Protocol exit: User converts FeelsSOL back to assets
  | CreateToken String String String              -- Token creation: User launches new token with ticker/name
  | CreateLendOffer String TokenType Number TokenType Number Duration (Maybe String)  -- Lending: User offers liquidity with terms
  | TakeLoan String TokenType Number TokenType Number Duration         -- Borrowing: User takes loan against collateral
  | ClosePosition String Int                      -- Position management: User closes existing position
  | WaitBlocks Int                               -- Timing control: Simulate realistic action pacing

derive instance eqTradingAction :: Eq TradingAction

-- | Human-readable action descriptions for logging and analysis
instance showTradingAction :: Show TradingAction where
  show (EnterProtocol user amount asset) = user <> " enters protocol with " <> formatAmount amount <> " " <> show asset
  show (ExitProtocol user amount asset) = user <> " exits protocol with " <> formatAmount amount <> " " <> show asset
  show (CreateToken user ticker name) = user <> " creates token " <> ticker <> " (" <> name <> ")"
  show (CreateLendOffer user lendAsset amount collAsset ratio term targetToken) = 
    user <> " offers " <> formatAmount amount <> " " <> show lendAsset <> 
    " @ " <> formatAmount ratio <> " " <> show collAsset <> " (" <> show term <> ")" <>
    case targetToken of
      Just ticker -> " for token " <> ticker
      Nothing -> ""
  show (TakeLoan user borrowAsset amount collAsset collAmount term) = 
    user <> " borrows " <> formatAmount amount <> " " <> show borrowAsset <> 
    " with " <> formatAmount collAmount <> " " <> show collAsset <> " (" <> show term <> ")"
  show (ClosePosition user posId) = user <> " closes position #" <> show posId
  show (WaitBlocks blocks) = "Wait " <> show blocks <> " blocks for realistic pacing"

--------------------------------------------------------------------------------
-- COMPREHENSIVE TRADING SEQUENCE GENERATION ENGINE
--------------------------------------------------------------------------------
-- Advanced action generation system creating realistic trading patterns

-- | Generate comprehensive trading sequence based on market conditions and agent behaviors
-- | Creates realistic action patterns that drive protocol activity and fee generation
generateTradingSequence :: SimulationConfig -> ActionSimulationState -> Effect (Array TradingAction)
generateTradingSequence config state = do
  -- Calculate dynamic action frequency based on market conditions and configuration
  baseActions <- random
  let baseNumActions = Int.floor $ config.actionFrequency * (0.5 + baseActions)
  
  -- Ensure realistic minimum activity levels for protocol viability
  let numActions = max 3 baseNumActions
  
  -- Analyze current token ecosystem status
  let existingTokenCount = length $ getRecentlyCreatedTokens state.actionHistory
  
  -- Determine target token diversity (5-10 tokens) for healthy ecosystem
  tokenRand <- random
  let targetTokenCount = 5 + Int.floor (tokenRand * 6.0)  -- 5-10 tokens for variety
      
      -- Strategic token creation timing for ecosystem development
      shouldCreateToken = existingTokenCount < targetTokenCount && 
                         state.currentBlock > existingTokenCount * 8 &&  -- Space out launches
                         state.currentBlock < 80                         -- Focus creation early
  
  -- Assess token trading opportunities for fee generation
  let hasActiveTokens = length (getRecentlyCreatedTokens state.actionHistory) > 0
  
  -- Generate diversified action sequence with market-aware distribution
  actions <- sequence $ map (\i -> 
    if i == 1 && shouldCreateToken
      then generateTokenCreationAction config state  -- Strategic token launches
      else if hasActiveTokens && Int.toNumber i <= (Int.toNumber numActions * 4.0 / 5.0)
        then do
          -- Prioritize token trading when tokens exist (80% of actions)
          -- This generates protocol fees and drives price discovery
          let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
              -- Rotate through accounts for realistic trading distribution
              accountIndex = (i * 7 + state.currentBlock) `mod` max 1 (length eligibleAccounts)
          case head (drop accountIndex eligibleAccounts) of
            Just acc -> generateTokenSwapAction config state acc
            Nothing -> generatePositionCreationAction config state
      else if Int.toNumber i <= (Int.toNumber numActions * 3.0 / 4.0)
        then generatePositionCreationAction config state  -- Position creation for liquidity
        else generateRandomAction config state             -- Variety actions for realism
    ) (range 1 numActions)
  
  pure actions

--------------------------------------------------------------------------------
-- TOKEN CREATION ACTION GENERATION
--------------------------------------------------------------------------------
-- Strategic token creation for ecosystem development and fee generation

-- | Generate strategic token creation action with realistic naming and validation
-- | Creates new tokens that drive protocol activity and fee generation opportunities
generateTokenCreationAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
generateTokenCreationAction config state = do
  -- Select random account for token creation (any account can create tokens)
  accountIndex <- randomInt 0 (length state.accounts - 1)
  case head (drop accountIndex state.accounts) of
    Just acc -> do
      -- Generate realistic 3-letter token ticker using uppercase letters
      rand1 <- randomInt 65 90  -- ASCII A-Z range
      rand2 <- randomInt 65 90  -- ASCII A-Z range
      rand3 <- randomInt 65 90  -- ASCII A-Z range
      
      -- Convert ASCII codes to characters with fallback safety
      let c1 = case toEnum rand1 of
                 Just c -> String.singleton c
                 Nothing -> "A"  -- Fallback for invalid ASCII
          c2 = case toEnum rand2 of
                 Just c -> String.singleton c
                 Nothing -> "B"  -- Different fallback for variety
          c3 = case toEnum rand3 of
                 Just c -> String.singleton c
                 Nothing -> "C"  -- Different fallback for variety
          ticker = c1 <> c2 <> c3
          name = ticker <> " Token"  -- Simple descriptive name
      
      -- Validate ticker doesn't conflict with system tokens
      if ticker == "FEELSSOL" || ticker == "JITOSOL" || ticker == "SOL"
        then generateTokenCreationAction config state  -- Recursive retry for conflicts
        else pure $ CreateToken acc.id ticker name
    Nothing -> pure $ WaitBlocks 1  -- Fallback if no accounts available

--------------------------------------------------------------------------------
-- TOKEN SWAP ACTION GENERATION ENGINE
--------------------------------------------------------------------------------
-- Sophisticated swap generation with market scenario awareness and realistic pricing

-- | Generate market-aware token swap action for price discovery and fee generation
-- | Creates realistic trading patterns based on market conditions and agent profiles
generateTokenSwapAction :: SimulationConfig -> ActionSimulationState -> SimulatedAccount -> Effect TradingAction
generateTokenSwapAction config state account = do
  -- Select token for trading from available ecosystem
  let existingTokens = getRecentlyCreatedTokens state.actionHistory
      -- Rotate through tokens based on block for distributed trading activity
      tokenIndex = state.currentBlock `mod` max 1 (length existingTokens)
  
  case head (drop tokenIndex existingTokens) of
    Nothing -> pure $ WaitBlocks 1  -- No tokens available yet
    Just tokenTicker -> do
      -- Apply market scenario dynamics to inform trading decisions
      _ <- generateMarketScenario config state.currentBlock
      priceRand <- random
      
      -- Get current market price from oracle for realistic pricing
      currentMarketPrice <- do
        snapshot <- takeMarketSnapshot state.oracle
        pure snapshot.spot  -- Use real-time price data
      
      -- Determine trading direction based on market sentiment and agent profile
      swapRoll <- random
      let profile = account.profile
          isAggressiveTrader = profile == Aggressive || profile == Whale
          
          -- Market scenario influences trading bias
          marketBias = case config.scenario of
            BullMarket -> 0.7    -- 70% buy probability in bull markets
            BearMarket -> 0.3    -- 30% buy probability in bear markets
            _ -> 0.5             -- Neutral 50/50 in other scenarios
          
          -- Aggressive traders amplify market bias by 20%
          shouldBuy = swapRoll < (marketBias + if isAggressiveTrader then 0.2 else 0.0)
      
      -- Execute buy orders (FeelsSOL → Token)
      if shouldBuy && account.feelsSOLBalance >= 50.0
        then do
          amountRand <- random
          
          -- Profile-based position sizing for realistic market behavior
          let swapAmount = case profile of
                Whale -> 100.0 + amountRand * 400.0      -- 100-500 FeelsSOL (high impact)
                Aggressive -> 50.0 + amountRand * 150.0  -- 50-200 FeelsSOL (active trading)
                _ -> 20.0 + amountRand * 80.0            -- 20-100 FeelsSOL (typical retail)
              
              -- Risk management: limit to 80% of available balance
              actualAmount = min swapAmount (account.feelsSOLBalance * 0.8)
              
              -- Market making spread: 5-15% above market price for buy orders
              priceWithSpread = currentMarketPrice * (1.05 + priceRand * 0.1)
          
          pure $ CreateLendOffer account.id (Token tokenTicker) actualAmount FeelsSOL priceWithSpread spotDuration Nothing
          
        -- Execute sell orders (Token → FeelsSOL)
        else if account.feelsSOLBalance >= 20.0
          then do
            amountRand <- random
            
            -- Profile-based sell sizing with more conservative amounts
            let swapAmount = case profile of
                  Whale -> 80.0 + amountRand * 320.0       -- 80-400 FeelsSOL
                  Aggressive -> 40.0 + amountRand * 120.0  -- 40-160 FeelsSOL
                  _ -> 15.0 + amountRand * 60.0            -- 15-75 FeelsSOL
                
                -- More conservative limit for sells: 60% of balance
                actualAmount = min swapAmount (account.feelsSOLBalance * 0.6)
                
                -- Market making spread: 5-15% below market price for sell orders
                priceWithSpread = currentMarketPrice * (0.95 - priceRand * 0.1)
            
            pure $ CreateLendOffer account.id FeelsSOL actualAmount (Token tokenTicker) priceWithSpread spotDuration Nothing
          else pure $ WaitBlocks 1  -- Insufficient balance for trading

--------------------------------------------------------------------------------
-- POSITION CREATION ACTION GENERATION
--------------------------------------------------------------------------------
-- Specialized position creation for protocol liquidity and yield generation

-- | Generate position creation action focusing on protocol liquidity provision
-- | Creates positions that generate yield and support protocol mechanics
generatePositionCreationAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
generatePositionCreationAction config state = do
  -- Identify accounts with sufficient FeelsSOL for position creation
  let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
  
  case head eligibleAccounts of
    Just account -> 
      -- Generate token swap as primary position creation mechanism
      -- Token swaps create liquidity and drive fee generation
      generateTokenSwapAction config state account
    Nothing -> do
      -- Fallback behavior when no eligible accounts available
      actionRand <- random
      if actionRand < 0.5
        then pure $ WaitBlocks 1                    -- Wait for better conditions
        else generateRandomAction config state      -- Generate alternative action

--------------------------------------------------------------------------------
-- RANDOM ACTION GENERATION FOR VARIETY
--------------------------------------------------------------------------------
-- Diverse action generation to create realistic trading variety and protocol usage

-- | Generate diverse random actions to maintain realistic protocol activity variety
-- | Creates balanced mix of protocol entry/exit, trading, and staking activities
generateRandomAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
generateRandomAction config state = do
  -- Select random account for action generation
  accountIndex <- randomInt 0 (length state.accounts - 1)
  
  -- Provide fallback account if selection fails
  let account = case head (drop accountIndex state.accounts) of
        Just acc -> acc
        Nothing -> { id: "user1", profile: Moderate, jitoSOLBalance: 1000.0, feelsSOLBalance: 0.0, 
                    activePositions: [], totalDeposited: 0.0, totalWithdrawn: 0.0, netPnL: 0.0 }
  
  actionRoll <- random
  
  -- Prioritize token trading when tokens exist and account has sufficient balance
  let existingTokens = getRecentlyCreatedTokens state.actionHistory
  shouldCreateTokenSwap <- if length existingTokens > 0 && account.feelsSOLBalance > 50.0
    then do
      swapRoll <- random
      pure $ swapRoll < 0.5  -- 50% probability for token trading when conditions met
    else pure false
  
  if shouldCreateTokenSwap
    then generateTokenSwapAction config state account
    else if actionRoll < 0.3
      then do
        -- Protocol entry: JitoSOL → FeelsSOL conversion
        amountRand <- random
        
        -- Profile-based entry amounts reflecting realistic user behavior
        let amount = case account.profile of
              Whale -> 500.0 + amountRand * 2000.0       -- 500-2500 JitoSOL (institutional)
              Aggressive -> 200.0 + amountRand * 800.0   -- 200-1000 JitoSOL (active traders)
              Conservative -> 50.0 + amountRand * 200.0  -- 50-250 JitoSOL (cautious users)
              _ -> 100.0 + amountRand * 400.0            -- 100-500 JitoSOL (typical users)
        
        pure $ EnterProtocol account.id amount JitoSOL
      else if actionRoll < 0.5
        then do
          -- Protocol exit: FeelsSOL → JitoSOL conversion (only if sufficient balance)
          if account.feelsSOLBalance > 50.0
            then do
              amountRand <- random
              -- Conservative exit sizing: max 50% of balance or 250 FeelsSOL
              let amount = min (account.feelsSOLBalance * 0.5) (50.0 + amountRand * 200.0)
              pure $ ExitProtocol account.id amount FeelsSOL
            else pure $ WaitBlocks 1  -- Wait if insufficient balance
      else if actionRoll < 0.8
        then do
          -- Token staking for launch support and yield generation
          let recentTokens = getRecentlyCreatedTokens state.actionHistory
          if length recentTokens == 0
            then pure $ WaitBlocks 1  -- No tokens available for staking
            else do
              -- Select random token for staking activity
              tokenIndex <- randomInt 0 (max 0 (length recentTokens - 1))
              case head (drop tokenIndex recentTokens) of
                Just tokenTicker -> do
                  if account.feelsSOLBalance >= 100.0
                    then do
                      -- Standard 100 FeelsSOL stake for token launch support
                      pure $ CreateLendOffer account.id FeelsSOL 100.0 (Token tokenTicker) 100.0 Monthly (Just tokenTicker)
                    else pure $ WaitBlocks 1  -- Insufficient balance for staking
                Nothing -> pure $ WaitBlocks 1  -- Token selection failed
        else pure $ WaitBlocks 1  -- Default wait action for pacing

--------------------------------------------------------------------------------
-- ACTION ANALYSIS AND UTILITY FUNCTIONS
--------------------------------------------------------------------------------
-- Supporting functions for action history analysis and ecosystem tracking

-- | Extract all token tickers created during the simulation
-- | Used for tracking token ecosystem development and trading opportunities
getRecentlyCreatedTokens :: Array TradingAction -> Array String
getRecentlyCreatedTokens actions = 
  foldl extractToken [] actions
  where
    -- | Extract token ticker from CreateToken actions
    extractToken acc action = case action of
      CreateToken _ ticker _ -> ticker : acc  -- Accumulate all created token tickers
      _ -> acc                                -- Ignore non-creation actions
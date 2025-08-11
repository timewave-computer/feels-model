-- Trading Actions and Sequence Generation for Simulation Engine
-- Handles generation of trading actions based on market conditions and agent behaviors
module Simulation.Actions
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

-- Core system imports
import Token (TokenType(..))
import Position (TermCommitment(..), weeklyTerm)
import Utils (formatAmount)

-- Import from our new modules
import Simulation.Agents (AccountProfile(..), SimulatedAccount)
import Simulation.Market (SimulationConfig, MarketScenario(..), generateMarketScenario)

-- Import needed types that will be in Engine (for now we'll define minimal versions)
import Oracle (Oracle, takeMarketSnapshot)

-- Minimal simulation state type for actions (Engine will have the full version)
type ActionSimulationState =
  { accounts :: Array SimulatedAccount
  , currentBlock :: Int  
  , actionHistory :: Array TradingAction
  , oracle :: Oracle
  }

--------------------------------------------------------------------------------
-- Trading Action Types
--------------------------------------------------------------------------------

-- Individual trading actions
data TradingAction
  = EnterProtocol String Number TokenType        -- User, Amount, Asset
  | ExitProtocol String Number TokenType         -- User, Amount, Asset
  | CreateToken String String String              -- User, Ticker, Name
  | CreateLendOffer String TokenType Number TokenType Number TermCommitment (Maybe String)  -- User, LendAsset, Amount, CollateralAsset, Ratio, Term, TargetToken
  | TakeLoan String TokenType Number TokenType Number TermCommitment         -- User, BorrowAsset, Amount, CollateralAsset, Amount, Term
  | ClosePosition String Int                      -- User, PositionID
  | WaitBlocks Int                               -- Simulate passage of time

derive instance eqTradingAction :: Eq TradingAction

instance showTradingAction :: Show TradingAction where
  show (EnterProtocol user amount asset) = user <> " enters " <> formatAmount amount <> " " <> show asset
  show (ExitProtocol user amount asset) = user <> " exits " <> formatAmount amount <> " " <> show asset
  show (CreateToken user ticker name) = user <> " creates token " <> ticker <> " (" <> name <> ")"
  show (CreateLendOffer user lendAsset amount collAsset ratio term targetToken) = 
    user <> " offers " <> formatAmount amount <> " " <> show lendAsset <> 
    " @ " <> formatAmount ratio <> " " <> show collAsset <> " (" <> show term <> ")" <>
    case targetToken of
      Just ticker -> " staking for " <> ticker
      Nothing -> ""
  show (TakeLoan user borrowAsset amount collAsset collAmount term) = user <> " borrows " <> formatAmount amount <> " " <> show borrowAsset <> " with " <> formatAmount collAmount <> " " <> show collAsset <> " (" <> show term <> ")"
  show (ClosePosition user posId) = user <> " closes position #" <> show posId
  show (WaitBlocks blocks) = "Wait " <> show blocks <> " blocks"

--------------------------------------------------------------------------------
-- Trading Pattern Generation  
--------------------------------------------------------------------------------

-- Generate trading actions based on market conditions and account profiles
generateTradingSequence :: SimulationConfig -> ActionSimulationState -> Effect (Array TradingAction)
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
  
  -- Log token activity
  -- when (state.currentBlock `mod` 10 == 0 && hasTokens) $ do
  --   log $ "Block " <> show state.currentBlock <> ": Active tokens: " <> show (getRecentlyCreatedTokens state.actionHistory)
  
  actions <- sequence $ map (\i -> 
    if i == 1 && shouldCreateToken
      then generateTokenCreationAction config state  -- Conditionally create tokens
      else if hasTokens && Int.toNumber i <= (Int.toNumber numActions * 4.0 / 5.0)  -- 80% of actions are token trades when tokens exist
        then do
          -- Generate swap actions for active trading and fee generation
          let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
              -- Better account distribution to ensure variety
              accountIndex = (i * 7 + state.currentBlock) `mod` max 1 (length eligibleAccounts)
          case head (drop accountIndex eligibleAccounts) of
            Just acc -> generateTokenSwapAction config state acc
            Nothing -> generatePositionCreationAction config state
      else if Int.toNumber i <= (Int.toNumber numActions * 3.0 / 4.0)
        then generatePositionCreationAction config state  -- Some position creation
        else generateRandomAction config state
    ) (range 1 numActions)
  
  pure actions

-- Generate token creation action
generateTokenCreationAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
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
generateTokenSwapAction :: SimulationConfig -> ActionSimulationState -> SimulatedAccount -> Effect TradingAction
generateTokenSwapAction config state account = do
  -- Get recently created tokens and cycle through them
  let existingTokens = getRecentlyCreatedTokens state.actionHistory
      -- Rotate through different tokens based on block number for variety
      tokenIndex = state.currentBlock `mod` max 1 (length existingTokens)
  
  case head (drop tokenIndex existingTokens) of
    Nothing -> pure $ WaitBlocks 1  -- No tokens yet, wait
    Just tokenTicker -> do
      -- Apply market scenario to token prices
      _ <- generateMarketScenario config state.currentBlock
      priceRand <- random
      
      -- Get current market price from oracle if available, otherwise use random
      currentMarketPrice <- case state.oracle of
        oracle -> do
          snapshot <- takeMarketSnapshot oracle
          -- Use spot price from snapshot
          pure snapshot.spot
      
      -- Determine swap direction and amount based on market sentiment and profile
      swapRoll <- random
      let profile = account.profile
          isAggressiveTrader = profile == Aggressive || profile == Whale
          marketBias = case config.scenario of
            BullMarket -> 0.7    -- 70% chance to buy in bull market
            BearMarket -> 0.3    -- 30% chance to buy in bear market
            _ -> 0.5             -- 50/50 otherwise
          
          -- Bias towards buying based on market and trader type
          shouldBuy = swapRoll < (marketBias + if isAggressiveTrader then 0.2 else 0.0)
      
      if shouldBuy && account.feelsSOLBalance >= 50.0
        then do
          -- Buy token with FeelsSOL
          amountRand <- random
          let swapAmount = case profile of
                Whale -> 100.0 + amountRand * 400.0      -- 100-500 FeelsSOL
                Aggressive -> 50.0 + amountRand * 150.0  -- 50-200 FeelsSOL  
                _ -> 20.0 + amountRand * 80.0            -- 20-100 FeelsSOL
              
              -- Ensure we don't exceed balance
              actualAmount = min swapAmount (account.feelsSOLBalance * 0.8)
              
              -- Token to FeelsSOL ratio (market price with some spread)
              priceWithSpread = currentMarketPrice * (1.05 + priceRand * 0.1)  -- 1.05x-1.15x market price
          
          pure $ CreateLendOffer account.id (Token tokenTicker) actualAmount FeelsSOL priceWithSpread Spot Nothing
        else if account.feelsSOLBalance >= 20.0
          then do
            -- Sell FeelsSOL for token
            amountRand <- random
            let swapAmount = case profile of
                  Whale -> 80.0 + amountRand * 320.0       -- 80-400 FeelsSOL
                  Aggressive -> 40.0 + amountRand * 120.0  -- 40-160 FeelsSOL
                  _ -> 15.0 + amountRand * 60.0            -- 15-75 FeelsSOL
                
                -- Ensure we don't exceed balance  
                actualAmount = min swapAmount (account.feelsSOLBalance * 0.6)
                
                -- FeelsSOL to Token ratio (market price with spread)
                priceWithSpread = currentMarketPrice * (0.95 - priceRand * 0.1)  -- 0.85x-0.95x market price
            
            pure $ CreateLendOffer account.id FeelsSOL actualAmount (Token tokenTicker) priceWithSpread Spot Nothing
          else pure $ WaitBlocks 1

-- Generate position creation action
generatePositionCreationAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
generatePositionCreationAction config state = do
  -- Pick a user with FeelsSOL balance
  let eligibleAccounts = filter (\acc -> acc.feelsSOLBalance > 10.0) state.accounts
  
  case head eligibleAccounts of
    Just account -> generateTokenSwapAction config state account
    Nothing -> do
      -- Fallback to basic actions if no suitable accounts
      actionRand <- random
      if actionRand < 0.5
        then pure $ WaitBlocks 1
        else generateRandomAction config state

-- Generate random action for variety
generateRandomAction :: SimulationConfig -> ActionSimulationState -> Effect TradingAction
generateRandomAction config state = do
  accountIndex <- randomInt 0 (length state.accounts - 1)
  
  let account = case head (drop accountIndex state.accounts) of
        Just acc -> acc
        Nothing -> { id: "user1", profile: Moderate, jitoSOLBalance: 1000.0, feelsSOLBalance: 0.0, 
                    activePositions: [], totalDeposited: 0.0, totalWithdrawn: 0.0, netPnL: 0.0 }
  
  actionRoll <- random
  
  -- Increased probability of token swaps when tokens exist
  let existingTokens = getRecentlyCreatedTokens state.actionHistory
  shouldCreateTokenSwap <- if length existingTokens > 0 && account.feelsSOLBalance > 50.0
    then do
      swapRoll <- random
      pure $ swapRoll < 0.5  -- 50% chance if conditions are met (increased from 30%)
    else pure false
  
  if shouldCreateTokenSwap
    then generateTokenSwapAction config state account
    else if actionRoll < 0.3
      then do
        -- Enter protocol (JitoSOL -> FeelsSOL)
        amountRand <- random
        let amount = case account.profile of
              Whale -> 500.0 + amountRand * 2000.0
              Aggressive -> 200.0 + amountRand * 800.0
              Conservative -> 50.0 + amountRand * 200.0
              _ -> 100.0 + amountRand * 400.0
        pure $ EnterProtocol account.id amount JitoSOL
      else if actionRoll < 0.5
        then do
          -- Exit protocol (FeelsSOL -> JitoSOL) - but only if they have FeelsSOL
          if account.feelsSOLBalance > 50.0
            then do
              amountRand <- random
              let amount = min (account.feelsSOLBalance * 0.5) (50.0 + amountRand * 200.0)
              pure $ ExitProtocol account.id amount FeelsSOL
            else pure $ WaitBlocks 1
      else if actionRoll < 0.8
        then do
          -- Create staking position for token creation/launch
          let recentTokens = getRecentlyCreatedTokens state.actionHistory
          if length recentTokens == 0
            then pure $ WaitBlocks 1  -- No tokens to stake
            else do
              -- Pick a random token to stake
              tokenIndex <- randomInt 0 (max 0 (length recentTokens - 1))
              case head (drop tokenIndex recentTokens) of
                Just tokenTicker -> do
                  if account.feelsSOLBalance >= 100.0
                    then do
                      -- Stake exactly 100 FeelsSOL to help launch token
                      pure $ CreateLendOffer account.id FeelsSOL 100.0 (Token tokenTicker) 100.0 (weeklyTerm state.currentBlock) (Just tokenTicker)
                    else pure $ WaitBlocks 1
                Nothing -> pure $ WaitBlocks 1
        else pure $ WaitBlocks 1

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- Extract recently created tokens from action history
getRecentlyCreatedTokens :: Array TradingAction -> Array String
getRecentlyCreatedTokens actions = 
  foldl extractToken [] actions
  where
    extractToken acc action = case action of
      CreateToken _ ticker _ -> ticker : acc
      _ -> acc
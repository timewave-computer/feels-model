-- | Core Types for Simulation Engine
-- |
-- | This module contains fundamental types shared across simulation modules
-- | to avoid circular dependencies between Action, Analysis, and other modules.
module Simulation.Types
  ( TradingAction(..)
  ) where

import Prelude
import Data.Maybe (Maybe(..))

-- Core protocol system imports
import Protocol.Token (TokenType)
import Protocol.Position (Duration)
import Utils (formatAmount)

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
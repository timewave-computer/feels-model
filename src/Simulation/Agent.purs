-- | Agent Profiles and Management for Simulation Engine
-- |
-- | This module defines simulated market participants with distinct behavioral
-- | patterns and risk profiles. Each agent type represents a different class
-- | of DeFi user, from conservative investors to high-frequency arbitrageurs.
-- |
-- | Agent Types:
-- | - Conservative: Risk-averse users with stable, long-term positions
-- | - Moderate: Balanced traders with measured risk/reward approach
-- | - Aggressive: High-risk, high-reward active traders
-- | - Arbitrageur: Sophisticated traders exploiting price inefficiencies
-- | - Whale: Large capital holders with significant market impact
-- | - Retail: Small-scale frequent traders representing typical DeFi users
-- |
-- | Balance Distribution:
-- | - Realistic initial capital allocation based on agent behavior profiles
-- | - Ranges from 50-250 JitoSOL (Retail) to 10k-100k JitoSOL (Whale)
-- | - Reflects actual DeFi participation patterns and capital distribution
module Simulation.Agent
  ( AccountProfile(..)
  , SimulatedAccount
  , generateAccounts
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (range, length, head, drop)
import Data.Functor (map)
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Random (random, randomInt)

import Utils (formatAmount)

--------------------------------------------------------------------------------
-- AGENT BEHAVIOR PROFILE DEFINITIONS
--------------------------------------------------------------------------------
-- Core types defining different classes of simulated market participants

-- | Comprehensive agent behavior profiles representing different DeFi user types
-- | Each profile determines trading patterns, risk tolerance, and capital allocation
data AccountProfile
  = Conservative    -- Risk-averse users: stable positions, low turnover, long-term holds
  | Moderate       -- Balanced traders: diversified approach, measured risk-taking
  | Aggressive     -- High-risk traders: frequent rebalancing, leveraged positions
  | Arbitrageur    -- Sophisticated users: exploit price differences, high-frequency trading
  | Whale          -- Large capital holders: significant market impact, strategic positioning
  | Retail         -- Typical users: small frequent trades, trend-following behavior

derive instance eqAccountProfile :: Eq AccountProfile

instance showAccountProfile :: Show AccountProfile where
  show Conservative = "Conservative"
  show Moderate = "Moderate"  
  show Aggressive = "Aggressive"
  show Arbitrageur = "Arbitrageur"
  show Whale = "Whale"
  show Retail = "Retail"

--------------------------------------------------------------------------------
-- SIMULATED ACCOUNT DATA STRUCTURE
--------------------------------------------------------------------------------
-- Complete account state tracking for simulation participants

-- | Complete simulated account with balance tracking and performance metrics
-- | Maintains realistic state for each agent throughout simulation lifecycle
type SimulatedAccount =
  { id :: String                    -- Unique account identifier
  , profile :: AccountProfile       -- Agent behavior classification
  , jitoSOLBalance :: Number         -- JitoSOL holdings (staked SOL)
  , feelsSOLBalance :: Number        -- FeelsSOL holdings (protocol token)
  , activePositions :: Array Int     -- Currently held position IDs
  , totalDeposited :: Number         -- Cumulative deposits into protocol
  , totalWithdrawn :: Number         -- Cumulative withdrawals from protocol
  , netPnL :: Number                 -- Net profit/loss from all activities
  }

--------------------------------------------------------------------------------
-- AGENT GENERATION CONFIGURATION
--------------------------------------------------------------------------------
-- Configuration parameters for creating realistic agent populations

-- | Configuration parameters for generating diverse agent populations
-- | Controls the number and distribution of different agent types
type AgentConfig =
  { numAccounts :: Int                      -- Total number of accounts to generate
  , accountProfiles :: Array AccountProfile  -- Available profile types for assignment
  }

--------------------------------------------------------------------------------
-- AGENT POPULATION GENERATION ENGINE
--------------------------------------------------------------------------------
-- Core functions for creating realistic simulated user populations

-- | Generate diverse array of simulated accounts with realistic capital distribution
-- | Creates agents with profile-appropriate initial balances and clean state
generateAccounts :: AgentConfig -> Effect (Array SimulatedAccount)
generateAccounts config = do
  let numAccounts = config.numAccounts
  let profiles = config.accountProfiles
  
  -- Generate sequential user IDs for tracking
  accountIds <- sequence $ map (\i -> pure $ "user" <> show i) (range 1 numAccounts)
  
  -- Create individual accounts with randomized profiles and balances
  traverse (generateAccount profiles) accountIds
  where
    -- | Generate single account with randomized profile and appropriate initial balance
    -- | Balance ranges reflect realistic DeFi participation patterns
    generateAccount profiles userId = do
      -- Randomly assign profile from available options
      profileIndex <- randomInt 0 (length profiles - 1)
      let profile = case head (drop profileIndex profiles) of
            Just p -> p
            Nothing -> Moderate  -- Default fallback for safety
      
      -- Generate profile-appropriate initial balance
      -- Balance distributions reflect real DeFi user capital patterns
      baseBalance <- random
      let multiplier = case profile of
            Whale -> 10000.0 + baseBalance * 90000.0        -- 10k-100k JitoSOL: Institutional/high net worth
            Arbitrageur -> 5000.0 + baseBalance * 15000.0   -- 5k-20k JitoSOL: Professional traders
            Aggressive -> 1000.0 + baseBalance * 9000.0     -- 1k-10k JitoSOL: Active retail traders
            Moderate -> 500.0 + baseBalance * 2000.0        -- 500-2.5k JitoSOL: Typical DeFi users
            Conservative -> 100.0 + baseBalance * 900.0     -- 100-1k JitoSOL: Conservative investors
            Retail -> 50.0 + baseBalance * 200.0            -- 50-250 JitoSOL: Small retail participants
      
      -- Create account with clean initial state
      pure { id: userId
           , profile: profile
           , jitoSOLBalance: multiplier
           , feelsSOLBalance: 0.0              -- No protocol tokens initially
           , activePositions: []               -- No positions at start
           , totalDeposited: 0.0               -- No historical deposits
           , totalWithdrawn: 0.0               -- No historical withdrawals
           , netPnL: 0.0                       -- No profit/loss initially
           }
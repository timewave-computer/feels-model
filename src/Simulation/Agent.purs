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
  -- Preference types
  , AgentPreferences
  , TimeTolerance
  , LeverageTolerance
  , LiquidityTolerance
  -- Preference constructors
  , defaultPreferences
  , conservativePreferences
  , aggressivePreferences
  , balancedPreferences
  -- Preference utilities
  , sampleFromPreferences
  , getPositionProbability
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (range, length, head, drop)
import Data.Functor (map)
import Data.Traversable (traverse, sequence)
import Effect (Effect)
import Effect.Random (random, randomInt)

import Utils (formatAmount)
import Protocol.PositionVault (Duration(..), Leverage(..))
import Protocol.Token (TokenType(..))

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
-- 3D PREFERENCE MODEL TYPES
--------------------------------------------------------------------------------
-- Three-dimensional preference system for agent decision-making

-- | 3D agent preference model replacing single juniorTranchePreference
-- | Each dimension represents independent risk tolerances
type AgentPreferences =
  { timeTolerances :: TimeTolerance       -- Duration preferences
  , leverageTolerance :: LeverageTolerance  -- Leverage tier preferences  
  , liquidityTolerance :: LiquidityTolerance -- Asset type preferences
  }

-- | Time tolerance preferences across duration options
-- | Flash = Low tolerance (immediate settlement)
-- | Monthly = High tolerance (28-day lock)
-- | Spot = Infinite tolerance (theoretical, no commitment)
type TimeTolerance =
  { flashProbability :: Number     -- Low time tolerance (0.0-1.0)
  , monthlyProbability :: Number   -- High time tolerance (0.0-1.0)
  , spotProbability :: Number      -- Infinite tolerance (0.0-1.0)
  }

-- | Leverage tolerance preferences between tiers
-- | Senior = Low tolerance (1x exposure)
-- | Junior = High tolerance (3x exposure)
type LeverageTolerance =
  { seniorProbability :: Number    -- Low leverage tolerance (0.0-1.0)
  , juniorProbability :: Number    -- High leverage tolerance (0.0-1.0)
  }

-- | Liquidity tolerance preferences for lending assets
-- | FeelsSOL = Low tolerance (highly liquid synthetic SOL)
-- | Token = High tolerance (potentially illiquid project tokens)
type LiquidityTolerance =
  { feelsSOLProbability :: Number  -- Low liquidity tolerance (0.0-1.0)
  , tokenProbability :: Number     -- High liquidity tolerance (0.0-1.0)
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

--------------------------------------------------------------------------------
-- PREFERENCE CONSTRUCTORS
--------------------------------------------------------------------------------
-- Pre-defined preference profiles for different agent types

-- | Default balanced preferences
defaultPreferences :: AgentPreferences
defaultPreferences =
  { timeTolerances:
      { flashProbability: 0.2      -- 20% flash loans
      , monthlyProbability: 0.3    -- 30% term commitments
      , spotProbability: 0.5       -- 50% perpetual positions
      }
  , leverageTolerance:
      { seniorProbability: 0.6     -- 60% conservative leverage
      , juniorProbability: 0.4     -- 40% aggressive leverage
      }
  , liquidityTolerance:
      { feelsSOLProbability: 0.7   -- 70% prefer liquid assets
      , tokenProbability: 0.3      -- 30% accept illiquid tokens
      }
  }

-- | Conservative agent preferences (low risk tolerance)
conservativePreferences :: AgentPreferences
conservativePreferences =
  { timeTolerances:
      { flashProbability: 0.5      -- 50% prefer immediate settlement
      , monthlyProbability: 0.1    -- 10% accept term locks
      , spotProbability: 0.4       -- 40% perpetual positions
      }
  , leverageTolerance:
      { seniorProbability: 0.9     -- 90% low leverage
      , juniorProbability: 0.1     -- 10% high leverage
      }
  , liquidityTolerance:
      { feelsSOLProbability: 0.95  -- 95% require liquidity
      , tokenProbability: 0.05     -- 5% accept illiquid
      }
  }

-- | Aggressive agent preferences (high risk tolerance)
aggressivePreferences :: AgentPreferences
aggressivePreferences =
  { timeTolerances:
      { flashProbability: 0.1      -- 10% flash loans
      , monthlyProbability: 0.6    -- 60% term commitments for yield
      , spotProbability: 0.3       -- 30% perpetual positions
      }
  , leverageTolerance:
      { seniorProbability: 0.2     -- 20% conservative
      , juniorProbability: 0.8     -- 80% leveraged positions
      }
  , liquidityTolerance:
      { feelsSOLProbability: 0.3   -- 30% liquid preference
      , tokenProbability: 0.7      -- 70% accept illiquid for yield
      }
  }

-- | Balanced preferences (medium risk across all dimensions)
balancedPreferences :: AgentPreferences
balancedPreferences =
  { timeTolerances:
      { flashProbability: 0.33     -- Equal distribution
      , monthlyProbability: 0.34
      , spotProbability: 0.33
      }
  , leverageTolerance:
      { seniorProbability: 0.5     -- Equal leverage preference
      , juniorProbability: 0.5
      }
  , liquidityTolerance:
      { feelsSOLProbability: 0.5   -- Equal liquidity preference
      , tokenProbability: 0.5
      }
  }

--------------------------------------------------------------------------------
-- PREFERENCE SAMPLING AND UTILITIES
--------------------------------------------------------------------------------
-- Functions for using preferences in simulations

-- | Sample a position configuration from agent preferences
-- | Returns (Duration, Leverage, TokenType) based on preference probabilities
sampleFromPreferences :: AgentPreferences -> Effect { duration :: Duration, leverage :: Leverage, lendAsset :: TokenType }
sampleFromPreferences prefs = do
  -- Sample duration
  timeRoll <- random
  let duration = 
        if timeRoll < prefs.timeTolerances.flashProbability
          then Flash
        else if timeRoll < (prefs.timeTolerances.flashProbability + prefs.timeTolerances.monthlyProbability)
          then Monthly
        else Spot
  
  -- Sample leverage
  leverageRoll <- random
  let leverage = 
        if leverageRoll < prefs.leverageTolerance.seniorProbability
          then Senior
        else Junior
  
  -- Sample lending asset
  liquidityRoll <- random
  let lendAsset =
        if liquidityRoll < prefs.liquidityTolerance.feelsSOLProbability
          then FeelsSOL
        else Token "USDC"  -- Default to USDC for now, should be parameterized
  
  pure { duration, leverage, lendAsset }

-- | Calculate probability of a specific position configuration
-- | Multiplies probabilities across all three dimensions
getPositionProbability :: AgentPreferences -> Duration -> Leverage -> TokenType -> Number
getPositionProbability prefs duration leverage lendAsset =
  let
    timeProbability = case duration of
      Flash -> prefs.timeTolerances.flashProbability
      Monthly -> prefs.timeTolerances.monthlyProbability
      Spot -> prefs.timeTolerances.spotProbability
    
    leverageProbability = case leverage of
      Senior -> prefs.leverageTolerance.seniorProbability
      Junior -> prefs.leverageTolerance.juniorProbability
    
    liquidityProbability = case lendAsset of
      FeelsSOL -> prefs.liquidityTolerance.feelsSOLProbability
      _ -> prefs.liquidityTolerance.tokenProbability
  in
    timeProbability * leverageProbability * liquidityProbability
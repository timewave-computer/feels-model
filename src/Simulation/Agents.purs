-- Agent Profiles and Management for Simulation Engine
-- Defines different types of simulated market participants and their behaviors
module Simulation.Agents
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
-- Agent Profile Types
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

--------------------------------------------------------------------------------
-- Agent Generation
--------------------------------------------------------------------------------

-- Configuration type needed for generation (minimal subset)
type AgentConfig =
  { numAccounts :: Int
  , accountProfiles :: Array AccountProfile
  }

-- Generate array of simulated accounts based on configuration
generateAccounts :: AgentConfig -> Effect (Array SimulatedAccount)
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
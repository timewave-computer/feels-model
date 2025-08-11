-- | Main state module for the Feels Protocol.
-- | This module provides initialization and re-exports all state functionality
-- | to maintain backward compatibility.
module State.State
  ( -- Re-export all types
    module State.Types
    -- Re-export command functions
  , module State.Commands
    -- Re-export query functions  
  , module State.Queries
    -- Local functions
  , initState
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (new)

-- Import and re-export types
import State.Types (AppState, AppRuntime, AppCommand(..), AppQuery(..), AppResult(..))

-- Import and re-export command functions
import State.Commands (executeCommand, captureRebaseDifferential)

-- Import and re-export query functions
import State.Queries (executeQuery)

-- Import modules needed for initialization
import Token (initTokenRegistry, TokenType(..))
import PoolRegistry (initPoolRegistry, addPool)
import Data.Map as Map
import Tick (createTick)
import POL (initPOL, contribute)
import Oracle (initOracle)
import Incentives (initMarketDynamics)
import Accounts (initAccountRegistry)
import Gateway (initGateway)
import FFI (currentTime)
import Launch.Orchestrator (initOrchestrator)

--------------------------------------------------------------------------------
-- State Initialization
--------------------------------------------------------------------------------

-- | Initialize state with empty state
initState :: Effect AppRuntime
initState = do
  -- Initialize all subsystems
  tokenRegistry <- initTokenRegistry
  poolRegistry <- initPoolRegistry
  polState <- initPOL
  oracle <- initOracle 1.22  -- Initialize with JitoSOL price
  marketDynamics <- initMarketDynamics oracle polState
  
  -- Initialize accounts with some demo data for the user
  accounts <- initAccountRegistry
  
  -- Initialize launch orchestrator
  launchOrchestrator <- initOrchestrator
  
  -- Initialize gateway
  let priceOracle = pure 1.22  -- JitoSOL/SOL = 1.22 (current market price)
  gateway <- initGateway priceOracle 0.001 0.002 accounts polState
  
  -- Initialize POL with initial contribution
  -- These contributions help bootstrap the system
  contribute polState 150.0     -- Initial 150 FeelsSOL POL total
  
  -- Note: No demo trades - the market starts clean
  -- User tokens will deploy their own POL liquidity when created
  
  -- Create a default pool for MVP
  let defaultPool = 
        { pair: { base: FeelsSOL, quote: JitoSOL }
        , tickBook: 
          { ticks: [createTick 1.0 1000.0]  -- Single tick at price 1.0
          , tickSpacing: 0.01
          , activeTicks: 1
          }
        , aggregate:
          { spot: 1.0
          , twap: 1.0
          , volatility: 0.0
          , volume24h: 0.0
          , depth: 1000.0
          , feeRate: 0.001
          }
        , positions:
          { positionIds: []
          , totalManaged: 0.0
          , totalSpot: 0.0
          , lockedValue: 0.0
          }
        , managed:
          { total: 0.0
          , senior: { amount: 0.0, shares: 0.0, multiplier: 1.0 }
          , junior: { amount: 0.0, shares: 0.0, multiplier: 3.0 }
          , polAllocation: 0.0
          , polUtilized: 0.0
          , distribution: 0.0
          , termBuckets: Map.empty
          }
        , issuance:
          { feelsSolOracle: 1.0
          , tokenSupply: 0.0
          , lastIssuance: 0.0
          , issuanceMetrics:
            { demandPressure: 0.0
            , velocity: 0.0
            , depthRatio: 0.0
            , priceDeviation: 0.0
            }
          , controller:
            { maxSupplyGrowth: 0.1
            , minLiquidity: 100.0
            , targetUtilization: 0.8
            , priceStabilization: true
            }
          }
        , trancheMetrics:
          { seniorTotal: 0.0
          , juniorTotal: 0.0
          , juniorMultiplier: 3.0
          , totalExposure: 0.0
          , juniorHealth: 1.0
          }
        }
  
  addPool "FeelsSOL/DEFAULT" defaultPool poolRegistry
  
  -- Get current timestamp and initial block
  timestamp <- currentTime
  let initialBlock = 1000  -- Start at block 1000 for testing
  
  -- Create initial state
  let initialState =
        { tokenRegistry
        , poolRegistry
        , gateway
        , polState
        , oracle
        , marketDynamics
        , accounts
        , launchOrchestrator
        , positionTokenMap: []
        , currentUser: "user1"
        , currentBlock: initialBlock
        , timestamp
        , lastJitoSOLPrice: 1.22  -- Initial JitoSOL price
        }
  
  stateRef <- new initialState
  listenersRef <- new []
  nextListenerIdRef <- new 0
  
  pure { state: stateRef, listeners: listenersRef, nextListenerId: nextListenerIdRef }
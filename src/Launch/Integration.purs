-- | LaunchIntegration Module - Connects launch system to pools and positions
-- |
-- | This module handles the integration between batch auction winners and
-- | the position/pool system, routing priority fees to POL.
module Launch.Integration
  ( processBatchWinners
  , createLaunchPool
  , routeFeesToPOL
  ) where

import Prelude
import Data.Either (Either(..))
import Control.Monad (when)
import Data.Traversable (traverse, sequence)
import Effect (Effect)

import Launch.Launch (BatchResult, Bid, LaunchPhase(..))
import Position (Position, TermCommitment(..), Tranche(..), spotTerm, weeklyTerm, dailyTerm, hourlyTerm)
import Position as P
import PoolRegistry (PoolRegistry, addPosition, getNextPositionId, addPool)
import Pool (PoolState)
import POL (POLState, contribute)
import Common (PoolId, BlockNumber)
import Token (TokenType(..), FeelsSOLAmount(..))
import Errors (ProtocolError(..))
import Data.Map as Map
import Tick (createTick)

--------------------------------------------------------------------------------
-- Pool Creation
--------------------------------------------------------------------------------

-- | Create a pool for a newly launched token
createLaunchPool :: 
  String ->           -- Token ticker
  PoolId ->           -- Pool ID
  Number ->           -- Initial price from launch
  PoolRegistry ->
  Effect (Either ProtocolError Unit)
createLaunchPool ticker poolId initialPrice poolRegistry = do
  -- Create pool state for the launched token
  let pool = 
        { pair: { base: FeelsSOL, quote: Token ticker }
        , tickBook: 
          { ticks: [createTick initialPrice 1000.0]  -- Single tick at discovered price
          , tickSpacing: 0.01
          , activeTicks: 1
          }
        , aggregate:
          { spot: initialPrice
          , twap: initialPrice
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
          { feelsSolOracle: initialPrice
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
  
  addPool poolId pool poolRegistry
  pure $ Right unit

--------------------------------------------------------------------------------
-- Batch Processing Integration
--------------------------------------------------------------------------------

-- | Process batch winners and create positions
processBatchWinners ::
  BatchResult ->
  PoolId ->
  LaunchPhase ->
  BlockNumber ->
  PoolRegistry ->
  Effect (Either ProtocolError (Array Position))
processBatchWinners batchResult poolId phase currentBlock poolRegistry = do
  -- Map each winning bid to a position
  positions <- traverse (createPositionFromBid poolId phase currentBlock poolRegistry) batchResult.winners
  
  -- Return array of created positions
  case sequence positions of
    Left err -> pure $ Left err
    Right pos -> pure $ Right pos

-- | Create a position from a winning bid
createPositionFromBid ::
  PoolId ->
  LaunchPhase ->
  BlockNumber ->
  PoolRegistry ->
  Bid ->
  Effect (Either ProtocolError Position)
createPositionFromBid poolId phase currentBlock poolRegistry bid = do
  -- Get next position ID
  posId <- getNextPositionId poolRegistry
  
  -- Determine term commitment based on launch phase
  let term = case phase of
        WeeklyPhase -> weeklyTerm currentBlock
        DailyPhase -> dailyTerm currentBlock  
        HourlyPhase -> hourlyTerm currentBlock
        _ -> spotTerm  -- Spot phase or completed
  
  -- Create position (all launch positions are Senior tranche)
  let position = P.createPosition
        posId
        poolId
        bid.bidder
        bid.baseAmount  -- Amount is just the base amount
        Senior          -- Launch positions are always Senior
        term
        bid.baseAmount  -- Shares = amount for launch positions
        currentBlock
  
  -- Add position to registry
  addPosition position poolRegistry
  
  pure $ Right position

--------------------------------------------------------------------------------
-- Fee Routing
--------------------------------------------------------------------------------

-- | Route priority fees from batch to POL
routeFeesToPOL ::
  BatchResult ->
  POLState ->
  Effect Unit
routeFeesToPOL batchResult polState = do
  -- All priority fees go to POL
  when (batchResult.protocolRevenue > 0.0) $ do
    contribute polState batchResult.protocolRevenue


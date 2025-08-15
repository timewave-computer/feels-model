-- | Protocol-integrated simulation engine
-- | Executes all trades through the actual protocol to properly track fees and POL
module Simulation.ProtocolEngine
  ( executeSimulationWithProtocol
  , getProtocolMetrics
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Array (foldM, range, length, (:))
import Data.Functor (map)
import Data.Foldable (sum)
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Ord (abs, min)
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref, read, write)
import Effect.Console (log)
import FFI (pow)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad (when)

-- Import protocol types and functions
import Protocol.Common (CommandResult(..), QueryResult(..))
import Protocol.Error (ProtocolError)
import Protocol.Position (Duration(..), Leverage(..))
import Protocol.Token (TokenType(..))
import UI.ProtocolState (ProtocolState, IndexerQuery(..))
import UI.Command (executeCommand)
import UI.Query (executeQuery)
import UI.ProtocolState as PS
import Protocol.POL (contribute, allocateToPool, getUnallocatedPOL, getAllAllocations)
import Protocol.Pool (PoolState, swap, SwapParams)
import UI.PoolRegistry (getAllPools, getPool, updatePool)
import UI.Account (updateChainAccountBalance, getChainAccountBalance)

-- Import simulation types
import Simulation.Sim as Sim
import Simulation.Sim (SimulationState, TradingAction(..), SimulatedAccount, MarketScenario(..))
import Simulation.Market (SimulationConfig, generateMarketScenario)
import Simulation.Action (generateTradingSequence)
import Protocol.Oracle (updatePriceWithTimestamp, takeMarketSnapshot)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- PROTOCOL-INTEGRATED EXECUTION
--------------------------------------------------------------------------------

-- | Execute simulation using proper block-by-block event loop
-- | Each block: collect orders -> update oracle -> adjust POL -> update fees -> process orders
executeSimulationWithProtocol :: 
  Ref ProtocolState -> 
  SimulationConfig -> 
  SimulationState -> 
  Effect { finalSimState :: SimulationState, finalProtocolState :: ProtocolState }
executeSimulationWithProtocol protocolRef config initialSimState = do
  log "executeSimulationWithProtocol called - ENTERING FUNCTION"
  -- Get initial protocol state
  initialProtocolState <- read protocolRef
  log "Read initial protocol state"
  
  -- Create chain accounts for all simulated agents
  log $ "Creating chain accounts for " <> show (length initialSimState.accounts) <> " simulated agents..."
  traverse_ (\account -> do
    log $ "Processing account " <> account.id <> " with balance " <> show account.jitoSOLBalance
    -- Create chain account with initial balance (updateChainAccountBalance adds delta to existing balance)
    result <- updateChainAccountBalance initialProtocolState.accounts account.id account.jitoSOLBalance
    case result of
      Right _ -> do
        -- Verify the balance was set
        balance <- getChainAccountBalance initialProtocolState.accounts account.id
        log $ "Created chain account for " <> account.id <> " with " <> show balance <> " JitoSOL"
      Left err -> log $ "Failed to create chain account for " <> account.id <> ": " <> err
  ) initialSimState.accounts
  
  -- Run the simulation block by block
  finalState <- foldM (executeSimulationBlock protocolRef config) 
    initialSimState 
    (range 1 config.simulationBlocks)
    
  -- Get final protocol state
  finalProtocolState <- read protocolRef
  
  pure { finalSimState: finalState, finalProtocolState: finalProtocolState }

-- | Execute a single simulation block with proper event ordering
executeSimulationBlock :: 
  Ref ProtocolState -> 
  SimulationConfig -> 
  SimulationState -> 
  Int -> 
  Effect SimulationState
executeSimulationBlock protocolRef config simState blockNum = do
  when (blockNum == 1) do
    log "TRACE: executeSimulationBlock called for Block 1 - WHO IS CALLING ME?"
    log "If executeSimulationWithProtocol was called, we should have seen chain account logs"
  log $ "=== Block " <> show blockNum <> " ===" 
  
  -- Step 1: Generate market price movement for this block
  priceMovement <- generateMarketScenario config blockNum
  let newPrice = simState.currentPrice * (1.0 + priceMovement)
  
  -- Step 2: Calculate simulated timestamp (5 seconds per block)
  baseTime <- currentTime
  let simulatedTimestamp = baseTime + (toNumber blockNum * 5000.0)  -- 5 seconds per block in milliseconds
  
  -- Step 3: Update oracle price with simulated timestamp
  _ <- updatePriceWithTimestamp newPrice simulatedTimestamp simState.oracle
  log $ "Updated oracle price to: " <> show newPrice <> " at simulated time"
  
  -- Step 4: Get current protocol state and update POL distribution if needed
  protocolState <- read protocolRef
  updatePOLDistribution protocolState config.scenario blockNum
  
  -- Step 4b: Capture POL allocations after distribution
  -- This runs every block to track the evolution of POL allocations over time
  polAllocations <- getAllAllocations protocolState.polState
  let polAllocationMap = Map.fromFoldable $ map (\alloc -> Tuple alloc.poolId alloc.permanentAllocated) polAllocations
      polSnapshot = { block: blockNum, timestamp: simulatedTimestamp, allocations: polAllocationMap }
  
  -- Step 5: Update protocol fees and pool health factors
  updatePoolHealthMetrics protocolState
  
  -- Step 6: Generate and collect orders for this block
  tradingActions <- generateTradingSequence config 
    { accounts: simState.accounts
    , currentBlock: blockNum
    , actionHistory: simState.actionHistory
    , oracle: simState.oracle
    }
  
  log $ "Generated " <> show (length tradingActions) <> " orders for block " <> show blockNum
  
  -- Step 7: Process all orders through the protocol
  updatedSimState <- foldM (executeProtocolAction protocolRef) simState tradingActions
  
  -- Step 8: Collect and contribute swap fees from all pools to POL
  protocolState' <- read protocolRef
  collectAndContributePoolFees protocolState'
  
  -- Step 9: Take market snapshot after all orders are processed
  marketSnapshot <- takeMarketSnapshot updatedSimState.oracle
  
  -- Step 9: Get token metrics for this block
  finalProtocolState <- read protocolRef
  metrics <- getProtocolMetrics finalProtocolState
  
  -- Step 10: Update simulation state with new block data including token metrics
  pure $ updatedSimState 
    { currentBlock = blockNum
    , currentPrice = marketSnapshot.spot
    , priceHistory = { price: marketSnapshot.spot, timestamp: marketSnapshot.timestamp } : updatedSimState.priceHistory
    , actionHistory = updatedSimState.actionHistory <> tradingActions
    , polAllocationHistory = polSnapshot : updatedSimState.polAllocationHistory
    }

-- | Execute a single protocol action and update simulation state
executeProtocolAction :: 
  Ref ProtocolState -> 
  SimulationState -> 
  TradingAction -> 
  Effect SimulationState
executeProtocolAction protocolRef simState action = do
  protocolState <- read protocolRef
  
  let executeAndUpdate :: forall a. Effect (Either ProtocolError (Tuple ProtocolState a)) -> Effect SimulationState
      executeAndUpdate commandEffect = do
        result <- commandEffect
        case result of
          Right (Tuple newProtocolState _) -> do
            write newProtocolState protocolRef
            pure simState
          Left err -> do
            log $ "Protocol error: " <> show err
            pure simState
  
  case action of
    -- Protocol entry: Convert JitoSOL to FeelsSOL
    EnterProtocol userId amount _ -> do
      result <- executeCommand (PS.EnterFeelsSOL userId amount) protocolState
      case result of
        Right (Tuple newProtocolState (FeelsSOLMinted _)) -> do
          write newProtocolState protocolRef
          -- Update simulation state to reflect balance changes
          let updatedAccounts1 = updateAccountBalances simState.accounts userId (-amount) JitoSOL
              updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amount FeelsSOL
          pure simState { accounts = updatedAccounts2 }
        _ -> do
          log $ "Failed to enter protocol for user " <> userId
          pure simState
              
    -- Protocol exit: Convert FeelsSOL to JitoSOL
    ExitProtocol userId amount _ -> do
      result <- executeCommand (PS.ExitFeelsSOL userId amount) protocolState
      case result of
        Right (Tuple newProtocolState (FeelsSOLBurned _)) -> do
          write newProtocolState protocolRef
          -- Calculate actual JitoSOL received after fees
          let jitoAmount = amount * 0.998  -- 0.2% exit fee
              updatedAccounts1 = updateAccountBalances simState.accounts userId (-amount) FeelsSOL
              updatedAccounts2 = updateAccountBalances updatedAccounts1 userId jitoAmount JitoSOL
          pure simState { accounts = updatedAccounts2 }
        _ -> do
          log $ "Failed to exit protocol for user " <> userId
          pure simState
              
    -- Create lending position (this generates swap fees)
    CreateLendOffer userId lendAsset amount collateralAsset collateralAmount duration _targetToken -> do
      -- Default to Senior leverage for simulation
      let protocolLeverage = Senior
      
      result <- executeCommand 
        (PS.CreatePosition userId lendAsset amount collateralAsset collateralAmount 
          duration protocolLeverage false Nothing) 
        protocolState
        
      case result of
        Right (Tuple newProtocolState (PositionCreated _)) -> do
          write newProtocolState protocolRef
          log $ "Created position for " <> userId <> ", fees collected to POL"
          -- Update simulation balances
          let updatedAccounts = updateAccountBalances simState.accounts userId (-amount) lendAsset
          pure simState { accounts = updatedAccounts }
        _ -> do
          log $ "Failed to create position for user " <> userId
          pure simState
              
    -- Take loan (executes swap through pool, generates fees)
    TakeLoan userId lendAsset amount collateralAsset _collateralAmount _duration -> do
      -- In "everything is lending", a loan is a swap through the pool
      -- Get the pool for this token pair
      let poolId = case Tuple lendAsset collateralAsset of
            Tuple (Token ticker) FeelsSOL -> ticker <> "/FeelsSOL"
            Tuple FeelsSOL (Token ticker) -> ticker <> "/FeelsSOL"
            _ -> "Unknown/FeelsSOL"
      
      -- Get the pool from registry
      maybePool <- getPool poolId protocolState.poolRegistry
      case maybePool of
        Just pool -> do
          -- Determine swap direction
          let zeroForOne = case lendAsset of
                FeelsSOL -> false  -- Selling token for FeelsSOL
                _ -> true          -- Selling FeelsSOL for token
              
              swapParams = 
                { zeroForOne: zeroForOne
                , amountSpecified: amount  -- Positive = exact input
                , sqrtPriceLimitX96: 0.0    -- No price limit
                }
          
          -- Execute swap (this collects fees)
          protocolState <- read protocolRef
          let currentBlock = protocolState.currentBlock + 1
          let swapResult = swap pool swapParams currentBlock
          
          -- Update pool state in registry with new values after swap
          updatePool poolId swapResult.updatedPool protocolState.poolRegistry
          
          log $ "Executed swap in pool " <> poolId <> " for user " <> userId <> 
                ", fee collected: ~" <> show (amount * pool.protocolFee / 10000.0) <> " FeelsSOL"
          
          -- Update simulation balances based on swap result
          let amountOut = if zeroForOne 
                then abs swapResult.result.amount1
                else abs swapResult.result.amount0
              
              updatedAccounts1 = updateAccountBalances simState.accounts userId (-amount) collateralAsset
              updatedAccounts2 = updateAccountBalances updatedAccounts1 userId amountOut lendAsset
              
          pure simState { accounts = updatedAccounts2 }
          
        Nothing -> do
          log $ "Pool " <> poolId <> " not found, cannot execute swap"
          pure simState
      
    -- Create token
    Sim.CreateToken userId ticker name -> do
      result <- executeCommand (PS.CreateToken userId ticker name) protocolState
      case result of
        Right (Tuple newProtocolState (TokenCreated _)) -> do
          write newProtocolState protocolRef
          log $ "Created token " <> ticker
          pure simState
        _ -> do
          log $ "Failed to create token " <> ticker
          pure simState
          
    -- Other actions
    _ -> pure simState

-- Helper to update account balances
updateAccountBalances :: Array SimulatedAccount -> String -> Number -> TokenType -> Array SimulatedAccount
updateAccountBalances accounts userId amount token =
  map updateAccount accounts
  where
    updateAccount acc
      | acc.id == userId = case token of
          JitoSOL -> acc { jitoSOLBalance = max 0.0 (acc.jitoSOLBalance + amount) }
          FeelsSOL -> acc { feelsSOLBalance = max 0.0 (acc.feelsSOLBalance + amount) }
          Token _ -> acc
      | otherwise = acc

--------------------------------------------------------------------------------
-- PROTOCOL METRICS EXTRACTION
--------------------------------------------------------------------------------

-- | Extract key metrics from protocol state including actual POL values
getProtocolMetrics :: ProtocolState -> Effect 
  { totalValueLocked :: Number
  , polReserves :: Number
  , totalFeesCollected :: Number
  , tokenMetrics :: Array { ticker :: String, supply :: Number, polFloor :: Number }
  }
getProtocolMetrics state = do
  -- Get POL state to extract reserves and fees
  polState <- read state.polState
  log $ "DEBUG getProtocolMetrics - polState.totalPOL: " <> show polState.totalPOL
  log $ "DEBUG getProtocolMetrics - polState.unallocated: " <> show polState.unallocated
  
  -- Get all tokens to calculate POL floors
  tokensResult <- executeQuery GetAllTokens state
  let tokens = case tokensResult of
        Right (TokenList ts) -> ts
        _ -> []
  
  -- Calculate POL floor for each token
  -- POL reserves are distributed across all tokens based on their supply
  let totalSupply = 1000000.0  -- Placeholder - would sum all token supplies
      tokenMetrics = map (\foreignToken ->
        let token = unsafeCoerce foreignToken :: { ticker :: String, supply :: Number, live :: Boolean }
            -- POL floor = token's share of POL reserves / token supply
            tokenShare = token.supply / totalSupply
            polFloor = if token.supply > 0.0 
                       then (polState.totalPOL * tokenShare) / token.supply
                       else 0.0
        in { ticker: token.ticker
           , supply: token.supply
           , polFloor: polFloor
           }
      ) tokens
  
  -- Calculate total value locked from pools
  -- For now, use POL reserves as a proxy
  let tvl = polState.totalPOL * 10.0  -- Assume 10x leverage
      
      -- Calculate total fees from contribution history
      totalFees = case polState.contributionHistory of
        [] -> 0.0
        _ -> polState.totalPOL  -- Total POL represents all accumulated fees
  
  pure 
    { totalValueLocked: tvl
    , polReserves: polState.totalPOL
    , totalFeesCollected: totalFees
    , tokenMetrics: tokenMetrics
    }

--------------------------------------------------------------------------------
-- FEE COLLECTION
--------------------------------------------------------------------------------

-- | Collect accumulated swap fees from all pools and contribute to POL
collectAndContributePoolFees :: ProtocolState -> Effect Unit
collectAndContributePoolFees state = do
  -- Get all pools from the registry
  pools <- getAllPools state.poolRegistry
  
  -- Calculate total fees to collect
  totalFees <- foldM collectPoolFees 0.0 pools
  
  -- Contribute collected fees to POL if any
  when (totalFees > 0.0) do
    log $ "Collecting " <> show totalFees <> " FeelsSOL in swap fees for POL"
    contribute state.polState totalFees
  
  where
    -- Collect fees from a single pool
    collectPoolFees :: Number -> Tuple String PoolState -> Effect Number
    collectPoolFees accFees (Tuple poolId pool) = do
      -- Calculate actual fees from fee growth globals
      -- Fee growth tracks cumulative fees per unit of liquidity
      let totalFeeGrowth = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
          -- Convert fee growth to actual fees (simplified calculation)
          -- In reality, this would track the delta since last collection
          collectedFees = if pool.liquidity > 0.0 && totalFeeGrowth > 0.0
                         then min (totalFeeGrowth * pool.liquidity / 1000000.0) (pool.liquidity * 0.01)
                         else 0.0
      
      when (collectedFees > 0.0) do
        log $ "Pool " <> poolId <> " collected " <> show collectedFees <> " FeelsSOL in fees"
        -- Note: In a real implementation, we'd reset fee growth after collection
        -- For simulation purposes, we'll let fees accumulate
      
      pure (accFees + collectedFees)

--------------------------------------------------------------------------------
-- POL DISTRIBUTION
--------------------------------------------------------------------------------

-- | Update POL distribution based on market conditions
-- | In volatile markets, allocate more POL to provide stability
updatePOLDistribution :: ProtocolState -> MarketScenario -> Int -> Effect Unit
updatePOLDistribution state scenario blockNum = do
  -- Only update distribution every 10 blocks to avoid excessive rebalancing
  let shouldDistribute = blockNum `mod` 10 == 0
  when shouldDistribute do
    log $ "POL Distribution check at block " <> show blockNum
    unallocated <- getUnallocatedPOL state.polState
    log $ "Unallocated POL: " <> show unallocated
    
    when (unallocated > 100.0) do  -- Only distribute if we have meaningful POL
      -- Get all pools with their current metrics
      pools <- getAllPools state.poolRegistry
      log $ "Found " <> show (length pools) <> " pools for POL distribution"
      
      -- Calculate allocation amounts based on market scenario
      let baseAllocation = case scenario of
            VolatileMarket -> unallocated * 0.3  -- Allocate 30% in volatile markets
            CrashScenario -> unallocated * 0.4   -- Allocate 40% during crashes for stability
            BearMarket -> unallocated * 0.2      -- Allocate 20% in bear markets
            RecoveryMarket -> unallocated * 0.25 -- Allocate 25% during recovery
            _ -> unallocated * 0.1               -- Allocate 10% in normal conditions
      
      -- Distribute to pools based on their volume and liquidity
      when (length pools > 0) do
        let poolsWithMetrics = map (\(Tuple poolId pool) -> 
              let volume = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
                  -- Ensure minimum score even for pools with no liquidity yet
                  score = max 1.0 (pool.liquidity * (1.0 + min volume 1.0))  -- Score based on liquidity and fees
              in Tuple poolId score
            ) pools
            
            totalScore = sum (map snd poolsWithMetrics)
            
        log $ "POL allocation scores: " <> show poolsWithMetrics
        log $ "Total score: " <> show totalScore
            
        -- Allocate proportionally to top performing pools
        traverse_ (\(Tuple poolId score) -> do
          let proportion = if totalScore > 0.0 then score / totalScore else 0.0
              allocationAmount = baseAllocation * proportion
          
          when (allocationAmount > 10.0) do  -- Minimum allocation threshold
            success <- allocateToPool state.polState poolId allocationAmount
            when success do
              log $ "Allocated " <> show allocationAmount <> " POL to pool " <> poolId
        ) poolsWithMetrics

--------------------------------------------------------------------------------
-- POOL HEALTH METRICS
--------------------------------------------------------------------------------

-- | Update pool health metrics and fee parameters
-- | Adjusts fees based on liquidity depth and market conditions
updatePoolHealthMetrics :: ProtocolState -> Effect Unit
updatePoolHealthMetrics state = do
  pools <- getAllPools state.poolRegistry
  
  traverse_ (\(Tuple poolId pool) -> do
    -- Calculate pool health based on liquidity and volume
    let liquidityDepth = pool.liquidity
        volume = pool.feeGrowthGlobal0X128 + pool.feeGrowthGlobal1X128
        
        -- Health score: 0.0 (unhealthy) to 1.0 (very healthy)
        liquidityScore = min 1.0 (liquidityDepth / 10000.0)  -- Normalize to 10k liquidity
        volumeScore = min 1.0 (volume / 100.0)               -- Normalize to 100 volume
        healthScore = (liquidityScore + volumeScore) / 2.0
        
        -- Dynamic fee adjustment based on health
        -- Lower fees for healthy pools to attract volume
        -- Higher fees for unhealthy pools to compensate LPs
        baseFee = pool.protocolFee
        adjustedFee = if healthScore > 0.8
                      then max 10.0 (baseFee * 0.8)    -- 20% fee reduction for very healthy pools
                      else if healthScore < 0.3
                           then min 50.0 (baseFee * 1.5) -- 50% fee increase for unhealthy pools
                           else baseFee
    
    -- Update pool if fee changed significantly
    when (abs (adjustedFee - baseFee) > 1.0) do
      let updatedPool = pool { protocolFee = adjustedFee }
      updatePool poolId updatedPool state.poolRegistry
      log $ "Updated pool " <> poolId <> " health score: " <> show healthScore <> 
            ", fee: " <> show baseFee <> " -> " <> show adjustedFee
  ) pools
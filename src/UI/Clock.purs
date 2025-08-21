-- | UI Clock Module - Frontend wrapper for Protocol Clock
-- |
-- | This module provides a bridge between the UI's ProtocolState and the
-- | Protocol Clock's event loop. It handles the conversion between the
-- | full UI state and the minimal Protocol state needed for block processing.
module UI.Clock
  ( runProtocolBlock
  ) where

import Prelude
import Data.Array ((:))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Effect (Effect)
import Effect.Ref (Ref, read, modify_)
import Effect.Console (log)

-- Import UI types
import UI.ProtocolState (ProtocolState)
import UI.PoolRegistry (getAllPools, getPool, updatePool, getPositionsRef)

-- Import Protocol clock
import Protocol.Clock as ProtocolClock

-- | Execute a single protocol block using UI ProtocolState
-- | This wraps the Protocol.Clock.runProtocolBlock to work with UI state
runProtocolBlock :: 
  Ref ProtocolState -> 
  Number ->              -- Oracle price for this block
  Int ->                 -- Block number
  Effect { timestamp :: Number, allocations :: Map.Map String Number }
runProtocolBlock protocolRef oraclePrice blockNum = do
  log $ "\n=== UI Protocol Block " <> show blockNum <> " ==="
  
  -- Read the current protocol state
  protocolState <- read protocolRef
  
  -- Get all pools from the registry
  pools <- getAllPools protocolState.poolRegistry
  
  -- Get positions as a separate ref
  positionsRef <- getPositionsRef protocolState.poolRegistry
  
  -- Create the params record that Protocol.Clock expects
  let params = 
        { oracle: protocolState.oracle
        , polState: protocolState.polState
        , pools: pools
        }
  
  -- Call the Protocol.Clock function
  result <- ProtocolClock.runProtocolBlock params oraclePrice blockNum
  
  -- Update the UI protocol state with the block number and timestamp
  modify_ (\s -> s 
    { currentBlock = blockNum
    , timestamp = result.timestamp
    , priceHistory = { timestamp: result.timestamp
                     , block: blockNum
                     , price: oraclePrice
                     , polValue: 0.0
                     } : s.priceHistory
    }) protocolRef
  
  log $ "UI Protocol block " <> show blockNum <> " completed"
  
  pure result
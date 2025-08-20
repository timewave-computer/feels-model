-- | Protocol Clock Module - Simplified
module Protocol.Clock
  ( runProtocolBlock
  , ProtocolBlockResult
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array ((:))
import Data.Map as Map
import Effect (Effect)
import Effect.Console (log)
import Protocol.ProtocolVault (POLState, getAllAllocations)
import Protocol.Pool (Pool)
import Protocol.Oracle (Oracle, updatePriceWithTimestamp)
import FFI (currentTime)
import Data.Int (toNumber)

-- | Result of running a protocol block
type ProtocolBlockResult =
  { timestamp :: Number
  , allocations :: Map.Map String Number
  }

-- | Execute a single protocol block (simplified)
runProtocolBlock :: 
  { oracle :: Oracle
  , polState :: POLState
  , pools :: Array (Tuple String Pool)
  } ->
  Number ->              -- Oracle price for this block
  Int ->                 -- Block number
  Effect ProtocolBlockResult
runProtocolBlock params oraclePrice blockNum = do
  log $ "\n=== Protocol Block " <> show blockNum <> " ==="
  
  -- Calculate block timestamp
  baseTime <- currentTime
  let blockTimestamp = baseTime + (toNumber blockNum * 5000.0)  -- 5 seconds per block
  
  -- Update oracle price
  updatePriceWithTimestamp oraclePrice blockTimestamp params.oracle
  log $ "Oracle price updated to: " <> show oraclePrice
  
  -- Get POL allocations
  let allocations = getAllAllocations params.polState
  
  log $ "Protocol block " <> show blockNum <> " completed"
  
  pure { timestamp: blockTimestamp, allocations: allocations }
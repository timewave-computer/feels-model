module Deposit where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import FFI (currentTime)

--------------------------------------------------------------------------------
-- Deposit Layer
--------------------------------------------------------------------------------

-- Deposit component state
-- Tracks jitoSOL deposits and synthetic SOL minting
type DepositState =
  { jitoSOLBalance :: Number         -- Total jitoSOL in deposit vault
  , syntheticSOLSupply :: Number     -- Total synthetic SOL minted
  , exchangeRate :: Number           -- Current SOL/jitoSOL exchange rate
  , lastUpdate :: Number             -- Timestamp of last rate update
  }

-- Initialize deposit state
initDepositState :: DepositState
initDepositState =
  { jitoSOLBalance: 0.0
  , syntheticSOLSupply: 0.0
  , exchangeRate: 1.02  -- jitoSOL typically trades at slight premium (this should be updated by the oracle)
  , lastUpdate: 0.0
  }

-- Deposit jitoSOL and receive synthetic SOL
-- Entry: jitoSOL deposited × (SOL/jitoSOL rate) = synthetic SOL minted
deposit :: Number -> DepositState -> Effect { state :: DepositState, syntheticSOL :: Number }
deposit jitoSOLAmount state = do
  timestamp <- currentTime
  let syntheticMinted = jitoSOLAmount * state.exchangeRate
  let newState = state
        { jitoSOLBalance = state.jitoSOLBalance + jitoSOLAmount
        , syntheticSOLSupply = state.syntheticSOLSupply + syntheticMinted
        , lastUpdate = timestamp
        }
  log $ "Deposited " <> show jitoSOLAmount <> " jitoSOL, minted " <> show syntheticMinted <> " synthetic SOL"
  pure { state: newState, syntheticSOL: syntheticMinted }

-- Withdraw jitoSOL by burning synthetic SOL
-- Exit: synthetic SOL burned / (SOL/jitoSOL rate) = jitoSOL returned
withdraw :: Number -> DepositState -> Effect { state :: DepositState, jitoSOL :: Number }
withdraw syntheticSOLAmount state = do
  timestamp <- currentTime
  let jitoSOLReturned = syntheticSOLAmount / state.exchangeRate
  if jitoSOLReturned > state.jitoSOLBalance
    then do
      log $ "Insufficient jitoSOL balance for withdrawal"
      pure { state: state, jitoSOL: 0.0 }
    else do
      let newState = state
            { jitoSOLBalance = state.jitoSOLBalance - jitoSOLReturned
            , syntheticSOLSupply = state.syntheticSOLSupply - syntheticSOLAmount
            , lastUpdate = timestamp
            }
      log $ "Burned " <> show syntheticSOLAmount <> " synthetic SOL, returned " <> show jitoSOLReturned <> " jitoSOL"
      pure { state: newState, jitoSOL: jitoSOLReturned }

-- Update exchange rate (would come from oracle in production)
updateExchangeRate :: Number -> DepositState -> Effect DepositState
updateExchangeRate newRate state = do
  timestamp <- currentTime
  let newState = state { exchangeRate = newRate, lastUpdate = timestamp }
  log $ "Updated SOL/jitoSOL exchange rate to " <> show newRate
  pure newState

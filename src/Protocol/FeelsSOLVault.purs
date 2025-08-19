-- | FeelsSOL Vault Implementation - JitoSOL → FeelsSOL synthetic token system
-- |
-- | This module implements the FeelsSOL system as a vault where:
-- | - JitoSOL is deposited as the underlying asset
-- | - FeelsSOL is issued as the share token
-- | - Oracle pricing determines the exchange rate
-- | - Buffer management ensures withdrawal liquidity
-- |
-- | Key Features:
-- | - JitoSOL backing with oracle-based pricing
-- | - Withdrawal buffer for instant liquidity
-- | - Entry/exit fees configurable by governance
-- | - Solvency monitoring and rebalancing
module Protocol.FeelsSOLVault
  ( -- Core types
    FeelsSOLVault
  , FeelsSOLStrategy
  , FeelsSOLState
  , OraclePrice
  , MintResult
  , BurnResult
  -- Vault creation
  , createFeelsSOLVault
  -- Balance sheet operations
  , feelsSOLSharePrice
  -- State management
  , updateOraclePrice
  -- Functions
  , allocateToBuffer
  , useBufferForWithdrawal
  , feelsSOLAllocationStrategy
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array ((:), filter, find)
import Data.Foldable (foldr, sum)
import Data.Number (abs)
import Unsafe.Coerce (unsafeCoerce)
import Protocol.Vault (Vault, VaultState, BalanceSheet, Asset, Liability, deposit, withdraw, allocate, totalAssets)
import Protocol.Token (TokenType(..))
import Protocol.Common (BlockNumber)
import Protocol.Error (ProtocolError(..))
import FFI (currentTime)

--------------------------------------------------------------------------------
-- SECTION 1: BALANCE SHEET
--------------------------------------------------------------------------------

-- | Oracle price information
type OraclePrice =
  { price :: Number      -- JitoSOL/SOL exchange rate
  , timestamp :: Number  -- When price was fetched
  }

-- | Result of minting FeelsSOL
type MintResult =
  { feelsSOLMinted :: Number
  , jitoSOLLocked :: Number
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- | Result of burning FeelsSOL
type BurnResult =
  { feelsSOLBurned :: Number
  , jitoSOLReleased :: Number
  , exchangeRate :: Number
  , fee :: Number
  , timestamp :: Number
  }

-- | FeelsSOL share price based on oracle pricing
-- | 1 FeelsSOL = oracle price amount of JitoSOL
feelsSOLSharePrice :: FeelsSOLStrategy -> BalanceSheet -> Number
feelsSOLSharePrice strategy bs =
  -- In the FeelsSOL system, the share price is determined by the oracle
  -- This ensures 1 FeelsSOL always equals the oracle price in JitoSOL value
  case strategy.cachedPrice of
    Just oracle -> oracle.price
    Nothing -> 1.05  -- Default price if no oracle data yet

--------------------------------------------------------------------------------
-- SECTION 2: STATE
--------------------------------------------------------------------------------

-- | FeelsSOL strategy state
type FeelsSOLStrategy =
  { priceOracle :: Effect Number           -- JitoSOL/SOL price oracle
  , lastOracleUpdate :: Number             -- Last oracle update timestamp
  , cachedPrice :: Maybe OraclePrice       -- Cached oracle price
  , entryFee :: Number                     -- Fee for JitoSOL → FeelsSOL conversion
  , exitFee :: Number                      -- Fee for FeelsSOL → JitoSOL conversion
  , polAllocationRate :: Number            -- Portion of fees allocated to POL system
  , bufferTargetRatio :: Number            -- Target withdrawal buffer ratio
  , jitoSOLBuffer :: Number                -- JitoSOL buffer for withdrawals
  }

-- | FeelsSOL vault type
type FeelsSOLVault = Vault FeelsSOLStrategy

-- | Legacy FeelsSOL state type (for compatibility)
type FeelsSOLState = Ref FeelsSOLVault

-- | Update oracle price with caching
updateOraclePrice :: FeelsSOLStrategy -> Effect FeelsSOLStrategy
updateOraclePrice strategy = do
  currentTime' <- currentTime
  
  -- Use cached price if less than 1 minute old
  case strategy.cachedPrice of
    Just cached | (currentTime' - strategy.lastOracleUpdate) < 60000.0 -> 
      pure strategy
    _ -> do
      -- Fetch fresh price from oracle
      price <- strategy.priceOracle
      let newPrice = { price: price, timestamp: currentTime' }
      
      pure strategy 
        { cachedPrice = Just newPrice
        , lastOracleUpdate = currentTime'
        }

--------------------------------------------------------------------------------
-- SECTION 3: FUNCTIONS
--------------------------------------------------------------------------------

-- | Create a new FeelsSOL vault
createFeelsSOLVault :: String -> FeelsSOLStrategy -> Effect (Ref FeelsSOLVault)
createFeelsSOLVault name initialStrategy = do
  -- Initial empty balance sheet
  let initialBalanceSheet =
        { assets: []
        , liabilities: []
        , totalShares: 0.0
        }
      
      -- Initial vault state
      initialState =
        { balanceSheet: initialBalanceSheet
        , strategyState: initialStrategy
        , lastUpdateBlock: 0
        }
      
      -- FeelsSOL-specific share price function
      feelsSOLVaultSharePrice :: BalanceSheet -> Number
      feelsSOLVaultSharePrice = feelsSOLSharePrice initialStrategy
  
  -- Create a ref to hold the vault
  vaultRef <- new (unsafeCoerce unit :: FeelsSOLVault)
  
  -- Create vault implementation
  let vault =
        { name: name
        , state: initialState
        
        -- Deposit implementation (only accepts JitoSOL)
        , deposit: \tokenType amount depositor -> do
            case tokenType of
              JitoSOL -> do
                currentVault <- read vaultRef
                
                -- Update oracle price before deposit
                updatedStrategy <- updateOraclePrice currentVault.state.strategyState
                let updatedState = currentVault.state { strategyState = updatedStrategy }
                    
                    -- Calculate fee
                    feeAmount = amount * updatedStrategy.entryFee
                    netAmount = amount - feeAmount
                    
                    -- Use custom share price function that uses oracle
                    customSharePrice = feelsSOLSharePrice updatedStrategy
                    result = deposit customSharePrice updatedState tokenType netAmount depositor
                    
                    -- Allocate to buffer if needed
                    finalState = allocateToBuffer result.state netAmount updatedStrategy.bufferTargetRatio
                
                modify_ (\v -> v { state = finalState }) vaultRef
                pure result.shares  -- This is the amount of FeelsSOL minted
              _ -> pure 0.0  -- Only accept JitoSOL
        
        -- Withdraw implementation (burn FeelsSOL, return JitoSOL)
        , withdraw: \shares withdrawer -> do
            currentVault <- read vaultRef
            
            -- Update oracle price before withdrawal
            updatedStrategy <- updateOraclePrice currentVault.state.strategyState
            let updatedState = currentVault.state { strategyState = updatedStrategy }
                customSharePrice = feelsSOLSharePrice updatedStrategy
            
            case withdraw customSharePrice updatedState shares withdrawer of
              Nothing -> pure 0.0
              Just result -> do
                -- Calculate exit fee on the JitoSOL amount
                let feeAmount = result.amount * updatedStrategy.exitFee
                    netAmount = result.amount - feeAmount
                    
                    -- Use buffer first if available
                    finalState = useBufferForWithdrawal result.state result.amount
                
                modify_ (\v -> v { state = finalState }) vaultRef
                pure netAmount  -- Return net JitoSOL after fee
        
        -- Allocation strategy (manages buffer)
        , allocate: do
            modify_ (\v -> 
              let strategy = v.state.strategyState
                  newState = allocate v.state (feelsSOLAllocationStrategy strategy.bufferTargetRatio)
              in v { state = newState }
            ) vaultRef
        
        -- Strategy update
        , updateStrategy: \newStrategy -> do
            modify_ (\v -> v { state = v.state { strategyState = newStrategy } }) vaultRef
        
        -- Share price implementation
        , sharePrice: feelsSOLVaultSharePrice
        }
  
  -- Write the vault to the ref
  _ <- write vault vaultRef
  pure vaultRef

-- | Allocate portion of deposit to buffer based on target ratio
allocateToBuffer :: forall a. VaultState a -> Number -> Number -> VaultState a
allocateToBuffer vaultState _ _ = vaultState  -- Simplified for now

-- | Use buffer for withdrawal if available
useBufferForWithdrawal :: forall a. VaultState a -> Number -> VaultState a
useBufferForWithdrawal vaultState _ = vaultState  -- Simplified for now

-- | FeelsSOL allocation strategy for buffer management
feelsSOLAllocationStrategy :: Number -> FeelsSOLStrategy -> Array Asset -> Array Asset
feelsSOLAllocationStrategy targetBufferRatio _ assets =
  let
    -- Calculate total JitoSOL
    totalJitoSOL = sum $ map _.amount $ filter (\a -> a.tokenType == JitoSOL) assets
    
    -- Determine buffer allocation
    bufferAmount = totalJitoSOL * targetBufferRatio
    backingAmount = totalJitoSOL - bufferAmount
    
    -- Create allocated assets
    backingAsset = { tokenType: JitoSOL, amount: backingAmount, venue: "FeelsSOL-Backing" }
    bufferAsset = { tokenType: JitoSOL, amount: bufferAmount, venue: "FeelsSOL-Buffer" }
    
  in [backingAsset, bufferAsset]
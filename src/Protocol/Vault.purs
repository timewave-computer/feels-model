-- | Vault Abstraction - Core balance sheet and strategy framework
-- |
-- | This module provides a minimal vault abstraction that can be used as a foundation
-- | for various protocol components. A vault accepts capital (assets) and issues shares
-- | (liabilities) in return, maintaining a balance sheet and executing allocation strategies.
-- |
-- | Key concepts:
-- | - Assets: Capital deposited into the vault
-- | - Liabilities: Shares issued to depositors  
-- | - State: Internal state for allocation strategy
-- | - Functions: Operations to manage capital and execute strategy
-- | - Dimensions: Vaults can be 1D (price only) or 3D (price, duration, leverage)
module Protocol.Vault
  ( -- Core types
    Vault
  , VaultState
  , BalanceSheet
  , Asset
  , Liability
  , ShareAmount
  -- Vault dimensions
  , class VaultDimension
  , Dimension1D(..)
  , Dimension3D(..)
  , getDimensionInfo
  -- Result types
  , DepositResult
  , WithdrawResult
  , WithdrawRequest
  , WithdrawRequestResult
  -- Vault operations
  , deposit
  , withdraw
  , requestWithdraw
  , processWithdrawRequest
  , allocate
  , updateStrategy
  -- Balance sheet queries
  , totalAssets
  , totalLiabilities
  , netAssetValue
  , defaultSharePrice
  -- State management
  , getVaultState
  , updateVaultState
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array ((:), find, filter)
import Data.Foldable (sum)
import Effect (Effect)
import Protocol.Token (TokenType)
import Protocol.Common (BlockNumber)
import Protocol.PositionVault (Duration(..), Leverage(..))

--------------------------------------------------------------------------------
-- CORE TYPES
--------------------------------------------------------------------------------

-- | Represents an asset held by the vault
type Asset =
  { tokenType :: TokenType
  , amount :: Number
  , venue :: String              -- Where the asset is deployed (e.g., "POL", "Pool-X")
  }

-- | Represents a liability (shares issued) with dimensional information
type Liability d =
  { holder :: String             -- Address of share holder
  , shares :: ShareAmount        -- Number of shares held
  , depositBlock :: BlockNumber  -- When shares were issued
  , maturityBlock :: Maybe BlockNumber -- Optional: when shares can be withdrawn
  , locked :: Boolean            -- Whether shares are locked until maturity
  , dimensions :: d              -- Dimensional information (1D or 3D)
  }

-- | Share amount type for type safety
type ShareAmount = Number

-- | Balance sheet tracking assets and liabilities
type BalanceSheet d =
  { assets :: Array Asset        -- All assets held by vault
  , liabilities :: Array (Liability d) -- All shares issued with dimensions
  , totalShares :: ShareAmount   -- Total shares outstanding
  }

-- | Generic vault state that can be extended by specific implementations
type VaultState d a =
  { balanceSheet :: BalanceSheet d
  , strategyState :: a           -- Strategy-specific state
  , lastUpdateBlock :: BlockNumber
  }

-- | Core vault abstraction parameterized by dimension type
type Vault d a =
  { name :: String               -- Vault identifier
  , state :: VaultState d a      -- Current vault state
  -- Core vault functions
  , deposit :: TokenType -> Number -> String -> d -> Effect ShareAmount
  , withdraw :: ShareAmount -> String -> Effect Number
  , allocate :: Effect Unit      -- Execute allocation strategy
  , updateStrategy :: a -> Effect Unit -- Update strategy parameters
  -- Customizable share price calculation
  , sharePrice :: BalanceSheet d -> Number  -- Allows under/over-collateralization
  }

--------------------------------------------------------------------------------
-- BALANCE SHEET OPERATIONS
--------------------------------------------------------------------------------

-- | Calculate total assets value
totalAssets :: forall d. BalanceSheet d -> Number
totalAssets bs = sum $ map _.amount bs.assets

-- | Calculate total liabilities (shares * price)
totalLiabilities :: forall d. BalanceSheet d -> Number
totalLiabilities bs = bs.totalShares

-- | Calculate net asset value (assets - liabilities)
netAssetValue :: forall d. BalanceSheet d -> Number
netAssetValue bs = totalAssets bs

-- | Default share price calculation (assets / shares)
-- | Specific vault implementations can override this in their sharePrice function
-- | to implement under-collateralization or over-collateralization strategies
defaultSharePrice :: forall d. BalanceSheet d -> Number
defaultSharePrice bs = 
  if bs.totalShares > 0.0
  then totalAssets bs / bs.totalShares
  else 1.0  -- Initial share price

--------------------------------------------------------------------------------
-- VAULT OPERATIONS
--------------------------------------------------------------------------------

-- | Result of deposit operation
type DepositResult d a = 
  { state :: VaultState d a
  , shares :: ShareAmount 
  }

-- | Result of withdraw operation
type WithdrawResult d a = 
  { state :: VaultState d a
  , amount :: Number 
  }

-- | Represents a pending withdrawal request
type WithdrawRequest =
  { holder :: String             -- Address requesting withdrawal
  , shares :: ShareAmount        -- Number of shares to burn
  , requestBlock :: BlockNumber  -- When withdrawal was requested
  , maturityBlock :: BlockNumber -- When withdrawal can be processed
  }

-- | Result of withdraw request operation
type WithdrawRequestResult d a = 
  { state :: VaultState d a
  , request :: WithdrawRequest
  }

-- | Deposit assets into vault and receive shares
deposit :: forall d a. (BalanceSheet d -> Number) -> VaultState d a -> TokenType -> Number -> String -> d -> DepositResult d a
deposit sharePriceFn vaultState tokenType amount depositor dimensions =
  let
    -- Calculate shares to mint based on vault's share price function
    currentSharePrice = sharePriceFn vaultState.balanceSheet
    sharesToMint = amount / currentSharePrice
    
    -- Create new asset entry
    newAsset = { tokenType, amount, venue: "unallocated" }
    
    -- Create new liability entry with dimensions
    newLiability = { holder: depositor, shares: sharesToMint, depositBlock: vaultState.lastUpdateBlock, maturityBlock: Nothing, locked: false, dimensions: dimensions }
    
    -- Update balance sheet
    updatedBalanceSheet = vaultState.balanceSheet
      { assets = newAsset : vaultState.balanceSheet.assets
      , liabilities = newLiability : vaultState.balanceSheet.liabilities
      , totalShares = vaultState.balanceSheet.totalShares + sharesToMint
      }
    
    -- Return updated state and shares minted
    updatedState = vaultState { balanceSheet = updatedBalanceSheet }
    
  in { state: updatedState, shares: sharesToMint }

-- | Withdraw assets from vault by burning shares
withdraw :: forall d a. (BalanceSheet d -> Number) -> VaultState d a -> ShareAmount -> String -> Maybe (WithdrawResult d a)
withdraw sharePriceFn vaultState sharesToBurn withdrawer =
  let
    -- Find user's liability
    userLiability = find (\l -> l.holder == withdrawer) vaultState.balanceSheet.liabilities
    
  in case userLiability of
    Nothing -> Nothing
    Just liability ->
      if liability.shares < sharesToBurn
      then Nothing
      else
        let
          -- Calculate amount to return based on vault's share price function
          currentSharePrice = sharePriceFn vaultState.balanceSheet
          amountToReturn = sharesToBurn * currentSharePrice
          
          -- Update liabilities
          updatedLiabilities = map (\l -> 
            if l.holder == withdrawer
            then l { shares = l.shares - sharesToBurn }
            else l
          ) vaultState.balanceSheet.liabilities
          
          -- Remove assets proportionally
          totalAssetValue = totalAssets vaultState.balanceSheet
          removalRatio = amountToReturn / totalAssetValue
          
          updatedAssets = map (\a -> 
            a { amount = a.amount * (1.0 - removalRatio) }
          ) vaultState.balanceSheet.assets
          
          -- Update balance sheet
          updatedBalanceSheet = vaultState.balanceSheet
            { assets = updatedAssets
            , liabilities = filter (\l -> l.shares > 0.0) updatedLiabilities
            , totalShares = vaultState.balanceSheet.totalShares - sharesToBurn
            }
          
          -- Return updated state and amount
          updatedState = vaultState { balanceSheet = updatedBalanceSheet }
          
        in Just { state: updatedState, amount: amountToReturn }

-- | Execute vault's allocation strategy
-- | This is implemented by specific vault types
allocate :: forall d a. VaultState d a -> (a -> Array Asset -> Array Asset) -> VaultState d a
allocate vaultState strategyFn =
  let
    -- Apply strategy function to current assets
    newAssets = strategyFn vaultState.strategyState vaultState.balanceSheet.assets
    
    -- Update balance sheet with new asset allocation
    updatedBalanceSheet = vaultState.balanceSheet { assets = newAssets }
    
  in vaultState { balanceSheet = updatedBalanceSheet }

-- | Update vault's strategy parameters
updateStrategy :: forall d a. VaultState d a -> a -> VaultState d a
updateStrategy vaultState newStrategy = 
  vaultState { strategyState = newStrategy }

--------------------------------------------------------------------------------
-- STATE QUERIES
--------------------------------------------------------------------------------

-- | Request withdrawal with optional lock period
requestWithdraw :: forall d a. VaultState d a -> ShareAmount -> String -> BlockNumber -> Maybe BlockNumber -> WithdrawRequestResult d a
requestWithdraw vaultState sharesToBurn withdrawer currentBlock maturityBlock =
  let
    -- Find user's liability
    userLiability = find (\l -> l.holder == withdrawer) vaultState.balanceSheet.liabilities
    
    -- Determine maturity block
    maturity = case maturityBlock of
      Just mb -> mb
      Nothing -> currentBlock  -- Immediate withdrawal if no maturity specified
    
    -- Create withdrawal request
    request = 
      { holder: withdrawer
      , shares: sharesToBurn
      , requestBlock: currentBlock
      , maturityBlock: maturity
      }
    
    -- Update liability to mark shares as locked if future maturity
    updatedLiabilities = map (\l -> 
      if l.holder == withdrawer && maturity > currentBlock
      then l { locked = true, maturityBlock = Just maturity }
      else l
    ) vaultState.balanceSheet.liabilities
    
    updatedBalanceSheet = vaultState.balanceSheet { liabilities = updatedLiabilities }
    updatedState = vaultState { balanceSheet = updatedBalanceSheet }
    
  in { state: updatedState, request: request }

-- | Process a withdrawal request after maturity
processWithdrawRequest :: forall d a. (BalanceSheet d -> Number) -> VaultState d a -> WithdrawRequest -> BlockNumber -> Maybe (WithdrawResult d a)
processWithdrawRequest sharePriceFn vaultState request currentBlock =
  if request.maturityBlock > currentBlock
  then Nothing  -- Not yet mature
  else withdraw sharePriceFn vaultState request.shares request.holder

-- | Get current vault state
getVaultState :: forall d a. Vault d a -> VaultState d a
getVaultState vault = vault.state

-- | Update vault state
updateVaultState :: forall d a. Vault d a -> VaultState d a -> Vault d a
updateVaultState vault newState = vault { state = newState }

--------------------------------------------------------------------------------
-- VAULT DIMENSIONS
--------------------------------------------------------------------------------

-- | Type class for vault dimensions
class VaultDimension d where
  getDimensionInfo :: d -> String

-- | 1D vault dimension - just price
data Dimension1D = Dimension1D 
  { price :: Number              -- Price level for liquidity
  }

instance vaultDimension1D :: VaultDimension Dimension1D where
  getDimensionInfo (Dimension1D d) = "Price: " <> show d.price

-- | 3D vault dimension - price, duration, leverage
data Dimension3D = Dimension3D
  { price :: Number              -- Price level for liquidity
  , duration :: Duration         -- Time commitment (Flash, Monthly, Spot)
  , leverage :: Leverage         -- Risk tier (Senior 1x, Junior 3x)
  }

instance vaultDimension3D :: VaultDimension Dimension3D where
  getDimensionInfo (Dimension3D d) = 
    "Price: " <> show d.price <> 
    ", Duration: " <> show d.duration <> 
    ", Leverage: " <> show d.leverage

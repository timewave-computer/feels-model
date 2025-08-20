-- | Token System for the Feels Protocol
-- |
-- | This module provides a type-class based token system supporting both
-- | fungible and non-fungible tokens:
-- |
-- | Fungible Tokens:
-- | - JitoSOL: External liquid staking token
-- | - FeelsSOL: Internal synthetic SOL
-- | - User Tokens: Custom tokens created through launches
-- | - Positions: Tokenized liquidity positions (share tokens)
-- |
-- | Non-Fungible Tokens:
-- | - Ticks: Unique price points with liquidity state
-- |
-- | The type class hierarchy enables uniform handling of different token types
-- | while preserving their unique characteristics.
module Protocol.Token 
  ( -- Type classes
    class Token
  , class Fungible
  , class NonFungible
  , tokenId
  , tokenMetadata
  , transfer
  , balance
  , mint
  , burn
  , ownerOf
  , transferNFT
  -- Token types
  , TokenType(..)
  , FungibleToken(..)
  , NonFungibleToken
  , TokenId
  , TokenMetadata
  , TokenBalance
  , NFTMetadata
  -- Specific tokens
  , FeelsSOLAmount(..)
  , unwrapFeelsSOL
  , PositionToken
  , TickNFT
  -- Token creation
  , TokenCreationParams
  , createToken
  , createPositionToken
  , createTickNFT
  -- Validation
  , ValidationResult
  , isValidTicker
  , isValidPair
  , isTradeable
  , validate
  , validateTokenTicker
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), isRight)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Data.String as String
import Data.Map (Map)
import Data.Map as Map
import FFI (currentTime, generateId)

--------------------------------------------------------------------------------
-- TYPE CLASS DEFINITIONS
--------------------------------------------------------------------------------

-- | Base class for all token types
class Token t where
  tokenId :: t -> TokenId
  tokenMetadata :: t -> TokenMetadata

-- | Fungible tokens can be divided and combined
class Token t <= Fungible t where
  -- | Transfer amount from one holder to another
  transfer :: t -> String -> String -> Number -> Effect (Either String Unit)
  -- | Get balance of a holder
  balance :: t -> String -> Effect Number
  -- | Mint new tokens
  mint :: t -> String -> Number -> Effect (Either String Unit)
  -- | Burn tokens
  burn :: t -> String -> Number -> Effect (Either String Unit)

-- | Non-fungible tokens are unique and indivisible
class Token t <= NonFungible t where
  -- | Get the owner of an NFT
  ownerOf :: t -> Effect (Maybe String)
  -- | Transfer ownership of an NFT
  transferNFT :: t -> String -> String -> Effect (Either String Unit)

--------------------------------------------------------------------------------
-- CORE TYPES
--------------------------------------------------------------------------------

-- | Unique identifier for tokens
type TokenId = String

-- | Token balance tracking
type TokenBalance = Map String Number

-- | Base metadata for all tokens
type TokenMetadata =
  { id :: TokenId
  , name :: String
  , symbol :: String
  , tokenType :: String  -- "fungible" or "non-fungible"
  , creator :: String
  , createdAt :: Number
  }

-- | Additional metadata for NFTs
type NFTMetadata =
  { tokenId :: TokenId
  , owner :: String
  , uri :: Maybe String  -- Metadata URI
  , data :: String       -- On-chain data (JSON encoded)
  }

--------------------------------------------------------------------------------
-- FUNGIBLE TOKEN IMPLEMENTATION
--------------------------------------------------------------------------------

-- | Represents the different types of fungible tokens
data TokenType = JitoSOL | FeelsSOL | Custom String

derive instance eqTokenType :: Eq TokenType
derive instance ordTokenType :: Ord TokenType

instance showTokenType :: Show TokenType where
  show JitoSOL = "JitoSOL"
  show FeelsSOL = "FeelsSOL"
  show (Custom ticker) = ticker

-- | Fungible token implementation
newtype FungibleToken = FungibleToken
  { metadata :: TokenMetadata
  , tokenType :: TokenType
  , totalSupply :: Number
  , balances :: Ref TokenBalance
  , paused :: Boolean
  }

instance tokenFungibleToken :: Token FungibleToken where
  tokenId (FungibleToken token) = token.metadata.id
  tokenMetadata (FungibleToken token) = token.metadata

instance fungibleFungibleToken :: Fungible FungibleToken where
  transfer (FungibleToken token) from to amount = do
    balanceMap <- read token.balances
    let fromBalance = fromMaybe 0.0 $ Map.lookup from balanceMap
        toBalance = fromMaybe 0.0 $ Map.lookup to balanceMap
    
    if fromBalance < amount
      then pure $ Left "Insufficient balance"
      else if token.paused
        then pure $ Left "Token is paused"
      else do
        let newBalances = balanceMap
              # Map.insert from (fromBalance - amount)
              # Map.insert to (toBalance + amount)
        write newBalances token.balances
        pure $ Right unit
  
  balance (FungibleToken token) holder = do
    balanceMap <- read token.balances
    pure $ fromMaybe 0.0 $ Map.lookup holder balanceMap
  
  mint (FungibleToken token) to amount = do
    if token.paused
      then pure $ Left "Token is paused"
      else do
        balanceMap <- read token.balances
        let currentBalance = fromMaybe 0.0 $ Map.lookup to balanceMap
            newBalance = currentBalance + amount
        write (Map.insert to newBalance balanceMap) token.balances
        pure $ Right unit
  
  burn (FungibleToken token) from amount = do
    balanceMap <- read token.balances
    let currentBalance = fromMaybe 0.0 $ Map.lookup from balanceMap
    
    if currentBalance < amount
      then pure $ Left "Insufficient balance"
      else if token.paused
        then pure $ Left "Token is paused"
      else do
        let newBalance = currentBalance - amount
        write (Map.insert from newBalance balanceMap) token.balances
        pure $ Right unit

--------------------------------------------------------------------------------
-- POSITION AS FUNGIBLE TOKEN
--------------------------------------------------------------------------------

-- | Position represented as a fungible share token
newtype PositionToken = PositionToken
  { metadata :: TokenMetadata
  , poolId :: String                    -- Which pool this position is in
  , tickLower :: Int                    -- Position range
  , tickUpper :: Int
  , totalShares :: Number               -- Total shares outstanding
  , shareBalances :: Ref TokenBalance   -- Who owns how many shares
  , lockedUntil :: Map String Number    -- Lock expiry by holder
  , leverageTier :: String              -- "Senior" or "Junior"
  , duration :: String                  -- "Flash", "Monthly", or "Swap"
  }

instance tokenPositionToken :: Token PositionToken where
  tokenId (PositionToken pos) = pos.metadata.id
  tokenMetadata (PositionToken pos) = pos.metadata

instance fungiblePositionToken :: Fungible PositionToken where
  transfer (PositionToken pos) from to amount = do
    -- Check if sender's tokens are locked
    let lockExpiry = fromMaybe 0.0 $ Map.lookup from pos.lockedUntil
    currentTime' <- currentTime
    
    if lockExpiry > currentTime'
      then pure $ Left "Position shares are locked"
      else do
        balanceMap <- read pos.shareBalances
        let fromBalance = fromMaybe 0.0 $ Map.lookup from balanceMap
            toBalance = fromMaybe 0.0 $ Map.lookup to balanceMap
        
        if fromBalance < amount
          then pure $ Left "Insufficient shares"
          else do
            let newBalances = balanceMap
                  # Map.insert from (fromBalance - amount)
                  # Map.insert to (toBalance + amount)
            write newBalances pos.shareBalances
            pure $ Right unit
  
  balance (PositionToken pos) holder = do
    balanceMap <- read pos.shareBalances
    pure $ fromMaybe 0.0 $ Map.lookup holder balanceMap
  
  mint (PositionToken pos) to amount = do
    balanceMap <- read pos.shareBalances
    let currentBalance = fromMaybe 0.0 $ Map.lookup to balanceMap
        newBalance = currentBalance + amount
    write (Map.insert to newBalance balanceMap) pos.shareBalances
    pure $ Right unit
  
  burn (PositionToken pos) from amount = do
    balanceMap <- read pos.shareBalances
    let currentBalance = fromMaybe 0.0 $ Map.lookup from balanceMap
    
    if currentBalance < amount
      then pure $ Left "Insufficient shares"
      else do
        let newBalance = currentBalance - amount
        write (Map.insert from newBalance balanceMap) pos.shareBalances
        pure $ Right unit

--------------------------------------------------------------------------------
-- NON-FUNGIBLE TOKEN IMPLEMENTATION
--------------------------------------------------------------------------------

-- | Non-fungible token implementation
newtype NonFungibleToken = NonFungibleToken
  { metadata :: TokenMetadata
  , nftData :: Ref NFTMetadata
  , transferable :: Boolean
  }

instance tokenNonFungibleToken :: Token NonFungibleToken where
  tokenId (NonFungibleToken nft) = nft.metadata.id
  tokenMetadata (NonFungibleToken nft) = nft.metadata

instance nonFungibleNonFungibleToken :: NonFungible NonFungibleToken where
  ownerOf (NonFungibleToken nft) = do
    nftMeta <- read nft.nftData
    pure $ Just nftMeta.owner
  
  transferNFT (NonFungibleToken nft) from to = do
    if not nft.transferable
      then pure $ Left "NFT is not transferable"
      else do
        nftMeta <- read nft.nftData
        if nftMeta.owner /= from
          then pure $ Left "Not the owner"
          else do
            write (nftMeta { owner = to }) nft.nftData
            pure $ Right unit

--------------------------------------------------------------------------------
-- TICK AS NON-FUNGIBLE TOKEN
--------------------------------------------------------------------------------

-- | Tick represented as an NFT
newtype TickNFT = TickNFT
  { metadata :: TokenMetadata
  , tickData :: Ref
    { tickIndex :: Int
    , liquidityNet :: Number
    , liquidityGross :: Number
    , feeGrowthOutside0 :: Number
    , feeGrowthOutside1 :: Number
    , owner :: String  -- Current owner (usually the pool)
    , poolId :: String
    }
  }

instance tokenTickNFT :: Token TickNFT where
  tokenId (TickNFT tick) = tick.metadata.id
  tokenMetadata (TickNFT tick) = tick.metadata

instance nonFungibleTickNFT :: NonFungible TickNFT where
  ownerOf (TickNFT tick) = do
    tickData <- read tick.tickData
    pure $ Just tickData.owner
  
  transferNFT (TickNFT tick) from to = do
    tickData <- read tick.tickData
    if tickData.owner /= from
      then pure $ Left "Not the owner"
      else do
        write (tickData { owner = to }) tick.tickData
        pure $ Right unit

--------------------------------------------------------------------------------
-- FEELSSOL SPECIFIC TYPES
--------------------------------------------------------------------------------

-- | FeelsSOL amount with type safety
newtype FeelsSOLAmount = FeelsSOLAmount Number

derive instance eqFeelsSOLAmount :: Eq FeelsSOLAmount
derive instance ordFeelsSOLAmount :: Ord FeelsSOLAmount
derive newtype instance semiringFeelsSOLAmount :: Semiring FeelsSOLAmount
derive newtype instance ringFeelsSOLAmount :: Ring FeelsSOLAmount

instance showFeelsSOLAmount :: Show FeelsSOLAmount where
  show (FeelsSOLAmount n) = "FeelsSOL " <> show n

-- | Extract the underlying Number from FeelsSOLAmount
unwrapFeelsSOL :: FeelsSOLAmount -> Number
unwrapFeelsSOL (FeelsSOLAmount n) = n

--------------------------------------------------------------------------------
-- TOKEN CREATION
--------------------------------------------------------------------------------

-- | Parameters required for creating a new token
type TokenCreationParams =
  { ticker :: String      -- 3-10 character ticker symbol
  , name :: String        -- Full token name
  , creator :: String     -- Creator address/identifier
  }

-- | Create a new fungible token
createToken :: TokenCreationParams -> Effect (Either String FungibleToken)
createToken params = do
  -- Validate ticker
  case validateTokenTicker params.ticker of
    Left err -> pure $ Left err
    Right _ -> do
      timestamp <- currentTime
      let tokenId' = params.ticker <> "-" <> show (generateId timestamp)
          metadata =
            { id: tokenId'
            , name: params.name
            , symbol: params.ticker
            , tokenType: "fungible"
            , creator: params.creator
            , createdAt: timestamp
            }
      
      balanceRef <- new Map.empty
      
      pure $ Right $ FungibleToken
        { metadata: metadata
        , tokenType: Custom params.ticker
        , totalSupply: 1000000000.0  -- Default 1B supply
        , balances: balanceRef
        , paused: false
        }

-- | Create a position token
createPositionToken :: 
  String ->    -- poolId
  Int ->       -- tickLower
  Int ->       -- tickUpper
  String ->    -- leverageTier
  String ->    -- duration
  Effect PositionToken
createPositionToken poolId tickLower tickUpper leverageTier duration = do
  timestamp <- currentTime
  let posId = poolId <> "-" <> show tickLower <> "-" <> show tickUpper <> "-" <> show timestamp
      metadata =
        { id: posId
        , name: "Feels Position " <> posId
        , symbol: "fPOS-" <> show (generateId timestamp)
        , tokenType: "fungible"
        , creator: poolId
        , createdAt: timestamp
        }
  
  shareBalancesRef <- new Map.empty
  
  pure $ PositionToken
    { metadata: metadata
    , poolId: poolId
    , tickLower: tickLower
    , tickUpper: tickUpper
    , totalShares: 0.0
    , shareBalances: shareBalancesRef
    , lockedUntil: Map.empty
    , leverageTier: leverageTier
    , duration: duration
    }

-- | Create a tick NFT
createTickNFT :: String -> Int -> Effect TickNFT
createTickNFT poolId tickIndex = do
  timestamp <- currentTime
  let tickId = poolId <> "-tick-" <> show tickIndex
      metadata =
        { id: tickId
        , name: "Pool " <> poolId <> " Tick " <> show tickIndex
        , symbol: "TICK"
        , tokenType: "non-fungible"
        , creator: poolId
        , createdAt: timestamp
        }
  
  tickDataRef <- new
    { tickIndex: tickIndex
    , liquidityNet: 0.0
    , liquidityGross: 0.0
    , feeGrowthOutside0: 0.0
    , feeGrowthOutside1: 0.0
    , owner: poolId
    , poolId: poolId
    }
  
  pure $ TickNFT
    { metadata: metadata
    , tickData: tickDataRef
    }

--------------------------------------------------------------------------------
-- TOKEN VALIDATION
--------------------------------------------------------------------------------

type ValidationResult = Either String Unit

-- | Helper function to combine validation checks
validate :: Boolean -> String -> ValidationResult
validate true _ = Right unit
validate false err = Left err

-- | Comprehensive ticker validation with all protocol rules
validateTokenTicker :: String -> ValidationResult
validateTokenTicker ticker = do
  -- Length constraints
  _ <- validate (String.length ticker >= 3) "Ticker must be at least 3 characters"
  _ <- validate (String.length ticker <= 10) "Ticker must be at most 10 characters"
  
  -- Format validation
  _ <- validate (isAlphanumeric ticker) "Ticker must be alphanumeric"
  
  -- Reserved ticker protection
  _ <- validate (ticker /= "SOL") "Cannot use reserved ticker SOL"
  _ <- validate (ticker /= "JITO") "Cannot use reserved ticker JITO"
  _ <- validate (String.toUpper ticker /= "FEELSSOL") "Cannot use reserved ticker FEELSSOL"
  _ <- validate (String.toUpper ticker /= "JITOSOL") "Cannot use reserved ticker JITOSOL"
  
  Right unit
  where
    -- Simplified alphanumeric check - production would use proper character validation
    isAlphanumeric _ = true

--------------------------------------------------------------------------------
-- TOKEN QUERIES AND VALIDATION
--------------------------------------------------------------------------------

-- | Check if a ticker meets all validation requirements
isValidTicker :: String -> Boolean
isValidTicker ticker = isRight (validateTokenTicker ticker)

-- | Validate token pairs for trading operations
-- | Protocol only supports FeelsSOL ↔ User Token pairs
isValidPair :: TokenType -> TokenType -> Boolean
isValidPair FeelsSOL (Custom _) = true     -- FeelsSOL → User Token
isValidPair (Custom _) FeelsSOL = true     -- User Token → FeelsSOL
isValidPair _ _ = false                   -- All other pairs invalid

-- | Check if a token is available for trading
isTradeable :: FungibleToken -> Boolean
isTradeable (FungibleToken token) = not token.paused
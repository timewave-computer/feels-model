-- | Safe JSON Codecs for Feels Protocol UI  
-- |
-- | This module provides type-safe JSON encoding/decoding using Simple JSON
-- | to eliminate unsafe Foreign operations throughout the application.
-- | 
-- | REFACTORED: Now uses purescript-simple-json for automatic derivation
-- | and type safety, eliminating ~150-200 lines of unsafe operations.
module UI.Util.Codecs
  ( -- Core data types (now with automatic JSON instances)
    TokenMetadataCodec
  , PositionCodec 
  , LenderOfferCodec
  , LaunchResultCodec
  -- Safe encoding/decoding
  , decodeTokenMetadata
  , encodeTokenMetadata
  , decodePosition
  , encodePosition
  , decodeLenderOffer
  , encodeLenderOffer
  , decodeLaunchResult
  , encodeLaunchResult
  -- Array decoders (now safe)
  , decodeTokenArray
  , decodePositionArray
  , decodeLenderOfferArray
  ) where

import Prelude
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (mapMaybe)
import Foreign (Foreign)
import Data.Generic.Rep (class Generic)
import Simple.JSON as JSON

--------------------------------------------------------------------------------
-- Token Metadata Codec (replaces 8+ unsafeCoerce instances)
--------------------------------------------------------------------------------

-- | Safe token metadata type with automatic JSON derivation
newtype TokenMetadataCodec = TokenMetadataCodec
  { ticker :: String
  , name :: String
  , live :: Boolean
  , owner :: Maybe String
  , totalSupply :: Maybe Number
  }

-- Automatic JSON instances (no manual codec needed)
instance readTokenMetadata :: ReadForeign TokenMetadataCodec where
  readImpl = JSON.readImpl
instance writeTokenMetadata :: WriteForeign TokenMetadataCodec where
  writeImpl = JSON.writeImpl

-- | Decode token metadata safely (automatic)
decodeTokenMetadata :: String -> Either String TokenMetadataCodec
decodeTokenMetadata = readJSON

-- | Encode token metadata safely (automatic)
encodeTokenMetadata :: TokenMetadataCodec -> String
encodeTokenMetadata = writeJSON

--------------------------------------------------------------------------------
-- Position Codec (replaces 5+ unsafeCoerce instances)  
--------------------------------------------------------------------------------

-- | Safe position type with automatic JSON derivation
newtype PositionCodec = PositionCodec
  { id :: Int
  , amount :: Number
  , leverage :: String
  , price :: Number
  , duration :: String
  , shares :: Number
  , value :: Number
  , accumulatedYield :: Number
  , owner :: Maybe String
  }

-- Automatic JSON instances
instance readPosition :: ReadForeign PositionCodec where
  readImpl = JSON.readImpl
instance writePosition :: WriteForeign PositionCodec where
  writeImpl = JSON.writeImpl

-- | Decode position safely (automatic)
decodePosition :: String -> Either String PositionCodec
decodePosition = readJSON

-- | Encode position safely (automatic)
encodePosition :: PositionCodec -> String
encodePosition = writeJSON

--------------------------------------------------------------------------------
-- Lender Offer Codec (replaces 3+ unsafeCoerce instances)
--------------------------------------------------------------------------------

-- | Safe lender offer type with automatic JSON derivation
newtype LenderOfferCodec = LenderOfferCodec
  { id :: Int
  , owner :: String
  , leverage :: String
  , amount :: Number
  , lockedAmount :: Number
  }

-- Automatic JSON instances
instance readLenderOffer :: ReadForeign LenderOfferCodec where
  readImpl = JSON.readImpl
instance writeLenderOffer :: WriteForeign LenderOfferCodec where
  writeImpl = JSON.writeImpl

-- | Decode lender offer safely (automatic)
decodeLenderOffer :: String -> Either String LenderOfferCodec
decodeLenderOffer = readJSON

-- | Encode lender offer safely (automatic)
encodeLenderOffer :: LenderOfferCodec -> String
encodeLenderOffer = writeJSON

--------------------------------------------------------------------------------
-- Launch Result Codec (replaces 2+ unsafeCoerce instances)
--------------------------------------------------------------------------------

-- | Safe launch result type with automatic JSON derivation
newtype LaunchResultCodec = LaunchResultCodec
  { success :: Boolean
  , launchId :: String
  , tokensDistributed :: Number
  , finalPrice :: Number
  , revenue :: Number
  }

-- Automatic JSON instances
instance readLaunchResult :: ReadForeign LaunchResultCodec where
  readImpl = JSON.readImpl
instance writeLaunchResult :: WriteForeign LaunchResultCodec where
  writeImpl = JSON.writeImpl

-- | Decode launch result safely (automatic)
decodeLaunchResult :: String -> Either String LaunchResultCodec
decodeLaunchResult = readJSON

-- | Encode launch result safely (automatic)
encodeLaunchResult :: LaunchResultCodec -> String
encodeLaunchResult = writeJSON

--------------------------------------------------------------------------------
-- Array Decoders (now type-safe with automatic parsing)
--------------------------------------------------------------------------------

-- | Decode array of tokens safely using automatic JSON parsing
decodeTokenArray :: String -> Either String (Array TokenMetadataCodec)
decodeTokenArray = readJSON

-- | Decode array of positions safely using automatic JSON parsing
decodePositionArray :: String -> Either String (Array PositionCodec)
decodePositionArray = readJSON

-- | Decode array of lender offers safely using automatic JSON parsing
decodeLenderOfferArray :: String -> Either String (Array LenderOfferCodec)
decodeLenderOfferArray = readJSON
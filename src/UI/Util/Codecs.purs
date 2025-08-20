-- | Safe JSON Codecs for Feels Protocol UI  
-- |
-- | This module provides type-safe JSON encoding/decoding using Simple JSON
-- | to eliminate unsafe Foreign operations throughout the application.
-- | 
-- | REFACTORED: Now uses purescript-simple-json for automatic derivation
-- | and type safety, eliminating ~150-200 lines of unsafe operations.
module UI.Util.Codecs
  ( -- Core data types (now with automatic JSON instances)
    TokenMetadataCodec(..)
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
import Foreign (Foreign, renderForeignError)
import Data.Generic.Rep (class Generic)
import Simple.JSON as JSON
import Data.List.NonEmpty as NEL

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

-- Derive newtype instance for automatic JSON
derive newtype instance readForeignTokenMetadataCodec :: ReadForeign TokenMetadataCodec
derive newtype instance writeForeignTokenMetadataCodec :: WriteForeign TokenMetadataCodec

-- | Decode token metadata safely (automatic)
decodeTokenMetadata :: String -> Either String TokenMetadataCodec
decodeTokenMetadata jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

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

-- Derive newtype instance for automatic JSON
derive newtype instance readForeignPositionCodec :: ReadForeign PositionCodec
derive newtype instance writeForeignPositionCodec :: WriteForeign PositionCodec

-- | Decode position safely (automatic)
decodePosition :: String -> Either String PositionCodec
decodePosition jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

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

-- Derive newtype instance for automatic JSON
derive newtype instance readForeignLenderOfferCodec :: ReadForeign LenderOfferCodec
derive newtype instance writeForeignLenderOfferCodec :: WriteForeign LenderOfferCodec

-- | Decode lender offer safely (automatic)
decodeLenderOffer :: String -> Either String LenderOfferCodec
decodeLenderOffer jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

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

-- Derive newtype instance for automatic JSON
derive newtype instance readForeignLaunchResultCodec :: ReadForeign LaunchResultCodec
derive newtype instance writeForeignLaunchResultCodec :: WriteForeign LaunchResultCodec

-- | Decode launch result safely (automatic)
decodeLaunchResult :: String -> Either String LaunchResultCodec
decodeLaunchResult jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

-- | Encode launch result safely (automatic)
encodeLaunchResult :: LaunchResultCodec -> String
encodeLaunchResult = writeJSON

--------------------------------------------------------------------------------
-- Array Decoders (now type-safe with automatic parsing)
--------------------------------------------------------------------------------

-- | Decode array of tokens safely using automatic JSON parsing
decodeTokenArray :: String -> Either String (Array TokenMetadataCodec)
decodeTokenArray jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

-- | Decode array of positions safely using automatic JSON parsing
decodePositionArray :: String -> Either String (Array PositionCodec)
decodePositionArray jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value

-- | Decode array of lender offers safely using automatic JSON parsing
decodeLenderOfferArray :: String -> Either String (Array LenderOfferCodec)
decodeLenderOfferArray jsonStr = case readJSON jsonStr of
  Left errors -> Left (renderForeignError (NEL.head errors))
  Right value -> Right value
module Utils 
  ( find
  , filter
  , mapArray
  , findMap
  , catMaybes
  , updateAt
  , intercalate
  , abs
  , formatPrice
  , formatPercentage
  , formatAmount
  , formatFeePercentage
  ) where

import Prelude
import Data.Array as Array
import Data.Array (uncons)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Int as Int
import Data.Number (floor, pow)
import Data.Foldable (foldl)

--------------------------------------------------------------------------------
-- Array Helper Functions
--------------------------------------------------------------------------------

-- Find first element matching predicate
find :: forall a. (a -> Boolean) -> Array a -> Maybe a
find = Array.find

-- Filter array by predicate
filter :: forall a. (a -> Boolean) -> Array a -> Array a
filter = Array.filter

-- Map function over array (alias for clearer naming)
mapArray :: forall a b. (a -> b) -> Array a -> Array b
mapArray = map

-- Find and map in one pass
findMap :: forall a b. (a -> Maybe b) -> Array a -> Maybe b
findMap = Array.findMap

-- Filter out Nothing values
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = Array.catMaybes

-- Update element at index if it exists
updateAt :: forall a. Int -> (a -> a) -> Array a -> Array a
updateAt idx f arr = case Array.modifyAt idx f arr of
  Just newArr -> newArr
  Nothing -> arr

-- Helper to join strings with a separator
intercalate :: String -> Array String -> String
intercalate sep = case _ of
  [] -> ""
  [x] -> x
  arr -> foldl (\acc x -> acc <> sep <> x) (fromMaybe "" (head arr)) (fromMaybe [] (tail arr))
  where
    head :: forall a. Array a -> Maybe a
    head arr = case uncons arr of
      Nothing -> Nothing
      Just { head: h } -> Just h
    
    tail :: forall a. Array a -> Maybe (Array a)
    tail arr = case uncons arr of
      Nothing -> Nothing
      Just { tail: t } -> Just t

--------------------------------------------------------------------------------
-- Math Helper Functions
--------------------------------------------------------------------------------

-- Note: Using pow from Data.Number

-- Helper for absolute value
abs :: Number -> Number
abs x = if x < 0.0 then -x else x

--------------------------------------------------------------------------------
-- Number Formatting Functions
--------------------------------------------------------------------------------

-- Format price with appropriate decimal places based on magnitude
formatPrice :: Number -> String
formatPrice price = 
  if price < 0.01 then 
    show (toNumber (Int.floor (price * 1000000.0)) / 1000000.0)
  else if price < 1.0 then 
    show (toNumber (Int.floor (price * 10000.0)) / 10000.0)
  else 
    show (toNumber (Int.floor (price * 100.0)) / 100.0)

-- Format percentage (0.05 -> "5%")
formatPercentage :: Number -> String
formatPercentage n = show (toNumber (Int.floor (n * 100.0))) <> "%"

-- Format fee as percentage with 2 decimal places (0.0005 -> "0.05%")
formatFeePercentage :: Number -> String
formatFeePercentage fee = show (toNumber (Int.floor (fee * 10000.0)) / 100.0) <> "%"

-- Format amount with specified decimal places
formatAmount :: Number -> Int -> String
formatAmount amount decimals = 
  let factor = pow 10.0 (toNumber decimals)
      rounded = toNumber (Int.floor (amount * factor)) / factor
  in show rounded

-- Helper to convert Int to Number
toNumber :: Int -> Number
toNumber = Int.toNumber
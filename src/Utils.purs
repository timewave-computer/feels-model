-- Domain-specific utility functions for the lending-based protocol.
-- Contains only custom formatting and mathematical functions not available in standard libraries.
-- All standard array, math, and control flow functions should be imported from their respective
-- standard libraries (Data.Array, Data.Foldable, Control.Monad, etc.)
module Utils 
  ( formatPrice
  , formatPercentage
  , formatAmount
  , formatFeePercentage
  , average
  , standardDeviation
  ) where

import Prelude
import Data.Int as Int
import Data.Number (pow)
import Data.Foldable (sum)
import Data.Array (length)
import Data.Functor (map)

--------------------------------------------------------------------------------
-- Domain-Specific Formatting Functions
--------------------------------------------------------------------------------

-- Format price with appropriate decimal places based on magnitude
formatPrice :: Number -> String
formatPrice price = 
  if price < 0.01 then 
    show (Int.toNumber (Int.floor (price * 1000000.0)) / 1000000.0)
  else if price < 1.0 then 
    show (Int.toNumber (Int.floor (price * 10000.0)) / 10000.0)
  else 
    show (Int.toNumber (Int.floor (price * 100.0)) / 100.0)

-- Format percentage (0.05 -> "5%")
formatPercentage :: Number -> String
formatPercentage n = show (Int.toNumber (Int.floor (n * 100.0))) <> "%"

-- Format fee as percentage with 2 decimal places (0.0005 -> "0.05%")
formatFeePercentage :: Number -> String
formatFeePercentage fee = show (Int.toNumber (Int.floor (fee * 10000.0)) / 100.0) <> "%"

-- Format amount with maximum 2 decimal places, no decimals for integers
formatAmount :: Number -> String
formatAmount amount = 
  let rounded = Int.toNumber (Int.floor (amount * 100.0)) / 100.0
  in if amount == Int.toNumber (Int.floor amount) 
     then show (Int.floor amount)
     else show rounded

--------------------------------------------------------------------------------
-- Domain-Specific Statistical Functions
--------------------------------------------------------------------------------

-- Calculate average of an array of numbers
average :: Array Number -> Number
average xs = if length xs == 0 then 0.0 else sum xs / Int.toNumber (length xs)

-- Calculate standard deviation of an array of numbers
standardDeviation :: Array Number -> Number
standardDeviation xs =
  let avg = average xs
      variances = map (\x -> (x - avg) * (x - avg)) xs
      variance = average variances
  in sqrt variance
  where
    sqrt x = x `pow` 0.5
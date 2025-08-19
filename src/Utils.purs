-- Protocol-specific utility functions for formatting and calculations.
-- Custom functions not available in PureScript standard libraries.
-- Standard functions should be imported from Data.Array, Data.Foldable, etc.
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
import Data.Number (pow, sqrt)
import Data.Foldable (sum, length)
-- Removed unused imports

--------------------------------------------------------------------------------
-- Domain-Specific Formatting Functions
--------------------------------------------------------------------------------

-- Format price with decimal precision based on value magnitude
formatPrice :: Number -> String
formatPrice price = 
  if price < 0.01 then 
    show (Int.toNumber (Int.floor (price * 1000000.0)) / 1000000.0)
  else if price < 1.0 then 
    show (Int.toNumber (Int.floor (price * 10000.0)) / 10000.0)
  else 
    show (Int.toNumber (Int.floor (price * 100.0)) / 100.0)

-- Convert decimal to percentage string (0.05 -> "5%")
formatPercentage :: Number -> String
formatPercentage n = show (Int.toNumber (Int.floor (n * 100.0))) <> "%"

-- Format fee as basis points percentage (0.0005 -> "0.05%")
formatFeePercentage :: Number -> String
formatFeePercentage fee = show (Int.toNumber (Int.floor (fee * 10000.0)) / 100.0) <> "%"

-- Format amounts with up to 2 decimal places, integers without decimals
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
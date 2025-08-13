-- | UI Utility Functions
-- |
-- | This module contains utility functions for the user interface,
-- | including formatting, display helpers, and UI-specific calculations.
module UI.Util
  ( estimateTimeFromBlocks
  ) where

import Prelude
import Data.Int (toNumber, round)

--------------------------------------------------------------------------------
-- TIME DISPLAY UTILITIES
--------------------------------------------------------------------------------

-- | Convert blocks to human-readable time estimate (for UI display only)
-- | Note: This is an approximation based on 30-second Solana block times
estimateTimeFromBlocks :: Int -> String
estimateTimeFromBlocks blocks =
  if blocks <= 0 then
    "Expired"
  else if blocks < blocksPerHour then
    let minutes = round (toNumber blocks * 0.5)  -- 30-second blocks
    in show minutes <> " minutes"
  else if blocks < blocksPerDay then
    let hours = blocks / blocksPerHour
    in show hours <> " hours"
  else
    let days = blocks / blocksPerDay
    in show days <> " days"
  where
    blocksPerHour :: Int
    blocksPerHour = 120  -- 2 blocks per minute * 60
    
    blocksPerDay :: Int
    blocksPerDay = 2880  -- 120 * 24
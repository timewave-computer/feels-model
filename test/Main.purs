-- Test entry point for the Feels Protocol application
module Test.Main where

import Prelude
import Effect (Effect)
import Test.Model (runModelTests)

main :: Effect Unit
main = do
  runModelTests 
{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified ParensTrifecta

main =
  defaultMain $ testGroup "All tests" $
  [
    ParensTrifecta.allTests
  ]

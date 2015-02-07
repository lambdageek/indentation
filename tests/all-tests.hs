{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty (defaultMain, testGroup)
#if defined(ENABLE_PARSEC_TESTS)
import qualified ParensParsec 
#endif
#if defined(ENABLE_TRIFECTA_TESTS)
import qualified ParensTrifecta
#endif    

#if defined(ENABLE_TRIFECTA_TESTS)
#endif

main =
  defaultMain $ testGroup "All tests" $
  [
#if defined(ENABLE_PARSEC_TESTS)
    ParensParsec.allTests
#endif
  ]
  ++
  [
#if defined(ENABLE_TRIFECTA_TESTS)
    ParensTrifecta.allTests
#endif
  ]

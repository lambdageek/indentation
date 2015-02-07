module ParensTrifecta where

import Data.Monoid (Monoid(..))
import Control.Applicative

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure, Assertion)

import Text.Trifecta
import Text.Trifecta.Indentation

data A
  = Par A   -- '(' A ')'
  | Bra A   -- '[' A ']'
  | Seq A A -- A A
  | Nil     -- epsilon
  deriving (Show, Eq)

-- a :: (Monad m, Stream s m (Char, Indentation)) => ParsecT (IndentStream s) () m A
a :: (Applicative m, TokenParsing m, IndentationParsing m) => m A
a = choice [ Seq <$> a' <*> a, a', pure Nil ]

-- a' :: (Monad m, Stream s m (Char, Indentation)) => ParsecT (IndentStream s) () m A
a' :: (TokenParsing m, IndentationParsing m) => m A
a' = choice
    [ Par <$>
        between (localTokenMode (const Eq) $ symbolic '(')
                (localTokenMode (const Eq) $ symbolic ')')
                (localIndentation Gt a)
    , Bra <$>
        between (localTokenMode (const Ge) $ symbolic '[')
                (localTokenMode (const Ge) $ symbolic ']')
                (localIndentation Gt a)
    ]


evalCharIndentationParserT :: Monad m => IndentationParserT Char m a -> IndentationState -> m a
evalCharIndentationParserT = evalIndentationParserT

evalTokenIndentationParserT :: Monad m => IndentationParserT Token m a -> IndentationState -> m a
evalTokenIndentationParserT = evalIndentationParserT

runParse ev input
 = let indA = ev a $ mkIndentationState 0 infIndentation True Gt
   in case parseString indA mempty input of
    Failure err -> Left (show err)
    Success a -> Right a

runCharParse = runParse evalCharIndentationParserT
runTokenParse = runParse evalTokenIndentationParserT

-- conveniences for tests
parL = Par . listToSeq
braL = Bra . listToSeq

listToSeq [] = Nil
listToSeq (x:xs) = Seq x $ listToSeq xs

input1 = unlines [ "("
                 , "   [("
                 , "    )"
                 , "      ]"
                 , ")"
                 ]
output1c = runCharParse input1
output1t = runTokenParse input1
expected1 = listToSeq [ parL [braL [parL []]]
                      ]

input2 = unlines [ "("
                 , "       ["
                 , "      ("
                 , "      )"
                 , "        []"
                 , "    ]"
                 , "   ("
                 , "  )"
                 , ")"
                 ]
output2c = runCharParse input2
output2t = runTokenParse input2
expected2 = listToSeq [ parL [ braL [ parL []
                                    , braL []
                                    ]
                             , parL []
                             ]
                      ]


assertParsedOk :: (Show err, Show a, Eq a) => Either err a -> a -> Assertion
assertParsedOk actual expected =
  case actual of
   Right ok -> assertEqual "parsing succeeded, but " expected ok
   Left err -> assertFailure ("parse failed with " ++ show err
                              ++ ", expected " ++ show expected)

allTests :: TestTree
allTests =
  testGroup "parens (trifecta)"
  [
    testGroup "char parsing"
    [ testCase "1" $ assertParsedOk output1c expected1
    , testCase "2" $ assertParsedOk output2c expected2
    ]
  , testGroup "token parsing"
    [ testCase "1" $ assertParsedOk output1t expected1
    , testCase "2" $ assertParsedOk output2t expected2
    ]
  ]

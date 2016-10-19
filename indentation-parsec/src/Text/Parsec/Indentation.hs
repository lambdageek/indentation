{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, TupleSections #-}
{-# OPTIONS -Wall  #-}
-- | Indentation sensitive parsing for Parsec
module Text.Parsec.Indentation (
  -- $intro
  
  -- * Indentation Streams
  IndentStream
  , IndentationToken
  , mkIndentStream
  -- * Indentation Relation and Columns
  , I.IndentationRel(..)
    -- | We use indent 1 for the first column.  Not only is this consistent
    -- with how Parsec counts columns, but it also allows 'Gt' to refer to
    -- the first column by setting the indent to 0.
  , Indentation
    -- | Infinite indentation.
  , infIndentation
  -- * Indentation Combinators
  , localTokenMode
  , localIndentation
  , absoluteIndentation
  , ignoreAbsoluteIndentation
  , localAbsoluteIndentation
  -- * Parsec Parsers
  , indentStreamParser
      ) where

-- Implements "Indentation Senstivie Parising: Landin Revisited"
--
-- Primary functions are:
--  - 'localIndent':
--  - 'absoluteIndent':
--  - 'localTokenMode':
--
-- Primary driver functions are:
--  - TODO

-- TODO:
--   Grace style indentation stream

import Control.Monad
--import Text.Parsec.Prim
import Text.Parsec.Prim (ParsecT, mkPT, runParsecT,
                         Stream(..), Consumed(..), Reply(..),
                         State(..), getInput, setInput)
import Text.Parsec.Error (Message (Message), addErrorMessage)
import Text.Parser.Indentation.Implementation (Indentation, IndentationRel (..), infIndentation, LocalState(..), IndentationState (..), updateIndentation, indentationStateAbsMode, mkIndentationState)
import qualified Text.Parser.Indentation.Implementation as I

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import qualified Text.Parsec.Indentation.Char as C
-- >>> import Text.Parsec (Parsec, try)

------------------------
-- Indentable Stream
------------------------

data IndentStream s = IndentStream { indentationState :: !IndentationState, tokenStream :: !s } deriving (Show)
--data IndentationToken t = IndentationToken !t | InvalidIndentation String
type IndentationToken t = t

-- | @mkIndentStream lo hi isAbsMode rel s@ makes a new indentation
-- stream with the given minimum and maximum indentation levels,
-- starting in absolute mode if @isAbsMode@ is @True@ or relative mode
-- otherwise, with token relation @rel@.
-- 
-- >>> mkIndentStream 0 infIndentation True Gt "abcd"
-- IndentStream {indentationState = IndentationState {minIndentation = 0, maxIndentation = ..., absMode = True, tokenRel = Gt}, tokenStream = "abcd"}
--
-- Note that columns count from 1 so 0 and @Gt@ indicate that tokens should be in any column. 
{-# INLINE mkIndentStream #-}
mkIndentStream :: Indentation -> Indentation -> Bool -> IndentationRel -> s -> IndentStream s
mkIndentStream lo hi mode rel s = IndentStream (mkIndentationState lo hi mode rel) s

instance (Monad m, Stream s m (t, Indentation)) => Stream (IndentStream s) m (IndentationToken t) where
  uncons (IndentStream is s) = do
    x <- uncons s
    case x of
      Nothing -> return Nothing
      Just ((t, i), s') -> return $ updateIndentation is i ok err where
        ok is' = Just ({-IndentationToken-} t, IndentStream is' s')
        err _ = Nothing --(InvalidIndentation msg, IndentStream is s)
        -- HACK: Sigh! We have no way to properly signal the
        -- sort of failure that occurs here.  We would do 'fail
        -- "Invalid indentation.  "++msg', but that triggers a
        -- non-backtracking error.  'return Nothing' will make
        -- Parsec think the stream is empty (which is wrong),
        -- but at least it is a backtracking error.  The
        -- fundamental problem is that 'm' *not* ParsecT (where
        -- we could signal a parsing error) but is whatever
        -- monad 'm' happens to be the argument to ParsecT.

{-# INLINE localState #-}
localState :: (Monad m) => LocalState (ParsecT (IndentStream s) u m a)
localState pre post m = do
  IndentStream is s <- getInput
  setInput (IndentStream (pre is) s)
  x <- m
  IndentStream is' s' <- getInput
  setInput (IndentStream (post is is') s')
  return x

{-# INLINE localStateUnlessAbsMode #-}
localStateUnlessAbsMode :: (Monad m) => LocalState (ParsecT (IndentStream s) u m a)
localStateUnlessAbsMode pre post m = do
  a <- liftM (indentationStateAbsMode . indentationState) getInput
  if a then m else localState pre post m


------------------------
-- Operations
------------------------

{-# INLINE localTokenMode #-}
localTokenMode :: (Monad m) => (IndentationRel -> IndentationRel) -> ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
localTokenMode = I.localTokenMode localState

{-# INLINE localIndentation #-}
localIndentation :: (Monad m) => IndentationRel -> ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
localIndentation = I.localIndentation localStateUnlessAbsMode

{-# INLINE absoluteIndentation #-}
absoluteIndentation :: (Monad m) => ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
absoluteIndentation = I.absoluteIndentation localState
--  post _  i2 = when (absMode i2) (fail "absoluteIndent: no tokens consumed") >>

{-# INLINE ignoreAbsoluteIndentation #-}
ignoreAbsoluteIndentation :: (Monad m) => ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
ignoreAbsoluteIndentation = I.ignoreAbsoluteIndentation localState

{-# INLINE localAbsoluteIndentation #-}
localAbsoluteIndentation :: (Monad m) => ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
localAbsoluteIndentation = I.localAbsoluteIndentation localState

------------------------
-- Indent Stream Impls
------------------------

streamToList :: (Monad m, Stream s m t) => s -> m [t]
streamToList s = do
  x <- uncons s
  case x of
    Nothing -> return []
    Just (c, s') -> do s'' <- streamToList s'
                       return (c : s'')

----------------
-- SourcePos

{-
mkSourcePosIndentStream s = SourcePosIndentStream s
newtype SourcePosIndentStream s = SourcePosIndentStream s
instance (Stream s m t) => Stream (SourcePosIndentStream s) m (Indent, t) where
  uncons (SourcePosIndentStream s) = do
    col <- liftM sourceColumn $ getPosition
    x <- uncons s
    case x of
      Nothing -> return Nothing
      Just x -> return (Just ((col, x), SourcePosIndentStream s))
-}


----------------
-- TODO: parser based on first non-whitespace char

----------------
-- First token of line indents

----------------
-- Based on Indents

-- Note that if 'p' consumes input but is at the wrong indentation, then
-- 'indentStreamParser p' signals an error but does *not* consume input.
-- This allows Parsec primitives like 'string' to be properly backtracked.
{-# INLINE indentStreamParser #-}
indentStreamParser :: (Monad m) => ParsecT s u m (t, Indentation) -> ParsecT (IndentStream s) u m (IndentationToken t)
indentStreamParser p = mkPT $ \state ->
  let IndentStream is s = stateInput state
      go f (Ok (a, i) state' e) = updateIndentation is i ok err where
        ok is' = return $ f $ return (Ok ({-IndentationToken-} a) (state' {stateInput = IndentStream is' (stateInput state') }) e)
        err msg = return $ Empty $ return $ Error (Message ("Invalid indentation.  "++msg++show ((stateInput state) { tokenStream = ""})) `addErrorMessage` e)
      go f (Error e) = return $ f $ return (Error e)
  in runParsecT p (state { stateInput = s }) >>= consumed (go Consumed) (go Empty)

{-# INLINE consumed #-}
consumed :: (Monad m) => (a -> m b) -> (a -> m b) -> Consumed (m a) -> m b
consumed c _ (Consumed m) = m >>= c
consumed _ e (Empty m)    = m >>= e

-- lifting operator
-- token, tokens, tokenPrim, tokenPrimEx ???
-- whiteSpace
-- ByteString
-- ByteString.Lazy
-- Text

{-
delimitedLayout :: Stream (IndentStream s) m t =>
  ParsecT (IndentStream s) u m open -> Bool ->
  ParsecT (IndentStream s) u m close -> Bool ->
  ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m a
delimitedLayout open openAny close closeAny body = between open' close' (localIndent (Const 0) body) where
  open'  | openAny = localIndent (Const 0) open
         | otherwise = open
  close' | closeAny = localIndent (Const 0) close
         | otherwise = close

indentedLayout :: Stream (IndentStream s) m t =>
  (Maybe (ParsecT (IndentStream s) u m sep)) ->
  ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m [a]
indentedLayout (Nothing ) clause = localIndent Gt $ many $ absoluteIndent $ clause
indentedLayout (Just sep) clause = liftM concat $ localIndent Gt $ many $ absoluteIndent $ sepBy1 clause sep
-}

{-
layout p = delimitedLayout (symbol "{") False (symbol "}") True (semiSep p)
       <|> indentedLayout (Just semi) p

identifier pred = liftM fromString $ try $ identifier >>= \x -> guard (pred x) >> return x
operator pred = liftM fromString $ try $ operator >>= \x -> guard (pred x) >> return x

reserved name = (if name `elem` middleKeywords then localFirstTokenMode (const Ge) else id) $ reserved name

Numbers, Integers and Naturals are custom

dotSep
dotSep1

-}

{-
test :: String
test = foo where
          foo = "abc \
\def" ++ ""

test2 :: Int
test2 = foo where
          foo = let { x = 1;
 } in x


--- All code indented?
  foo = 3
  bar = 4
-}

-- $intro
-- 
-- An indentation-sensitive language can be thought of as annotating
-- each token in the input stream by a column number (represented as
-- an 'Indentation').  With column numbers, new combinators are
-- available to allow a parser to consume a token only if its
-- indentation agrees with a certain predicate such as /indented more
-- than the last successfully parsed token/ or /aligned in the same
-- column as the last successfully parsed token/.
--
-- Consider the following self-contained example:
--
-- >>> data T a = Node a [T a] deriving (Show)
-- >>> :{
-- let input = unlines [
--       "a",
--       "  b",
--       "  c",
--       "d",
--       "e",
--       "  f",
--       "  g"
--       ]
-- :}
--
-- >>> input
-- "a\n  b\n  c\n..."
--
-- >>> mkStream = mkIndentStream 0 infIndentation True Gt . C.mkCharIndentStream
-- >>> :t mkStream
-- mkStream :: s -> IndentStream (C.CharIndentStream s)
--
-- >>> import Control.Applicative
-- >>> import qualified Text.Parsec.Char as PC
-- >>> import Text.Parsec.Combinator
-- >>> upgradeParser = indentStreamParser . C.charIndentStreamParser
-- >>> whiteSpace = ignoreAbsoluteIndentation (localTokenMode (const Any) (upgradeParser PC.spaces))
-- >>> lexeme p = p <* whiteSpace
-- >>> letter = lexeme (upgradeParser PC.letter)
--
-- >>> :t letter
-- ...
--
-- >>> verticallyAlignedList p = localIndentation Gt (many (absoluteIndentation p))
-- >>> :t verticallyAlignedList
-- verticallyAlignedList
--   :: Monad m =>
--      ParsecT (IndentStream s) u m a -> ParsecT (IndentStream s) u m [a] 
--
-- >>> :{
-- oneTree :: Stream s m Char => ParsecT (IndentStream (C.CharIndentStream s)) u m a -> ParsecT (IndentStream (C.CharIndentStream s)) u m (T a)
-- oneTree thing = Node <$> try thing <*> (verticallyAlignedList (oneTree thing))
-- :}
--
-- >>> :{
-- parse :: Parsec (IndentStream (C.CharIndentStream String)) () a -> String -> Either Text.Parsec.Error.ParseError a
-- parse p s = Text.Parsec.Prim.parse (whiteSpace *> p <* localTokenMode (const Any) eof) "" (mkStream s)
-- :}
-- 
-- >>> parse letter "a"
-- Right 'a'
--
-- >>> parse letter " "
-- Left ...
-- ...
--
-- >>> parse (verticallyAlignedList letter) "a\nb\n"
-- Right "ab"
--
-- >>> parse (verticallyAlignedList letter) "  a\n  b\n"
-- Right "ab"
--
-- >>> parse (verticallyAlignedList letter) "  a\nb"
-- Left (line 2, column 2):
-- Invalid indentation.  Found a token at indentation 1.  Expecting a token at an indentation between 3 and 3...
--
-- >>> parse (oneTree letter) ""
-- Left ...
-- ...
--
-- >>> parse (oneTree letter) "a"
-- Right (Node 'a' [])
--
-- >>> parse (oneTree letter) "a\n b\n c\n"
-- Right (Node 'a' [Node 'b' [],Node 'c' []])
-- 
-- >>> parse (verticallyAlignedList (oneTree letter)) input
-- Right ...
--
-- See "Test.Parsec.Indentation.Char" for 'Test.Parsec.Indentation.Char.mkCharIndentStream'.
--
-- See "Text.Parsec.Indentation.Token" for token combinators built atop 'IndentStream' .
  

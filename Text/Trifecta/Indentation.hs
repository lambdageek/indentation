{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

-- Implements "Indentation Senstivie Parsing" for Trifecta
module Text.Trifecta.Indentation (
  I.IndentationRel(..), I.Indentation, I.infIndentation, I.mkIndentationState,
  IndentationParsing(..),
  IndentationParserT,
  runIndentationParserT,
  evalIndentationParserT,
  execIndentationParserT,
  ) where

import Control.Applicative
import Control.Monad.State

import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Trifecta.Combinators
import Text.Trifecta.Delta

import Text.Parser.Indentation.Implementation as I

--------------
-- User API --
--------------

class IndentationParsing m where
  localTokenMode :: (IndentationRel -> IndentationRel) -> m a -> m a
  localIndentation :: IndentationRel -> m a -> m a
  absoluteIndentation :: m a -> m a
  ignoreAbsoluteIndentation :: m a -> m a

---------------
-- Data Type --
---------------

newtype IndentationParserT t m a = IndentationParserT { unIndentationParserT :: StateT IndentationState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadPlus, Alternative)

deriving instance (Parsing m, MonadPlus m) => Parsing (IndentationParserT t m)
deriving instance (DeltaParsing m) => DeltaParsing (IndentationParserT Char m)
deriving instance (MarkParsing Delta m) => MarkParsing Delta (IndentationParserT Char m)
deriving instance (DeltaParsing m) => DeltaParsing (IndentationParserT Token m)
deriving instance (MarkParsing Delta m) => MarkParsing Delta (IndentationParserT Token m)

{-# INLINE runIndentationParserT #-}
runIndentationParserT :: IndentationParserT t m a -> IndentationState -> m (a, IndentationState)
runIndentationParserT (IndentationParserT m) = runStateT m

{-# INLINE evalIndentationParserT #-}
evalIndentationParserT :: (Monad m) => IndentationParserT t m a -> IndentationState -> m a
evalIndentationParserT (IndentationParserT m) = evalStateT m

{-# INLINE execIndentationParserT #-}
execIndentationParserT :: (Monad m) => IndentationParserT t m a -> IndentationState -> m IndentationState
execIndentationParserT (IndentationParserT m) = execStateT m

---------------------
-- Class Instances --
---------------------

-- Putting the check in CharParsing --

instance (DeltaParsing m) => CharParsing (IndentationParserT Char m) where
  satisfy f = checkIndentation (satisfy f)

instance (DeltaParsing m) => TokenParsing (IndentationParserT Char m) where
  someSpace = IndentationParserT $ someSpace -- Ignore indentation of whitespace

-- Putting the check in TokenParsing --

data Token

instance (DeltaParsing m) => CharParsing (IndentationParserT Token m) where
  satisfy f = IndentationParserT $ satisfy f

instance (DeltaParsing m) => TokenParsing (IndentationParserT Token m) where
  token p = checkIndentation (token (unIndentationParserT p))

--------

instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (IndentationParserT t m) where
  lookAhead m = IndentationParserT $ do
    s <- get
    x <- lookAhead (unIndentationParserT m)
    put s
    return x

--------

instance (Monad m) => IndentationParsing (IndentationParserT t m) where
  {-# INLINE localTokenMode #-}
  localTokenMode = I.localTokenMode localState

  {-# INLINE absoluteIndentation #-}
  absoluteIndentation = I.absoluteIndentation localState

  {-# INLINE ignoreAbsoluteIndentation #-}
  ignoreAbsoluteIndentation = I.ignoreAbsoluteIndentation localState

  {-# INLINE localIndentation #-}
  localIndentation = I.localIndentation localStateUnlessAbsMode

---------------------
-- Private Helpers --
---------------------

{-# INLINE localState #-}
localState :: (Monad m) => LocalState (IndentationParserT t m a)
localState pre post m = IndentationParserT $ do
  is <- get
  put (pre is)
  x <- unIndentationParserT m
  is' <- get
  put (post is is')
  return x

{-# INLINE localStateUnlessAbsMode #-}
localStateUnlessAbsMode :: (Monad m) => LocalState (IndentationParserT t m a)
localStateUnlessAbsMode pre post m = IndentationParserT $ do
  a <- gets indentationStateAbsMode
  unIndentationParserT $ if a then m else localState pre post m

{-# INLINE checkIndentation #-}
checkIndentation :: (DeltaParsing m) => StateT IndentationState m a -> IndentationParserT t m a
checkIndentation m = IndentationParserT $ do
    is <- get
    p <- position
    let ok is' = do x <- m; put is'; return x
        err msg = fail msg
    updateIndentation is (fromIntegral $ column p + 1) ok err

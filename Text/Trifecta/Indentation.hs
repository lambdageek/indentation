{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, StandaloneDeriving #-}

-- Implements "Indentation Senstivie Parsing" for Trifecta
module Text.Trifecta.Indentation (
  I.IndentationRel(..), I.Indentation, I.infIndentation, I.mkIndentationState,
  I.IndentationState,
  IndentationParsing(..),
  Token,
  IndentationParserT,
  runIndentationParserT,
  evalIndentationParserT,
  execIndentationParserT,
  ) where

import Control.Applicative
import Control.Monad.State.Lazy as LazyState
import Control.Monad.State.Strict as StrictState

import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Token (TokenParsing(..))
import Text.Parser.Char (CharParsing(..))
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Trifecta.Combinators (DeltaParsing(..), MarkParsing(..))
import Text.Trifecta.Delta (Delta, column)

import Text.Parser.Indentation.Implementation (IndentationState(..), IndentationRel(..), LocalState)
import qualified Text.Parser.Indentation.Implementation as I

--------------
-- User API --
--------------

class IndentationParsing m where
  localTokenMode :: (IndentationRel -> IndentationRel) -> m a -> m a
  localIndentation :: IndentationRel -> m a -> m a
  absoluteIndentation :: m a -> m a
  ignoreAbsoluteIndentation :: m a -> m a
  localAbsoluteIndentation :: m a -> m a
  localAbsoluteIndentation = ignoreAbsoluteIndentation . absoluteIndentation

----------------------
-- Lifted Instances --
----------------------

{- TODO:
Applicative
Functor
MonadWriter w m
MonadError e m
Monad m
MonadReader r m
MonadTrans (StateT s)	 
Monad m
Monad m
MonadFix m
MonadPlus m
MonadIO m
MonadCont m
#-}

{-# INLINE liftLazyStateT2 #-}
liftLazyStateT2 :: (m (a, s) -> m (a, s)) -> LazyState.StateT s m a -> LazyState.StateT s m a
liftLazyStateT2 f m = LazyState.StateT $ \s -> f (LazyState.runStateT m s)

instance (IndentationParsing i) => IndentationParsing (LazyState.StateT s i) where
  localTokenMode f = liftLazyStateT2 (localTokenMode f)
  localIndentation r = liftLazyStateT2 (localIndentation r)
  absoluteIndentation = liftLazyStateT2 absoluteIndentation
  ignoreAbsoluteIndentation = liftLazyStateT2 ignoreAbsoluteIndentation
  localAbsoluteIndentation = liftLazyStateT2 localAbsoluteIndentation

{-# INLINE liftStrictStateT2 #-}
liftStrictStateT2 :: (m (a, s) -> m (a, s)) -> StrictState.StateT s m a -> StrictState.StateT s m a
liftStrictStateT2 f m = StrictState.StateT $ \s -> f (StrictState.runStateT m s)

instance (IndentationParsing i) => IndentationParsing (StrictState.StateT s i) where
  localTokenMode f = liftStrictStateT2 (localTokenMode f)
  localIndentation r = liftStrictStateT2 (localIndentation r)
  absoluteIndentation = liftStrictStateT2 absoluteIndentation
  ignoreAbsoluteIndentation = liftStrictStateT2 ignoreAbsoluteIndentation
  localAbsoluteIndentation = liftStrictStateT2 localAbsoluteIndentation

---------------
-- Data Type --
---------------

-- TODO: do we need a strict version of this?
newtype IndentationParserT t m a = IndentationParserT { unIndentationParserT :: LazyState.StateT IndentationState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadPlus, Alternative)

deriving instance (Parsing m, MonadPlus m) => Parsing (IndentationParserT t m)
deriving instance (DeltaParsing m) => DeltaParsing (IndentationParserT Char m)
deriving instance (MarkParsing Delta m) => MarkParsing Delta (IndentationParserT Char m)
deriving instance (DeltaParsing m) => DeltaParsing (IndentationParserT Token m)
deriving instance (MarkParsing Delta m) => MarkParsing Delta (IndentationParserT Token m)

{-# INLINE runIndentationParserT #-}
runIndentationParserT :: IndentationParserT t m a -> IndentationState -> m (a, IndentationState)
runIndentationParserT (IndentationParserT m) = LazyState.runStateT m

{-# INLINE evalIndentationParserT #-}
evalIndentationParserT :: (Monad m) => IndentationParserT t m a -> IndentationState -> m a
evalIndentationParserT (IndentationParserT m) = LazyState.evalStateT m

{-# INLINE execIndentationParserT #-}
execIndentationParserT :: (Monad m) => IndentationParserT t m a -> IndentationState -> m IndentationState
execIndentationParserT (IndentationParserT m) = LazyState.execStateT m

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

  {-# INLINE localIndentation #-}
  localIndentation = I.localIndentation localStateUnlessAbsMode

  {-# INLINE absoluteIndentation #-}
  absoluteIndentation = I.absoluteIndentation localState

  {-# INLINE ignoreAbsoluteIndentation #-}
  ignoreAbsoluteIndentation = I.ignoreAbsoluteIndentation localState

  {-# INLINE localAbsoluteIndentation #-}
  localAbsoluteIndentation = I.localAbsoluteIndentation localState

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
  a <- gets I.indentationStateAbsMode
  unIndentationParserT $ if a then m else localState pre post m

{-# INLINE checkIndentation #-}
checkIndentation :: (DeltaParsing m) => LazyState.StateT IndentationState m a -> IndentationParserT t m a
checkIndentation m = IndentationParserT $ do
    is <- get
    p <- position
    let ok is' = do x <- m; put is'; return x
        err msg = fail msg
    I.updateIndentation is (fromIntegral $ column p + 1) ok err

{-# language PackageImports #-}
-- | This module re-exports <https://hackage.haskell.org/package/indentation-parsec/docs/Text-Parsec-Indentation-Token.html Text.Parsec.Indentation.Token> from <https://hackage.haskell.org/package/indentation-parsec indentation-parsec>.
module Text.Parsec.Indentation.Token (module Impl) where

import "indentation-parsec" Text.Parsec.Indentation.Token as Impl

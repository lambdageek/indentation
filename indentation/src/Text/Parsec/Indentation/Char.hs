{-# language PackageImports #-}
-- | This module re-exports <https://hackage.haskell.org/package/indentation-parsec/docs/Text-Parsec-Indentation-Char.html Text.Parsec.Indentation.Char> from <https://hackage.haskell.org/package/indentation-parsec indentation-parsec>.
module Text.Parsec.Indentation.Char (module Impl) where

import "indentation-parsec" Text.Parsec.Indentation.Char as Impl

{-# language PackageImports #-}
-- | This module re-exports <https://hackage.haskell.org/package/indentation-parsec/docs/Text-Parsec-Indentation.html Text.Parsec.Indentation> from <https://hackage.haskell.org/package/indentation-parsec indentation-parsec>.
module Text.Parsec.Indentation (module Impl) where

import "indentation-parsec" Text.Parsec.Indentation as Impl

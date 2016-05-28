{-# language PackageImports #-}
-- | This module re-exports <https://hackage.haskell.org/package/indentation-core/docs/Text-Parser-Indentation-Implementation.html Text.Parser.Indentation.Implementation> from <https://hackage.haskell.org/package/indentation-core indentation-core>.
module Text.Parser.Indentation.Implementation (module Impl)
       where

import "indentation-core" Text.Parser.Indentation.Implementation as Impl

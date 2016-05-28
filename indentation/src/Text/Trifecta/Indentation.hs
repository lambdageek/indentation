{-# language PackageImports #-}
-- | This module re-exports <https://hackage.haskell.org/package/indentation-trifecta/docs/Text-Trifecta-Indentation.html Text.Trifecta.Indentation> from <https://hackage.haskell.org/package/indentation-trifecta indentation-trifecta>.
module Text.Trifecta.Indentation (module Impl) where

import "indentation-trifecta" Text.Trifecta.Indentation as Impl hiding (Token)

# 0.3.1 #

* Bump `indentation-trifecta` dependency to `0.0.1` (supports `trifecta-1.6`)


# 0.3.0.1 #

* Hyperlink Haddock of re-exported modules to the other packages.

# 0.3 #

* Broke package up into `indentation-core`, `indentation-parsec` and `indentation-trifecta` packages.
  This package is now solely for backward compatability.
  
# 0.2.1.2 #

* Support GHC 8.0.1 (release candidate 2)

# 0.2.1.1 #

* Bump base dep to allow 4.8.*
  Builds with ghc-7.10.1

# 0.2.1 #

* Export 'Token' from 'Text.Trifecta.Indentation'

  * Add new testcase

* Re-export 'Text.Parser.Indentation.Implementation.IndentationState' from 'Text.Trifecta.Indentation'

# 0.2.0.3 #

* Add (simple) tests for Parsec and Trifecta.

# 0.2.0.2 #

* Allow building with `parsec >= 3.1.7` and `trifecta >= 1.5`



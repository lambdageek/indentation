The repository contains code for the `indentation-core`,
`indentation-parsec`, `indentation-trifecta` packages in their
respective subdirectories.

# Getting started with Cabal sandboxes #

Additionally it contains code for the `indentation` roll-up package
(useful primarily for backward compatability for projects that
depended on versions of indentation before 0.3).

The easiest environment for working with the packages is using Cabal's
sandboxes.

To get started the first time, run the following in the root directory
of the git repository:

```bash
$ cabal sandbox init
$ for d in indentation-core indentation-parsec indentation-trifecta indentation; do cabal sandbox add-source $d ; done
$ for d in indentation-core indentation-parsec indentation-trifecta indentation; do pushd $d ; cabal sandbox init --sandbox ../.cabal-sandbox ; popd ; done
```

This will set up a shared sandbox in `.cabal-sandbox/` which can be used from each subdirectory.

For example:
```bash
$ cd indentation-parsec
$ cabal install --dependencies-only --enable-tests
$ cabal configure --enable-tests
$ cabal build
$ cabal test
```

This will install (into the shared sandbox) all the dependencies of
`indentation-parsec` (including `indentation-core` from
`../indentation-core`), configure the package, build it, and run the
tests.  If you subsequently make changes to `indentation-parsec` or to
`indentation-core`, building and running will recompile as necessary.


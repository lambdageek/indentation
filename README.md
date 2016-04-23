# Combinator libraries for indentation-sensitive parsing

This repository contains Haskell libraries for indentation-sensitive parsing with Parsec and Trifecta.

The packages are available on hackage:
* [`indentation-parsec`](https://hackage.haskell.org/packages/indentation-parsec) for [Parsec](https://hackage.haskell.org/packages/parsec)
* [`indentation-trifecta`](https://hackage.haskell.org/packages/indentation-parsec) for [Trifecta](https://hackage.haskell.org/packages/trifecta)

Two other packages are provided:
* `indentation-core` contains the shared core implementation of
   indentation parsing based on:  
    __Michael D. Adams and Ömer S. Ağacan__.
    Indentation-sensitive parsing for Parsec.
    In *Proceedings of the 2014 ACM SIGPLAN Symposium on Haskell*,
    Haskell ’14, pages 121–132.
    ACM, New York, NY, USA, September 2014. ISBN 978-1-4503-3041-1.
    [doi:10.1145/2633357.2633369](http://dx.doi.org/10.1145/2633357.2633369).
* `indentation` a rollup package re-exporting the modules from all the
   above packages for backward compatability with earlier versions of `indentation`.

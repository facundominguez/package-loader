This package uses the RTS linker to load packages and their
dependencies.

The code here has been taken from the plugins package. Because it does
so much less than plugins, it does not depend on the GHC API or
haskell-src.

In addition to capabilities of the RTS linker, this package keeps track
of which packages have been loaded so packages are not loaded multiple
times when having diamond dependencies. Moreover, the Cabal API is used
to query package databases to find the dependencies of a package so
dependencies are recursively loaded.

Building from git
=================

    $ autoreconf
    $ automake --add-missing
    $ cabal install

Installing from hackage should work as usual.

    $ cabal install package-loader

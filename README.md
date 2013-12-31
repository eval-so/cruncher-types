# cruncher-types

[![Build Status](https://travis-ci.org/eval-so/cruncher-types.png)](https://travis-ci.org/eval-so/cruncher-types)

This provides the `Request`, `Result`, and `FinalResult` types that Cruncher
depends on.

The reason it is a separate library is that other libraries can make use of
these as well, to interface with http://eval.so/ via Haskell.

Since we already export Aeson instances for these types, there is no reason to
make library authors re-create them, and they are guaranteed to work with the
JSON API, since this is what is used in production.

Rather than making people depend on Cruncher to take advantage of this (and all
of its dependencies), we just make Cruncher depend on this separate library.

Like Cruncher, this library is licensed BSD-3.

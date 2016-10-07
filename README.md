# haskell-stats

The aim of this project is to provide a complete set of statistics about
hackage that are updated continously.

Currently the project supports:

* Getting the most popular and most downloadaded package.

* Getting the most popular module and id (e.g. function).

This is currently done from parsing a previously downloaded set of packages.
The idea is to provide this and other information directly from the
packages available in hackage (or maybe stackage?).

## TODOS

* Clean up the code and refactor as much as possible using good coding
  practices (at the moment everything is a bit too hacky).

* Provide an extensively API that can be used to reuse the provided statistics
  in other ways.

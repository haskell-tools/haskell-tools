# Project conventions

## Using GHCi

  - use `stack ghci`

## Test the code

  - The test folder contains the test package. The test suite contains both unit and nightly tests.
  - Run it with `stack test`. Run separate suites with `stack test haskell-tools-*`.
  - Continous integration is backed by Travis-CI.  [![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools/master.svg)](https://travis-ci.org/haskell-tools/haskell-tools)
  - [Test coverage](https://haskell-tools.github.io/master/coverage/hpc_index.html) is automatically calculated and published, but is currently very low because of generated fields/instances/references defined for API consistency but not being used by any refactoring.

## Coding conventions

 - We don't enforce to use tabular-like layout, because it can be hard to maintain.
 - Imports are usually divided into three block: GHC imports, internal ones (from the same package or other Haskell-tools packages) and other imports.
 - Because the Haskell AST is so large, we divided it into several parts. This structure can be found at the representation, converting, matchers and generators. The parts are: names, kinds, types, patterns, literals, expressions, statements (do syntax and list comprehensions), Template Haskell (TH), binds (function and value definitions), declarations and modules (containing imports/exports).

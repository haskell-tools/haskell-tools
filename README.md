# haskell-tools

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this repository contains the ht-refact tool, a refactoring tool for Haskell. There are 5 implemented refactorings: 
  - **Rename definition**: Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - **Extract binding**: Extracts the selected expression as a local binding.
  - **Generate type signature**: Generates the type signature for a function. Useful for declaring the type if it is complex.
  - **Organize imports**: Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - **Generate exports**: Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.

## [Check out our demo](http://haskelltools.org)

Sources for the demo application can be found [here](https://github.com/kelemzol/haskell-tools-demo)

Installation
  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - `cabal update`
  - Install haskell-tools-refactor: `cabal install haskell-tools-refactor`
    - On Windows you might have to manually download `old-time`, fix the base dependency and install it with Cygwin or MinGW.

Test:
  - The test folder contains the test package. The test suite contains both unit and nightly tests.
  - Run it with `cabal configure --enable-tests; cabal test`.
  - Continous integration is backed by Travis-CI. Status: [![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools.svg?maxAge=2592000)](https://travis-ci.org/haskell-tools/haskell-tools)
  - Test coverage is automatically calculated, but is currently very low due to not all tests running, and lot of generated fields/instances/references not being used by any refactoring: [![Coverage Status](https://coveralls.io/repos/github/haskell-tools/haskell-tools/badge.svg)](https://coveralls.io/github/haskell-tools/haskell-tools)


Using GHCi:
  - `ghci -package ghc -isrc\ast;src\ast-ghc;src\ast-trf;src\ast-ppr;src\ast-gen;src\refactor Language.Haskell.Tools.Refactor` (from root, on Windows)

Known limitations:
  - The refactoring tool currently works with only one module, but that will be changed in the near future.
  - The name of the module cannot be changed by rename.
  - Generate type signature does not generate `ScopedTypeVariables` extension and explicit `forall` in cases when it would be needed to create a compilable result. These have to be added manually.
  - The following extensions are not supported: `EmptyCase`, `ImplicitParams`, `UnicodeSyntax`
  - Template Haskell (`TemplateHaskell` extension) is not supported
  - The C preprocessor (`CPP` extension) is not supported

Plans: 
  - Refactorings on multiple modules by the summer of 2016
  - Editor support in the winter of 2016
  - Wider variety of refactorings and Template Haskell support in the spring of 2017

Contents: This repository contains 6 packages that provide different functionality.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast.svg)](http://hackage.haskell.org/package/haskell-tools-ast) **haskell-tools-ast** contains the representations of our syntax tree and utility functions.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-fromghc.svg)](http://hackage.haskell.org/package/haskell-tools-ast-fromghc) **haskell-tools-ast-fromghc** contains how can our AST be generated from the different representations of GHC.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-trf.svg)](http://hackage.haskell.org/package/haskell-tools-ast-trf) **haskell-tools-ast-trf** contains transformations that can be performed to make the syntax tree ready for rewriting.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-gen.svg)](http://hackage.haskell.org/package/haskell-tools-ast-gen) **haskell-tools-ast-gen** contains functions for generating parts of the syntax tree.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-prettyprint.svg)](http://hackage.haskell.org/package/haskell-tools-prettyprint) **haskell-tools-prettyprint** enables us to pretty print the AST in its original form.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-refactor.svg)](http://hackage.haskell.org/package/haskell-tools-refactor) **haskell-tools-refactor** defines the actual refactorings.

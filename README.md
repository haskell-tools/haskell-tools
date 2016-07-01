# haskell-tools

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this repository contains the ht-refact tool, a refactoring tool for Haskell. There are 5 implemented refactorings: 
  - Rename definition: Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - Extract binding: Extracts the selected expression as a local binding.
  - Generate type signature: Generates the type signature for a function. Useful for declaring the type if it is complex.
  - Organize imports: Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - Generate exports: Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.

## [Check out our demo](http://haskelltools.org)

Sources for the demo application can be found [here](https://github.com/kelemzol/haskell-tools-demo)

Installation
  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - `cabal update`
  - Install haskell-tools-refactor: `cabal install haskell-tools-refactor`

Test:
  - The test folder contains the test package.
  - The test suite contains both unit and nightly tests.
  - It can be run or from GHCi. After loading it with `ghci -package ghc -isrc\ast;src\ast-ghc;src\ast-trf;src\ast-ppr;src\ast-gen;src\refactor;test Main` (from root, on Windows), you can run the tests by `run unitTests` or `run nightlyTests`

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
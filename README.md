# haskell-tools
The goal of this project is to create developer tools for the functional programming language Haskell. Currently this repository contains the ht-refact tool, a refactoring tool for haskell.

[Check out our demo](http://haskelltools.org)

There are some packages that are required to be installed for haskell-tools:
  - [Instance Control package](https://github.com/nboldi/instance-control)
  - [References package](https://github.com/nboldi/references)
  - [Structural Traversal package](https://github.com/nboldi/structural-traversal)
  
This repository contains several related packages in the src directory. You should install them in this order:
  1. haskell-tools-ast
  2. haskell-tools-ast-ghc
  3. haskell-tools-ast-trf
  4. haskell-tools-ast-ppr
  5. haskell-tools-ast-gen
  6. haskell-tools-refactor

Or you might want to just start it in ghci: `ghci -package ghc -isrc\ast;src\ast-ghc;src\ast-trf;src\ast-ppr;src\ast-gen;src\refactor Language.Haskell.Tools.Refactor` (from root, on Windows)

Sources for the demo application can be found [here](https://github.com/kelemzol/haskell-tools-demo)


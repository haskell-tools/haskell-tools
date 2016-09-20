# haskell-tools

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this repository contains the **ht-refact** tool, a refactoring tool for Haskell. There are 5 implemented refactorings: 
  - **Rename definition**: Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - **Extract binding**: Extracts the selected expression as a local binding.
  - **Generate type signature**: Generates the type signature for a function. Useful for declaring the type if it is complex.
  - **Organize imports**: Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - **Generate exports**: Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.

**[Check out our demo](http://haskelltools.org)**

## Installation

  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - Use `stack install haskell-tools-refactor` or `cabal install haskell-tools-refactor` to install the library. Also install `haskell-tools-cli` or `haskell-tools-demo` to try the command line interface, or to setup the demo for yourself.
    - On Windows you might have to install `old-time` with Cygwin or MinGW.

## Installation from source

  - *Recommended*: use [stack](https://docs.haskellstack.org/en/stable/README/) for building the project
    - `stack --stack-yaml=stack-all.yaml setup`
    - `stack --stack-yaml=stack-all.yaml build`
  - *Alternative*: you have to cabal-install each package of the repository in the following order: `ast`, `ast-ghc`, `ast-trf`, `ast-gen`, `ast-ppr`, `refactor`.
  
## Running the CLI

  - If you are using stack to build from source: `stack --stack-yaml=stack-all.yaml exec ht-refact -- [ghc-flags] package-roots`
  - Otherwise, install the `haskell-tools-cli` package and use `ht-refact [ghc-flags] package-roots`.
  - Select a module to refactor with `SelectModule modulename`
  - Use the refactorings:
    - `RenameDefinition src-range new-name`
    - `ExtractDefinition src-range new-name`
    - `GenerateSignature src-range`
    - `OrganizeImports`
    - `GenerateExports`
  - Source ranges can be given in the `startrow:startcol-endrow:endcol` format.
  - The CLI automatically reloads the changed modules.
  - When finished, use `Exit` to close the CLI.

## Setup the demo for yourself

  - Install a web server, for example [apache](https://httpd.apache.org/).
  - Install the `haskell-tools-demo` package.
  - Host the website in the `demo/website` folder.
  - Use `haskell-tools-demo working-dir` command to start the demo service.
  - Visit `localhost` in the browser.

## Using GHCi

  - use `stack ghci`

## Test the code

  - The test folder contains the test package. The test suite contains both unit and nightly tests.
  - Run it with `stack test`.
  - Continous integration is backed by Travis-CI. Status: [![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools/master.svg)](https://travis-ci.org/haskell-tools/haskell-tools)
  - Test coverage is automatically calculated, but is currently very low because of generated fields/instances/references defined for API consistency but not being used by any refactoring: [![Coverage Status](https://coveralls.io/repos/github/haskell-tools/haskell-tools/badge.svg)](https://coveralls.io/github/haskell-tools/haskell-tools)

## Known limitations

  - Generate type signature does not generate `ScopedTypeVariables` extension and explicit `forall` in cases when it would be needed to create a compilable result. These have to be added manually.
  - The following extensions are not supported: `UnicodeSyntax`, `CPP`

## Plans

  - Editor support in the winter of 2016
  - Wider variety of refactorings and Template Haskell support in the spring of 2017

## Repository contents

This repository contains 6 packages that provide different functionality.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast.svg)](http://hackage.haskell.org/package/haskell-tools-ast) **haskell-tools-ast** contains the representations of our syntax tree and utility functions.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-fromghc.svg)](http://hackage.haskell.org/package/haskell-tools-ast-fromghc) **haskell-tools-ast-fromghc** contains how can our AST be generated from the different representations of GHC.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-trf.svg)](http://hackage.haskell.org/package/haskell-tools-ast-trf) **haskell-tools-ast-trf** contains transformations that can be performed to make the syntax tree ready for rewriting.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast-gen.svg)](http://hackage.haskell.org/package/haskell-tools-ast-gen) **haskell-tools-ast-gen** contains functions for generating parts of the syntax tree.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-prettyprint.svg)](http://hackage.haskell.org/package/haskell-tools-prettyprint) **haskell-tools-prettyprint** enables us to pretty print the AST in its original form.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-refactor.svg)](http://hackage.haskell.org/package/haskell-tools-refactor) **haskell-tools-refactor** defines the actual refactorings.

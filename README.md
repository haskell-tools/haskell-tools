# haskell-tools

The goal of this project is to create developer tools for the functional programming language Haskell. Currently this repository contains the **ht-refact** tool, a refactoring tool for Haskell. There are 5 implemented refactorings: 
  - **Rename definition**: Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - **Extract binding**: Extracts the selected expression as a local binding.
  - **Generate type signature**: Generates the type signature for a function. Useful for declaring the type if it is complex.
  - **Organize imports**: Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - **Generate exports**: Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.

**[Check out our demo](http://haskelltools.org)**

## Installation (last release from Hackage)

  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - On Linux you might need to install libz-dev and libgmp-dev (`sudo apt-get install libz-dev libgmp-dev`).
  - Use `cabal install haskell-tools-refactor` to install the library. Also install `haskell-tools-cli` or `haskell-tools-demo` to try the command line interface, or to setup the demo for yourself.

## Installation (last build from source)

  - *Recommended*: use [stack](https://docs.haskellstack.org/en/stable/README/) for building the project
    - `stack --stack-yaml=stack-all.yaml setup`
    - `stack --stack-yaml=stack-all.yaml build`
  - *Alternative*: you have to cabal-install each package of the repository in the following order: `ast`, `ast-ghc`, `ast-trf`, `ast-gen`, `ast-ppr`, `refactor`.
  
## Running the CLI

  - If you are using stack to build from source: `stack --stack-yaml=stack-all.yaml exec ht-refact -- [flags] package-roots`
  - Otherwise, install the `haskell-tools-cli` package and use `ht-refact [flags] package-roots`.
  - You can use [ghc flags](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html) to control how modules are loaded and checked.
  - By setting the `-one-shot`, `-module-name=<modulename>` and `-refactoring=<refactor-command>` flags, you can perform a refactoring without the interactive mode.
  - By using the `-dry-run` flag, the files will not be modified, the result will be printed on the output.
  
When the interactive session is started:
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
  - Use `haskell-tools-demo <working-dir>` command to start the demo service.
  - Visit `localhost` in the browser.

## Using GHCi

  - use `stack ghci`

## Test the code

  - The test folder contains the test package. The test suite contains both unit and nightly tests.
  - Run it with `stack test`.
  - Continous integration is backed by Travis-CI. Status: [![Travis](https://img.shields.io/travis/haskell-tools/haskell-tools/master.svg)](https://travis-ci.org/haskell-tools/haskell-tools)
  - Test coverage is automatically calculated, but is currently very low because of generated fields/instances/references defined for API consistency but not being used by any refactoring: [![Coverage Status](https://coveralls.io/repos/github/haskell-tools/haskell-tools/badge.svg)](https://coveralls.io/github/haskell-tools/haskell-tools)

## How to contribute / develop your own refactorings?

 - If you want to create new refactorings, check out the [tutorial](https://github.com/haskell-tools/haskell-tools/wiki/How-to-write-refactorings%3F). Good general purpose refactorings should be shared with other developers by creating pull requests.
 - If you want to help with the development of the framework, check out the [Developer Resources](https://github.com/haskell-tools/haskell-tools/wiki/Developer-Resources).

## Known limitations

  - Generate type signature does not generate `ScopedTypeVariables` extension and explicit `forall` in cases when it would be needed to create a compilable result. These have to be added manually.
  - Some semantic information is missing, for example, kinds of type variables, types of variables local to TH splices, fixity of locally defined operators.
  - The following extensions are not supported: `UnicodeSyntax`, `CPP`

## Plans
  
  - 2016: Refinements in project handling, performance improvements for generics.
  - Editor support in early 2017. Better layout handling. Refinement and wider variety of refactorings.

## Repository contents

This repository contains 5 packages that provide different functionality, so you can choose which ones you need.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-ast.svg)](http://hackage.haskell.org/package/haskell-tools-ast) **haskell-tools-ast** contains the representations of our syntax tree and utility functions.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-backend-ghc.svg)](http://hackage.haskell.org/package/haskell-tools-backend-ghc) **haskell-tools-backend-ghc** contains how can our AST be generated from the different representations of GHC.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-rewrite.svg)](http://hackage.haskell.org/package/haskell-tools-rewrite) **haskell-tools-rewrite** contains functions for changing parts of the syntax tree.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-prettyprint.svg)](http://hackage.haskell.org/package/haskell-tools-prettyprint) **haskell-tools-prettyprint** enables us to pretty print the AST in its original form.
  - [![Hackage](https://img.shields.io/hackage/v/haskell-tools-refactor.svg)](http://hackage.haskell.org/package/haskell-tools-refactor) **haskell-tools-refactor** defines the actual refactorings.

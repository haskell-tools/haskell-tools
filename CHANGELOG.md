# CHANGELOG

## 0.8
 - The tool now handles Main modules.
 - Only those modules are loaded that cabal would load.
 - Multiple improvements for the handling of Template Haskell.
 - Some minor fixes in loading and refactoring.

## 0.7
 - Implemented a complete handling of conditional compilation preprocessor pragmas.
 - Fixed problems about the scope-checking for validity.
 - Rename is now able to rename module aliases.
 - Removed unnecessary changes during re-loading.
 - Version checking between the client and the daemon.

## 0.6
 - Fixed a number of project-related bugs discovered from stackage testing.
 - Polished the editor support

## 0.5
 - New Refactoring: Float Out
 - Enhancements of existing refactorings:
   - Organize imports had been extended
     - Now recognizes import groups and does not reorder them
     - Uses a heuristics for removing imports
   - Extract binding now can extract operator sections, and recognizes known associative operators
 - Daemon: support for package DBs, automatically find cabal-sandbox and stack DBs
 - Created automatic testing for hackage and stackage, found and fixed a few problems (mostly transformation errors on edge cases)
 - Resolved formatting failures by defaulting to relative indentation for newly generated elements
 - Minor features for CLI

## 0.4

 - Inline Binding refactoring
 - Solved various issues of other refactorings:
   - Renaming a module produce a new module in the original module's source dir
   - Error for GenerateSignature when bindings have complex pattern left-hand-side
   - Resolved ExtractBinding indentation conflicts with case alternatives
   - Error for inline binding if the binding is not used
   - Generate type signature: detect when the generated signature needs type variables fixed
   - Elements now keep their indentation relative to parents when a binding is extracted out
 - Load compilation options from cabal file
 - Enable relative indentation for newly generated AST elements

## 0.3

 - Major API changes for refactorings. Refactorings are defined on the pure syntax tree instead of the annotated one.
 - Support for extensions: `TemplateHaskell`, `RecordWildcards`, `EmptyCase`

## 0.2

 - Support for transformation of multiple modules
 - Updates for the Rename Definition refactoring
 - Unified API for code generation
 - Command-line executable

## 0.1.3

  - Fixed performance-related issues.

## 0.1.2

  - Extract binding now generates a local binding. Fewer values must be passed as arguments.
  - Extract binding have better checks for when does it need to put values in parentheses.
  - Extract binding creates functions with arguments from lambdas.
  - Imported names are now present in the scope. Rename and extract binding is aware of them and prevents name clashes.

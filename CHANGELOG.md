# CHANGELOG

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

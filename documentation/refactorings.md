# Implemented refactorings

  - [Rename definition](refactorings/rename.md#rename-definition-refactoring): Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - [Extract binding](refactorings/extract-binding.md#extract-binding-refactoring): Extracts the selected expression as a local binding.
  - [Inline binding](refactorings/inline-binding.md#inline-binding-refactoring): Removes the selected binding and replaces the uses with binding's implementation.
  - [Generate type signature](refactorings/generate-signature.md#generate-type-signature-refactoring): Generates the type signature for a function. Useful for declaring the type if it is complex.
  - [Organize imports](refactorings/organize-imports.md#organize-imports-refactoring): Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - [Generate exports](refactorings/generate-exports.md#generate-exports-refactoring): Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.
  - [Float out](refactorings/float-out.md#float-out-refactoring): Moves the given local binding to an outer scope.

There are 7 implemented refactorings:
  - **Rename definition**: Can rename bindings, data types, constructors, fields, type variables, etc. Respects scoping.
  - **Extract binding**: Extracts the selected expression as a local binding.
  - **Inline binding**: Removes the selected binding and replaces the uses with binding's implementation.
  - **Generate type signature**: Generates the type signature for a function. Useful for declaring the type if it is complex.
  - **Organize imports**: Sorts the imports into alphabetical order and narrows the set of imported definitions to the ones that are really used.
  - **Generate exports**: Generate an export list for the module that contains all definitions in it. Useful for narrowing the list of exported definitions to the ones that need to be public.
  - **Float out**: Moves the given local binding to an outer scope.

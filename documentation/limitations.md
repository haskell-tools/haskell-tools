## Known limitations and problems

  - The refactorer currently only works on correct Haskell programs.
  - The preprocessor support is experimental.
    - It usually works for conditional code involving (`#if`, `#else`, `#endif` pragmas).
    - It does not support macro evaluation and include directives. The problems may come out at different stages of the transformation, from preventing the loading of modules to producing bogus source code as a result.
  - The tool does not refactor main modules defined by executables, tests and benchmarks. You should not put more code into these source files than what is needed to delegate the main function to a normal module.
  - We don't support generated Haskell code. This includes Literate Haskell, parser generators like alex, or `.hsc` files.
  - Packages are loaded into the same GHC session (as opposite to cabal for example, where packages and their components are compiled separately)
    - Two components that are using packages that define modules that have same names could not be used by the refactorer.
    - Some extensions are globally enabled if any component uses them. These are: ``
  - `ApplicativeDo` extension is not supported (it completely changes the syntax tree underlying a do-notation block)
  - `OverloadedLabels` extension is not supported (it implicitly binds unbound names and is risky for multiple refactorings)
  - We have issues with type checking `Template Haskell` brackets when they contain splices.
  - There are GHC code generation bugs that result in Segmentation fault/access violation errors when refactoring few of the packages. Similar issues have been spotted when using ghci to refactor code (without stack), and when profiling the tool.
  - Inlining a binding can cause typechecking errors because of ambiguous type variables. To prevent these cases we would need to re-typecheck the AST.
  - Organize imports works with rough heuristics for removing imports, and is not always successful.
  - When checking for reserved names, we don't check for names reserved by extensions.

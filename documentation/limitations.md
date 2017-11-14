## Known limitations and problems

### General limitations

  - The refactorer currently only works on correct Haskell programs.
  - Windows and Linux platforms are supported. OSX is not tested.
  
### Language limitations
  - The preprocessor support is experimental.
    - It usually works for conditional code involving (`#if`, `#else`, `#endif` pragmas).
    - It does not support macro evaluation and include directives. The problems may come out at different stages of the transformation, from preventing the loading of modules to producing bogus source code as a result.
  - We don't support generated Haskell code. This includes Literate Haskell, parser generators like alex, or `.hsc` files.
  - `ApplicativeDo` extension is not supported (it completely changes the syntax tree underlying a do-notation block)
  - `OverloadedLabels` extension is not supported (it implicitly binds unbound names and is risky for multiple refactorings)
  - `ImplicitParams` extension is not supported (there are problems with the scoping of the implicit bindings).
  - Packages imports (`import "packagename" ModuleName`, with `PackageImports` extension) are only supported for packages not loaded into Haskell-tools.
  - Defining names in a `TemplateHaskell` splice and reusing them in the same splice is not supported due to type-checking limitations.
  
### Project limitations
  - The supported project types are: implicit (just source files are present), cabal (`.cabal` file is present), cabal-sandbox and stack. No other project type will be recognized. The project type is auto-detected, but the auto-detection is not very precise. You can override the auto-detection with the `--project-type` command line flag.
  - Packages are loaded into the same GHC session (as opposite to cabal for example, where packages and their components are compiled separately)
    - Two components that are using packages that define modules that have same names could not be used by the refactorer.
    - Some extensions are globally enabled if any component uses them. These are: `CPP`, `ExplicitNamespaces`, `PatternSynonyms`, `PackageImports`, `MagicHash`
  - There are GHC code generation bugs that result in Segmentation fault/access violation errors when refactoring. This is a rare issue, and it is not clear what kind of source code triggers the error. Similar issues have been spotted when using ghci to refactor code (without stack), and when profiling the tool.
  - We don't support source directories outside of the project root folders.
  - Conditional cabal directives that depend on package versions will not be used when loading a package into Haskell-tools.
  - Only UTF-8 Haskell source files are supported.
  
### Limitations on the refactorings
  - Inlining a binding can cause typechecking errors because of ambiguous type variables.
  - Organize imports works with rough heuristics for removing imports, and is not always successful.
  - When checking for reserved names, we don't check for names reserved by extensions.

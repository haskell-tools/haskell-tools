# Using the command line refactorer

  - If you are using stack to build from source: `stack exec ht-refact -- [flags] package-roots`
  - Otherwise, install the `haskell-tools-cli` package and use `ht-refact [flags] package-roots`.
  - You can use [ghc flags](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html) to control how modules are loaded and checked.
  - By setting the `-one-shot`, `-module-name=<modulename>` and `-refactoring=<refactor-command>` flags, you can perform a refactoring without the interactive mode.
  - By using the `-dry-run` flag, the files will not be modified, the result will be printed on the output.

When the interactive session is started:
  - Select a module to refactor with `SelectModule modulename`
  - Use the [refactorings](refactorings.md):
    - `RenameDefinition src-range new-name`
    - `ExtractBinding src-range new-name`
    - `InlineBinding src-range`
    - `GenerateSignature src-range`
    - `OrganizeImports`
    - `GenerateExports`
    - `FloatOut src-range`
  - Source ranges can be given in the `startrow:startcol-endrow:endcol` format.
  - The CLI automatically reloads the changed modules.
  - When finished, use `Exit` to close the CLI.

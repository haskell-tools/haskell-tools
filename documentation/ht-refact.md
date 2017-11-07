# Using the command line refactorer

  - Running ht-refact: `ht-refact [flags] PACKAGE-ROOTS`. (`PACKAGE-ROOTS` stands for the directories containing the cabal files e.g.  `stack exec ht-refact -- .` or `stack exec ht-refact -- pkg1 pkg2`)
  - If you don't specify the commands to execute, when the project is loaded the program will start an interactive session. In this session you can use [commands](#commands) to perform refactorings. The source code will be reloaded after each refactoring. When you are finished you can quit the session with the `Exit` command.
  
## Options
  - `-h`, `--help`: Print out information about the command line arguments of ht-refact. (Does not start the program.)
  - `-v`, `--version`: Print out the version of ht-refact. (Does not start the program.)
  - `--verbose`: Debugging information is logged from the executable.
  - `-e COMMANDS`, `--execute=COMMANDS`: Run a one-shot refactoring, specifying the refactor command to execute after the project has been loaded. Multiple commands can be used separated by semicolons (`;`). See [possible commands](#commands).
  - `--no-watch`: Disables file system watching built into the program.
  - `-w`, `--watch-exe PATH`: Specify the location of the `hfswatch` program used for file system watching. If not specified it will be expected in the same directory where ht-refact is installed.
  - `--generate-code`: Always generate interpreted code for modules. Use in cases of GHC linker-related bugs.
  - `--no-history`: Disables saving the history of performed refactorings. Makes the `Undo` command unusable.
  - `-g OPTIONS`, `--ghc-options OPTIONS`: Pass flags to GHC to use when parsing and type checking modules. Overrides flags specified in cabal files. See the list of [GHC flags](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html). Multiple arguments can be used separated by spaces.
  - `--project-type PKG-DB`: Restricts the project type to the given one. `PKG-DB` can be one of the followings. `cabal` for normal cabal projects, `cabal-sandbox` for sandbox-based cabal projects, `stack` for Stack-based projects or a list of comma-separated file pathes to explicitely set the folders where Haskell packages are loaded.

## Commands
When the interactive session is started:
  - Use the [refactorings](refactorings.md):
    - `RenameDefinition MODULE SRC-RANGE NEW-NAME`
    - `ExtractBinding MODULE SRC-RANGE NEW-NAME`
    - `InlineBinding MODULE SRC-RANGE`
    - `GenerateSignature MODULE SRC-RANGE`
    - `OrganizeImports MODULE`
    - `GenerateExports MODULE`
    - `FloatOut MODULE SRC-RANGE`
    - `ProjectOrganizeImports`
  - A `MODULE` is the full module name (for example, `Control.Monad`), or a unique suffix of the absolute file path of a module (`Monad.hs` or `Control/Monad.hs`).
  - Source ranges can be given in the `startrow:startcol-endrow:endcol` format (for example `13:6-14:12`). If the start and the end position is the same you can omit the end (`13:6`). When supplying source ranges, please keep in mind that a tab character causes the insertion of enough spaces to align the current position with the next tab stop. Tab stops are 8 characters apart.
  - Writing `Try` before a refactoring command displays the changes as a unified diff instead of actually changing the source files. You can try out the results of a refactoring before you apply it.
  - When finished, use `Exit` to close the CLI.
  - Using `Undo` will take back the last refactoring if the sources were not modified since.
  - The program automatically reloads the changed modules. If you don't use file system watching (`--no-watch`) you can ask the program to re-load your modules by using the 3 commands, to simulate the modification, addition or deletion of a file:
    - `ChangeFile FILE-NAME`
    - `AddFile FILE-NAME`
    - `RemoveFile FILE-NAME`
  - Here `FILE-NAME` should be the absolute file path of the source file.

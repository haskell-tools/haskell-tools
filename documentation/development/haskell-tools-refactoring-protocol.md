
# Haskell-tools Refactoring protocol (HTRP)

## Client messages

### KeepAlive: `{"tag":"KeepAlive","contents":[]}`

Notifies the server that the client is still up and running. Currently it have no function other than checking that the server is alive. The server should respond with KeepAliveResponse.

### AddPackages: `{"tag":"AddPackages","addedPathes":[<pathes>]}`

Add the Haskell packages found at the given pathes to the refactoring session. Packages that are part of the refactoring session can be refactored and can be affected by global refactorings. The pathes can be absolute or relative pathes. If the packages are correct, there will be no response. If this command is sent with a package that is already loaded it will be re-loaded. This must be done, for example, when the .cabal file changes. Example: `{"tag":"AddPackages","addedPathes":["C:\\haskell\\project\\package1", "C:\\haskell\\project\\package2"]}`

### RemovePackages: `{"tag":"RemovePackages","removedPathes":[<pathes>]}`

Removes the selected packages from the refactoring session. The packages taken out of the refactoring session will not be modified when other packages are refactored. There is no response for that operation. Example: `{"tag":"RemovePackages","removedPathes":["C:\\haskell\\project\\package1"]}`

### PerformRefactoring: `{"tag":"PerformRefactoring","refactoring":<refactor-name>,"modulePath":<selected-module>,"editorSelection":<selected-range>,"details":[<refactoring-specific-data>],"shutdownAfter":<stop after refactoring>,"diffMode":<only send diff>}`

Asks the server to perform the selected refactoring on the given module, for the selected element. The changes may affect other modules. If the refactoring is successful, as a response it sends a LoadingModules message stating which modules will be reloaded and a LoadedModules for each module that has been changed or must be reloaded. Otherwise it sends an ErrorMessage as a response. Example: `{"tag":"PerformRefactoring","refactoring":"RenameDefinition","modulePath":"C:\\haskell\\project\\package1\\A.hs","editorSelection":"3:1-3:6","details":["newName"],"shutdownAfter":false,"diffMode":false}`

## SetGHCFlags `{"tag":"SetGHCFlags","ghcFlags":[]}`

The client sets the GHC flags for the session. These flags overwrite flags in the cabal files, but does not affect
the settings in the source files.

### Stop: `{"tag":"Stop","contents":[]}`

The client notifies the server that it is no longer needed and should stop. No response is expected.

### UndoLast: `{"tag":"UndoLast","contents":[]}`

The client asks the server to undo the last refactoring performed. Only works if there was no modification since.

### Disconnect: `{"tag":"Disconnect","contents":[]}`

The client notifies the server that it will disconnect, but the server should keep working. A Disconnected response is expected.

### ReLoad: `{"tag":"ReLoad","changedModules":[<pathes>], "removedModules":[<pathes>]}`

The client notifies the server that files have changed and/or have been deleted. As a response the server should send a LoadedModules response for each chagned module and the modules that need to be reloaded because of the changes. Example: `{"tag":"ReLoad","changedModules":["C:\\haskell\\project\\package1\\A.hs"], "removedModules":[]}`

## Server messages

### KeepAliveResponse: `{"tag":"KeepAliveResponse","contents":[]}`

A response for the KeepAlive message, notifying the client that the server is still running.

### ErrorMessage: `{"tag":"ErrorMessage","errorMsg":<message-text>}`

A message that tells the client of some error that happened. The client should display the error to the user. The error could be caused by an illegal refactoring, or might be an internal problem. The errors in the source code are sent as CompilationProblem response instead.

### CompilationProblem: `{"tag":"CompilationProblem","errorMsg":<message-text>}`

A message that is caused when the haskell source code sent by the client is not correct. The client should display these messages as markers.

### DiffInfo: `{"tag":"DiffInfo","diffInfo":<changes>}`

If `diffMode` was specified in a `PerformRefactoring` request, it sends back the changes as a unified diff.


### LoadingModules: `{"tag":"LoadingModules","modulesToLoad":[<file-pathes>]}`

The server notifies the client that the following modules will be re-loaded. The client could use this to show the user how many modules remain to be loaded. Each file in `modulesToLoad` will occur in one of the following `LoadedModules` messages.

### LoadedModules: `{"tag":"LoadedModules","loadedModules":[<changed-files>]}`

The server notifies the client that the given modules had been re-parsed and can be refactored. The client cannot perform refactoring on a file until it has been re-loaded. Global refactorings can only be performed if all modules are loaded.

Each changed file is in the format of `[<file-path>, <module-name>]`.

### Disconnected: `{"tag":"Disconnected","contents":[]}`

A response to the clients Disconnect message.

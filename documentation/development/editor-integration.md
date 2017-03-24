# Integrating Haskell Tools with an editor

If you decided to create a binding for Haskell Tools Refact, you can use the ht-daemon server. When you run the executable, it will listen on the given port (4123 by default) and respond to client messages as it is described in [the protocol description](haskell-tools-refactoring-protocol.md).

First, you should check the [reference implementation](https://github.com/nboldi/haskell-tools-atom), that is developed for the [Atom editor](https://atom.io/).

While writing this guide I assume the following things about your editor. If one or more statement is not true, than check the end of this guide for solutions.
 - It has the concept of projects and source folders inside the project.
 - It supports multi-threading and socket communication.
 - Can put markers in the source code (for signalling the location of errors or warnings).

## Components

The editor integration should have three main component. Both should be singleton objects.

**Server manager**
 - Starts the Haskell Tools engine executable.
 - Stops the server when needed.
 - *Optional:* restarts the server executable if it stopped for some reason.

**Client manager**
 - Maintains a socket connection with the server.
 - Sends messages to the server on user requests, or requests from other components.

**Package manager**
  - Handles adding and removing packages to the engine.
  - Uses the client manager to communicate with the engine.

Other optional components that may increase the usability of your integration:
 - Locating the engine executable at the first run.
 - Providing information about the status of the engine.
 - Keeping a history of the refactorings performed.
 - Dialogs for getting input from the user when performing a given refactoring.
 - Displaying markers in the editor when the source code is not correct.

## State

 - The integration should store which folders have been added to the engine. It should display this information somehow to the user.
 - It should also store some settings:
   - The location of the server executable
   - The port number to use for communication

## When to send messages?

 - `KeepAlive` if you need to check if the server is running.
 - `AddPackages` should be sent when the user adds a new package to the project. It should also be sent after Haskell Tool is started. In that case each selected package should be added.
 - `RemovePackages` should be sent when the user removes a package from the project.
 - If the user wants to perform a refactoring, the corresponding `PerformRefactoring` command should be sent. The `refactoring` field should be set to the name of the refactoring. Some refactoring need additional details, for example the name of the extracted definition. These should be asked from the user via input messages. The message should also contain the file that is currently viewed and the selected region (`modulePath` and `editorSelection`).
 - `ModulesChanged` tells the client that a refactoring changed some files on the disk and gives instructions to undo the changes if needed. If the editor does not re-load changed files automatically it can be used to reload the changed files. Otherwise it can be used to track changes of the files and be able to undo all changes of a refactoring.
 - `Stop` message is needed when the user closes the editor or decided to end the refactoring session and executes the Stop Command.
 - `Reload` is now the way for the editor to notify the server about changes, but **will be taken out**.

 Check the protocol description for details.

## What to do with the responses?

 - `KeepAliveResponse`: This is the standard response for `KeepAlive`. Shoud decrement the keep-alive counter.
 - `LoadingModules`: This message precedes all reloading of modules in the server. The client can use it to track the servers progress. All modules that will be re-loaded will appear here. This happens after adding packages and performing refactorings.
 - `LoadedModules`: Notifies the client that the given modules have been reloaded. The client can use it to update the progress of the reloading or check if some module is up-to-date.
 - `ErrorMessage`: This response is returned when the given refactoring cannot be performed out of a user or system error, it might mark the use of an unallowed language element.
 - `CompilationProblem`: This message is sent when the server cannot compile the submitted sources. Should display a marker at the source range given in the message, with the given problem text.

  An example of `CompilationProblem` response message. The first record specifies the location of the problem, the second specifies the message.

  ```json
  {"tag":"CompilationProblem","errorMarkers":[[{"endCol":22,"startRow":3,"endRow":3,"startCol":1,"file":".../A.hs"},"Failed to load interface for `No.Such.Module'\nUse -v to see a list of the files searched for."]]}
  ```

  The editor plugin should keep the message even if the given file is closed to show it when it is opened again.

Check the protocol description for details.

**TODO: how to check which files are up-to-date and which can be refactored**

If you decided to create a binding for Haskell Tools Refact, you can use the ht-daemon server. When you run the executable, it will listen on the given port (4123 by default) and respond to client messages as it is described in [the protocol description](haskell-tools-refactoring-protocol.md).

While writing this guide I assume the following things about your editor. If one or more statement is not true, than check the end of this guide for solutions.
 - It has the concept of projects and source folders inside the project.
 - It supports multi-threading and socket communication.
 - Can put markers in the source code (for signalling the location of errors or warnings).

## User Input

Create a submenu wherever you like. It should have:
 - A Start/Stop Haskell Tools Refact menu item
 - One menu item for each refactoring you want to use from Haskell Tools Refact

Associate keybindings with the new commands as you like.

Hook up listeners for the following actions:
 - A folder is added to the current project
 - A folder is removed from the current project

## State

The editor integration should have two main component, the server manager and the client manager. Both should be singleton objects.

Server manager:
 - Starts the Haskell Tools server instance.
 - Stops the server when needed.

Client manager
 - Maintains a socket connection with the server.
 - Sends messages to the server on object requests.
 - Checks if the server is still alive, otherwise restarts it.

### Client manager

The client manager should run two extra threads. One thread is used to receive server responses. It should do an infinite loop with a socket receive operation (`recv` in most languages), and perform the correct action for the incoming messages (see below). The other is for checking the server every one or two seconds. This thread should count how many `KeepAlive` messages did it send. If the number of unresponded messages go over a given threshold, than we can conclude that the server stopped and need to be restarted. If the server don't get the messages (for example, if the editor crashes) it will stop itself.

## When to send messages?

 - `KeepAlive` every one or two seconds, the thread for checking the server should send this.
 - `AddPackages` should be sent when the user adds a new package to the project. It should also be sent after Haskell Tool is started with each source folder in the package.
 - `RemovePackages` should be sent when the user removes a package from the project.
 - If the user wants to perform a refactoring, the corresponding `PerformRefactoring` command should be sent. The `refactoring` field should be set to the name of the refactoring. Some refactoring need additional details, for example the name of the extracted definition. These should be asked from the user via input messages. The message should also contain the file that is currently viewed and the selected region (`modulePath` and `editorSelection`).
 - `Stop` message is needed when the user closes the editor or decided to end the refactoring session and executes the Stop Command.
 - `Reload` is now the way for the editor to notify the server about changes, but **will be taken out**.

## What to do with the responses?

 - `KeepAliveResponse`: This is the standard response for `KeepAlive`. Shoud decrement the keep-alive counter.
 - `ErrorMessage`: This response is returned when the given refactoring cannot be performed out of a user or system error, it might mark the use of an unallowed language element.
 - `CompilationProblem`: This message is sent when the server cannot compile the submitted sources. Should display a marker at the source range given in the message, with the given problem text.

An example of `CompilationProblem` response message. The first record specifies the location of the problem, the second specifies the message.

```json
{"tag":"CompilationProblem","errorMarkers":[[{"endCol":22,"startRow":3,"endRow":3,"startCol":1,"file":".../A.hs"},"Failed to load interface for `No.Such.Module'\nUse -v to see a list of the files searched for."]]}
```

The editor plugin should care to keep the message even if the given file is closed to show it when it is opened again.

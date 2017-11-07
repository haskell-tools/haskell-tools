# Integrating Haskell Tools with an editor

If you decided to create a binding for Haskell Tools Refact, you can use the ht-daemon server. When you run the executable, it will listen on the given port (4123 by default) and respond to client messages as it is described in [the protocol description](haskell-tools-refactoring-protocol.md).

First, you should check the [reference implementation](https://github.com/nboldi/haskell-tools-atom), that is developed for the [Atom editor](https://atom.io/).

## Tasks
To allow the user to use the haskell-tools system in the editor, a plugin should be written that integrates it to the editor. 

 - It should manage a subprocess that runs the ht-daemon server. It should enable the user to start, stop and restart the process. Optionally it should automatically restart the process if terminated.
 - It should keep a socket connection with the server.
 - It should keep track of the packages that are loaded into the server. It should allow the user to add new packages as well as to remove them.
 - It should allow the user to use the [refactorings supported by the server](../refactorings.md). It should send the path of the focused file (absolute filepath) and the selected coordinates. When using the refactorings `RenameDefinition` or `ExtractBinding`, the plugin should allow the user to input a name before calling the refactoring.
   - It should calculate the logical coordinates of the cursor position when sending refactoring requests. Tab characters should expand to the next tab stop. Tab stops are 8 characters apart.
   - File contents are rewritten when the refactoring is done. It may appear to the editor as multiple changes, with a file becoming empty in between. Make sure that the cursor position is not changed by this.
 - When receiving error markers from the server, the plugin should display them on the source files in the editor. It should also mark the files and their parent folders to enable easy identification of the file with the error.
   - Make sure that markers are associated with files regardless if they are open or not.
   - Remove markers from files if they are reloaded.
   - Remove all markers when the server is stopped.
   - If the location of an error could not be decided, it should appear as a pop-up error message.
 - It should display the actual status of the haskell-tools server. It can be one of disconnected, ready, loading, compiling, refactoring or error.  
   - It is convenient for the user to see how many modules have been loaded and how many remain.

Check the [protocol description](haskell-tools-refactoring-protocol.md) for possible messages between the plugin and the haskell-tools server.

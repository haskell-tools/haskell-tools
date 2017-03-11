# Haskell-tools packages

We created separate packages for Haskell-tools that enable you to only import the functionality that your development tool needs.

Haskell-tools framework:

* **haskell-tools-ast**: The representation of a Haskell program. You need it every time you want to use Haskell-tools.
* **haskell-tools-ghc-backend**: Transforms programs from the GHC AST representation to the Haskell-tools representation. Needed when you use GHC as a backend. (Currently no other backend is available, but this may change in the future.)

Haskell-tools Refactor (sub)framework:

* **haskell-tools-prettyprint**: Enables you to print your AST after changing it while keeping the original formatting.
* **haskell-tools-rewrite**: A convenient way to build or deconstruct parts of the AST. Used for syntactical transformations of source code.
* **haskell-tools-refactor**: Support for writing code transformations, that rely on both syntactic and semantic information.

Application packages:

* **haskell-tools-daemon**: A backend for the Haskell-tools Refactor that supports editor integration. It exposes an interface that can be accessed through socket communication. The [protocol description](haskell-tools-refactoring-protocol.md) describes how the daemon should be used as a refactoring engine.
* **haskell-tools-cli**: A command-line interface for Haskell-tools Refactor. This can be used by scripts, or users who don't need editor integration. For an interactive usage, haskell-tools-daemon should be better.
* **haskell-tools-demo**: A demo web application that shows the capabilities of Haskell-tools Refactor


For example, refactoring uses all 5 framework packages. When writing a code analyzer tool based on GHC, I only need to use **haskell-tools-ast**, **haskell-tools-ghc-backend**. If I'm interested in debugging, I might also use **haskell-tools-prettyprint**.

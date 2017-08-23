# Tutorials for writing refactorings

The [Dollar application tutorial](tutorials/dollar-app.md) is the main tutorial for writing refactorings. It starts from a simple syntactical transformation and then step-by-step goes through the steps you need to take to perform it safer. In this tutorial you learn how to access semantic information and to automatically import used definitions.

Other tutorials:
 - The [Data to newtype tutorial](tutorials/data-to-newtype.md) teaches you the basics of changing the AST of a whole module.
 - The [Expression to guard tutorial](tutorials/expression-to-guard.md) shows how you can apply a given refactoring at the point the user selects, however it keeps the transformation simple.
 - We will be working on tutorials for more advanced topics. If you can't wait for them you should try to check the source code of [Builtin refactorings](https://github.com/haskell-tools/haskell-tools/tree/master/src/builtin-refactorings/Language/Haskell/Tools/Refactor/Builtin).

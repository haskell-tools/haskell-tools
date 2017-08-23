In this tutorial we will create a simple refactoring that transform each suitable data definition into newtype definitions. A type definition can be transformed if it has exactly one constructor that has exactly one field. This transformation does not change the semantics of the program, but may improve its performance.

## Preparations

First create a new module for our refactoring. Lets place it next to the ones that are already implemented and call it `Language.Haskell.Tools.Refactor.DataToNewtype`.

Import the References and the Haskell-tools API:

```haskell
import Control.Reference
import Language.Haskell.Tools.Refactor
```

## Entry point

Now create the main entry point of the refactoring. The refactoring only affects one module, so it will be a local refactoring(That means, the target of refactoring is only a Module). So its signature will be the following: `dataToNewtype :: Domain dom => LocalRefactoring dom`. The [`dom` parameter](https://github.com/haskell-tools/haskell-tools/wiki/AST-Domain) is not important now. A local refactoring is a monadic function that transforms a module. We can use the `modDecl & annList` reference to access all definitions in the module so the `modDecl & annList .- changeDeclaration` will apply the `changeDeclaration` function to all the declarations in the module.

```haskell
dataToNewtype = return . (modDecl & annList .- changeDeclaration)
```

## Transform declarations

The function that transforms bindings is a simple one, it transforms one declaration it is a suitable one into a newtype: `changeDeclaration :: Decl dom -> Decl dom`

We pattern match on the AST to find out if it is a data definition that have one constructor with one field. If so, then replaces the data keyword with a newtype keyword using the `.=` operator on the reference `declNewtype` that accesses the keyword of the type definition:

```haskell
changeDeclaration :: Decl dom -> Decl dom
changeDeclaration dd@(DataDecl DataKeyword ctx declHead (AnnList [ConDecl name (AnnList [arg])]) derivs)
  = declNewtype .= mkNewtypeKeyword $ dd
changeDeclaration decl = decl
```

For each other declaration we use a default case that does not change the code:

```haskell
changeDeclaration decl = decl
```

## Try the new refactoring

To try out our new refactoring we first need to import a few elements for a wrapper method:
```haskell
import Language.Haskell.Tools.Refactor
```

Now the last thing to do is to create a simple wrapper for our refactoring and try it out:

```haskell
tryItOut :: String -> IO ()
tryItOut moduleName = tryRefactor (localRefactoring $ dataToNewtype) moduleName
```

Here, [`localRefactoring`](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Utils/Monadic.hs) takes a refactoring that is defined on one module and makes it possible to use it on a whole project.

After that evaluating `tryItOut "Test"` will perform the transformation, and print the result:
```haskell
module Test where

data A = A Int
```
```haskell
module Test where

newtype A = A Int
```

The solution source code can be found [in the repository](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/DataToNewtype.hs).

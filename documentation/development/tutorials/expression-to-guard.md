In this tutorial we will create a simply refactoring that transforms a binding with an if on the right hand side by addig guards. I think in most situations it is a bad practice to use if expressions when the same could be written using guards.

During the tutorial we will pattern match on the AST representation, and generate new subtrees. We will also show how to use references to access and modify parts of the AST.

The refactoring will use the current selection to decide which binding(s) to transform. It will be usable on top-level or local refactorings.

So an example case would be to transform the max function:

```haskell
max a b = if a > b then a else b
```

It will be transformed to:

```haskell
max a b | a > b = a
        | otherwise = b
```

## Preparations

First create a new module for our refactoring. Lets place it next to the ones that are already implemented and call it `Language.Haskell.Tools.Refactor.IfToGuards`. We will use the `RankNTypes`, `FlexibleContexts` and `ViewPatterns` language extensions.

We will need to the Haskell-tools Refactor API use references, GHC source locations and uniplate instances. For trying out the refactoring we will use `runGhc`:

```haskell
import Language.Haskell.Tools.Refactor
import Control.Reference
import SrcLoc
import Data.Generics.Uniplate.Data
```

## Entry point

Now create the main entry point of the refactoring. The refactoring has one parameter, the current selection, and it only affects one module, so it will be a local refactoring. So its signature will be the following: `ifToGuards :: Domain dom => RealSrcSpan -> LocalRefactoring dom`.

The [AST domain](https://github.com/haskell-tools/haskell-tools/wiki/AST-Domain) (the semantic information contained in the AST) is irrelevant in this transformation, so we will only constrain the `dom` type parameter to be a correct `Domain`. This `ifToGuards` function will be the only definitions in this module that will be exported.

The refactoring will not change the whole module, just the part of it that is the selected binding. So we need to direct our refactoring to this part. To do this we use the `nodesContaining` reference is defined using uniplate and accesses all parts of the AST that contain the selected range. To use this reference we apply the [`.-` operator](http://hackage.haskell.org/package/references-0.3.2.0/docs/Control-Reference-Operators.html#v:.-45-). This operator applies a pure function to the part of the AST that is selected by the given reference. So the definition of `ifToGuards` will be:

```haskell
ifToGuards sp = return . (nodesContaining sp .- changeBindings)
```

## Transform bindings

The function that transforms bindings is a simple one: `changeBindings :: ValueBind dom -> ValueBind dom`

In our representation, bindings come in two flavors: `SimpleBind`s have a pattern on the left hand side. `FunBind`s have several `Match` parts, that represent possible pattern matches.

### Transform simple bindings

We use pattern matching to check that we have a simple binding that can be transformed: `SimpleBind (VarPat name) (UnguardedRhs (If pred thenE elseE)) locals`. The we construct the transformed binding using function from the [`Gen` modules](https://github.com/haskell-tools/haskell-tools/tree/0.3/src/rewrite/Language/Haskell/Tools/AST/Gen): `mkFunctionBind [mkMatch (mkMatchLhs name []) (createSimpleIfRhss pred thenE elseE) (locals ^. annMaybe) ]`. Here `createSimpleIfRhss` defines two guarded right-hand-sides (former then and else branches). We will reuse this definitions when dealing with function bindings.

```haskell
createSimpleIfRhss :: Expr dom -> Expr dom -> Expr dom -> Rhs dom
createSimpleIfRhss pred thenE elseE = mkGuardedRhss [ mkGuardedRhs [mkGuardCheck pred] thenE
                                                    , mkGuardedRhs [mkGuardCheck (mkVar (mkName "otherwise"))] elseE
                                                    ]
```

### Transform function bindings

To transform a function definition we must transform the right-hand-sides in all match. To do this, we use a composite reference that is created from simpler ones: `funBindMatches&annList&matchRhs`. We apply the following function, that transforms an unguarded right-hand-side into a guarded one:

```haskell
trfRhs :: Rhs dom -> Rhs dom
trfRhs (UnguardedRhs (If pred thenE elseE)) = createSimpleIfRhss pred thenE elseE
trfRhs e = e -- don't transform already guarded right-hand sides to avoid multiple evaluation of the same condition
```

## Try the new refactoring


Now the last thing to do is to create a simple wrapper for our refactoring and try it out:

```haskell
tryItOut moduleName sp = tryRefactor (localRefactoring $ ifToGuards (readSrcSpan (toFileName "." moduleName) sp)) moduleName
```

Here, [`localRefactoring`](https://github.com/haskell-tools/haskell-tools/tree/0.3/src/refactor/Language/Haskell/Tools/Refactor/RefactorBase.hs) takes a refactoring that is defined on one module and makes it possible to use it on a whole project. `readSrcSpan` is a utility function that creates a source range from a string like "3:1-3:20", so it goes from the first character of the 3rd row to the 20th character of the 3rd row. `toFileName` transforms a module name into the name of the corresponding `.hs` file.

After that evaluating `tryItOut "Test" "3:1-3:33"` will perform the transformation, and print the result:
```haskell
module Test where

max a b = if a > b then a else b
```
```haskell
module Test where

max a b | a > b = a
        | otherwise = b
```

The solution source code can be found [in the repository](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Predefined/IfToGuards.hs).

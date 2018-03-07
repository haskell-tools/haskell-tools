Refactoring tutorial: parentheses to dollar application
=================================

The task is to create a refactoring that replaces the selected function application using the dollar operator.

```haskell
x = f (g 1) -- becomes 'x = f $ g 1'
x2 = f (f (g 2)) -- becomes 'x2 = f $ f $ g 2'
```

First we create a simple transformation and then add check for different complications that could happen.

Preparation
-----------------

 - Install `haskell-tools-refactor` with cabal or from source with stack. See the [installation instructions](../../installation.md) for details.
 - Start from this `HelloRefactor` module that greets all subexpressions with maximum respect:

```haskell
module Language.Haskell.Tools.Refactor.Builtin.HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import Control.Reference
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . helloRefactor)

helloRefactor :: Domain dom => RealSrcSpan -> LocalRefactoring dom
helloRefactor sp = return . (nodesContained sp .- helloExpr)

helloExpr :: Expr dom -> Expr dom
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e

```

Now lets see what this code actually does:
  - It imports 2 modules from the haskell tools framework and 3 other modules.
    - `Language.Haskell.Tools.Refactor` provides us with all the framework support for writing refactorings. It supplies the definition of AST elements and the operations to manipulate them.
    - `Language.Haskell.Tools.PrettyPrint` provides the `prettyPrint` function that can produce the source code of the AST of modules or their fragments. It is normally used to get the refactored source code, but in this case we use it to show a piece of the original source code.
    - `Control.Reference` is a package that supports using references, that are an abstraction of properties. No we only use the `.-` operation. This operation is an update operation. It applies the function on the right side to the property on the left. If you want to learn more about references, check out the [reference tutorial](https://github.com/nboldi/references/wiki/References-Tutorial).
    - [`Debug.Trace`](http://hackage.haskell.org/package/base/docs/Debug-Trace.html) is a standard library module for debugging your programs. The `trace` function shows a message when it is evaluated while returning its second parameter as a result. It can be used to print information about your program when it is running. Now we use it to show the parts of the expression.
    - `SrcLoc` is a module from the GHC compiler. It provides use the `RealSrcSpan` type that is the representation of a range in a source file (it is file name, a starting position by row and column number and an end position by row and column number).
  - `tryItOut` is just a simple wrapper that enables us to show how the transformation of the syntax tree works. It simply performs the `helloRefactor` refactoring on a module with a given source range. The first parameter is the name of the module, the second is the source code range in which the expressions are shown.
    - There are two kinds of refactorings. A [`LocalRefactoring`](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Monad.hs) works on a single module, where a more general [`Refactoring`](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Monad.hs) works on a set of modules. The function [`localRefactoring`](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Utils/Monadic.hs) transforms a `LocalRefactoring` into a `Refactoring`.
  - In `helloRefactor`, the function needs a range to select the expression for which the subexpressions will be shown. It also have the restriction that the `dom` type variable should have a `Domain` instance. It only says that `dom` should be a valid semantic information kind, but does not reqire it to provide any kind of information yet. Later we will need some kinds of semantic information to be present, so we will change the required domain. If you want to learn more about domains, check out the [description of semantic information](../refactoring-guide.md#how-to-check-semantic-constraints)
  - `nodesContained` is a reference that accesses all the language elements that are inside a given source range. It is polymorphic in the type of the accessed element, so it can be used to access any type of language elements. Now we use it to access expressions inside a given range.
  - We use the `.-` update operator to apply the `helloExpr` function to all AST elements selected by `nodesContained`.
  - Since the `LocalRefactoring` type is monadic, but we calculated the result in a pure function (which is a bit of cheating, we use `trace` for printing the subexpressions), we need to use `return` to get a monadic result.
  - Finally in the `helloExpr` function, we simply get the source code representation of the current expression using `prettyPrint` to produce the output.

Now, test the results:

 - Create a simple `Test.hs` file to try out the HelloRefactor:

```haskell
module Test where

f = id
g = id

x = f (f (g 2))
```

 - Try it out in ghci: `ghci -package ghc HelloRefactor.hs`
 - In the result you will see the same module, but each selected subexpression will be greeted.

```
*HelloRefactor> tryOutHello "Test" "6:5-6:16"`
### Hello: 2

### Hello: g

### Hello: g 2

### Hello: (g 2)

### Hello: f

### Hello: f (g 2)

### Hello: (f (g 2))

### Hello: f

### Hello: f (f (g 2))
module Test where

f = id
g = id

x = f (f (g 2))
```


Create the basic transformation
----------------

 - Match a pattern for function application on a parenthesized expression (for example `f (g x)`). For general information see the pattern matching related part of the [refactoring guide](../refactoring-guide.md#how-to-pattern-match-on-elements). For the needed patterns, check out the [API for pattern matching on expressions](https://www.stackage.org/haddock/lts/haskell-tools-rewrite/Language-Haskell-Tools-Rewrite-Match-Exprs.html).
 - As a result, create an unqualified `$` operator and wrap it in an `InfixApp`. For general information about generating parts of expression, check out the relevant part of the [refactoring guide](../refactoring-guide.md#generating-parts-of-the-ast). For the needed generators, check out the [API for generating expressions](https://www.stackage.org/haddock/lts/haskell-tools-rewrite/Language-Haskell-Tools-Rewrite-Create-Exprs.html) and the [API for generating names](https://www.stackage.org/haddock/lts/haskell-tools-rewrite/Language-Haskell-Tools-Rewrite-Create-Names.html)
 - If the expression is not a function application on a parenthesized expression, we should return the expression unchanged.

Trying this out with our Test.hs should result in a transformed expression:
```
*DollarApp1> tryItOut "Test" "6:5-6:16"
module Test where

f = id
g = id

x = f $ f $ g 2
```

Handling NoImplicitPrelude
-----------------

The programmer may use the `NoImplicitPrelude` extension to explicitly select the definitions that are used from Prelude. In this case the compiler will not found the definition of `$`, so we must import it. Fortunately we can do it with Haskell-tools refactor monad support.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
module Test2 where

f = id
g = id

x = g (f (g 2))
```

Steps to solve this:
  - Get the unique name of the `$` operator. Since it is a wired-in name, we can get it from wiredInIds. We need to match the key from `PrelNames` with the Id of dollar from `PrelInfo`. Finally we get the `Name` from an `Id`:

  ```haskell
  [dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds
  ```

  Check out [GHC's name related types](https://downloads.haskell.org/~ghc/8.0.2/docs/html/libraries/ghc-8.0.2/Name.html). We need to import a few modules from GHC to do the trick:

  ```haskell
  import Unique (getUnique)
  import Id (idName)
  import PrelNames (dollarIdKey)
  import PrelInfo (wiredInIds)
  ```

  - Introduce the `LocalRefactor dom` monad to the type of `replaceExpr`.
    - Replace the pure update operator `.-` with its monadic counterpart `!~` where `replaceExpr` is used.
    - Rewrite the implementation of `replaceExpr` to produce a monadic value.
  - Constraint the domain to have semantic information about imports and modules. This is needed to enable auto-importing. We will need the `HasImportInfo` and `HasModuleInfo` typeclasses for `replaceExpr`.
  - Use `referenceOperator` monadic function to insert an operator and make sure that it is imported (the second parameter of the `InfixApp` constructor).

Transforming part of an infix expression
--------------------------------------------

When the function application is part of an infix expression, introducing the dollar operator changes the meaning of the expression (`f (g 3) + 3` is not the same as `f $ g 3 + 3`). Use the following example:

```haskell
module Test3 where

f = id
g = id

x = f (g 2) + 3
```

To solve this problem, we must
 - Introduce a state monad to the transformation, storing a list of source ranges (`SrcSpan`s). We need to store which expressions we refactored to be able to check that in a later stage. Use the `LocalRefactorT` monad transformer to add `State` to the monadic computation.
 - When transforming `f (g 3)`, store the location of the argument to be able to identify the generated expression later. Use `getRange` to get the source range of an annotated AST element and store it in the monadic state.
 - After the transformation, run a second pass (a monadic function), that is not limited to the selected range: `biplateRef !~ parenExpr`
 - In the `parenExpr` function we have to find infix expressions thats left or right-hand side have been transformed by the earlier pass. We can identify the transformed expressions from the ranges of their right-hand side (what was the argument of the function application before transformed).
 - We must wrap these sides into parentheses with the `Paren` element. Use the generator function as done earlier.
 - Of course if the whole expression (and not just the right side) is generated than we can leave the parens. (So `f (g (h 0))` becomes `f $ g $ h 0` rather than `f $ (g $ h 0)`)

Checking for another similar operator
---------------------------------------

If a different operator with 0 precedence is used inside the function argument (and it is not left-associative), in the result there will be a conflict between the two operators. So if for example `$$` is such an operator, we cannot perform the transformation on `f (g $$ 1)`. Use the following test file:

```haskell
module Test4 where

($$) :: (a -> b) -> a -> b
f $$ a = f a

f = id
g = id

infixl 0 $$

x = f (g $$ 1)
```

  - We will match on the case when there is an infix expression inside the parentheses.
  - First we must be sure that the expression we are checking is not generated in an earlier pass, because semantic information is not present on generated AST elements.
  - We only need to do the check if the operator is not the real `$` operator (multiple `$` operators can be used). To decide if the operator is `$` we access the unique name of the operator using `semanticsName`. Please keep in mind that the unique name is not associated with the operator but the `QualifiedName` inside.
  - Even if the operator is not `$`, it may have a nonzero precedence, so the transformation can continue (the operator will bind stronger than `$` and the result will be correct). Use the `semanticsFixity` function to access fixity information. (The result will be of GHC's `Fixity` type). Missing fixity might mean locally defined operator, so we must also stop the refactoring in that case (we cannot be sure).
  - If the operator is not `$` and has precedence zero, we should return the result unchanged (or maybe return an error message).
  - We also need to add two new domain constraints: `HasFixityInfo`, `HasNameInfo`

The solution source code can be found in the repository:
  1. [HelloRefactor](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/HelloRefactor.hs).
  1. [Basic transformation](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/DollarApp1.hs).
  1. [Checking `NoImplicitPrelude`](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/DollarApp2.hs).
  1. [Working with infix operators](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/DollarApp3.hs).
  1. [Final version](https://github.com/haskell-tools/haskell-tools/blob/master/src/experimental-refactorings/Language/Haskell/Tools/Refactor/Builtin/DollarApp.hs).

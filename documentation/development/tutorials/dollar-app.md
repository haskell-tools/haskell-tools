
The task is to create a refactoring that replaces the selected function application using the dollar operator.

```haskell
x = f (g 1)
x2 = f (f (g 2))
```
when the whole right-hand sides are selected will be
```haskell
x = f $ g 1
x2 = f $ f $ g 2
```

Preparation
-----------------

 - Install `haskell-tools-refactor-0.2.0.0` with cabal
 - Start from this HelloWorld example that greets all subexpressions with maximum respect:

```haskell
module HelloRefactor where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase

import SrcLoc

import Control.Reference hiding (element)
import Data.Generics.Uniplate.Data
import Debug.Trace

tryOutHello moduleName sp = tryRefactor (localRefactoring $ helloRefactor (readSrcSpan (toFileName "." moduleName) sp)) moduleName

helloRefactor :: Domain dom => RealSrcSpan -> LocalRefactoring dom
helloRefactor sp = return . (nodesContained sp .- helloExpr)

helloExpr :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e
```

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

 - Create a simple helper function to use as view pattern for accessing unannotated AST elements: `e = (^. element)`
 - Match a pattern for function application on a parenthesized expression. Remember to use the `e` function to get unannotated AST elements.
 - As a result, create an unqualified `$` operator and wrap it in an `InfixApp`. Use functions from the [Expr](http://hackage.haskell.org/package/haskell-tools-ast-gen-0.2.0.0/docs/Language-Haskell-Tools-AST-Gen-Exprs.html) and [Base](http://hackage.haskell.org/package/haskell-tools-ast-gen-0.2.0.0/docs/Language-Haskell-Tools-AST-Gen-Base.html) modules from [ast-gen] package.
 - As a default case, we should return the expression unchanged.

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
  - Get the unique name of the `$` operator. Since it is a wired-in name, we can get it from wiredInIds: `[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds`. We also need to import a few modules from GHC to do the trick:

```haskell
import Unique (getUnique)
import Id (idName)
import PrelNames (dollarIdKey)
import PrelInfo (wiredInIds)
```
  - Introduce the `LocalRefactor dom` monad to the type of the transformation (`dom` is the same as the domain of the result).
  - Now we must use the monadic update operator on the reference accessing contained nodes: `nodesContained sp !~ replaceExpr`.
  - Add some constraints on the domain to enable auto-importing: `(HasImportInfo dom, HasModuleInfo dom)`.
  - Use `referenceOperator` monadic function to insert an operator and make sure that it is imported.

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
 - Introduce a state monad to the transformation, storing a list of source ranges (`SrcSpan`s). Use the `LocalRefactorT` monad transformer to add `State` to the monadic computation.
 - When transforming `f (g 3)`, store the location of the argument to be able to identify the generated expression later. Use `getRange` to get the source range of an annotated AST element.
 - After the transformation, run a second pass, that is not limited to the selected range: `biplateRef !~ parenExpr @dom`
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
  - First we must be sure that the expression we are checking is not generate, because semantic informaition is not present on generated AST elements.
  - We check if the operator is really `$` (because multiple `$` operators can be used), we access the unique name of the operator using `semanticsName`. Please keep in mind that the unique name is not associated with the operator but the `QualifiedName` inside.
  - Even if the operator is not `$`, it may have a nonzero precedence, so the transformation can continue. Use the `semanticsFixity` function to access fixity information. (The result will be of GHC's `Fixity` type). Missing fixity might mean locally defined operator, so we must also stop the refactoring in that case.
  - If the operator is not `$` and don't have a nonzero precedence, we should return the result unchanged.
  - We also need to add two new domain constraints: `HasFixityInfo dom, HasNameInfo dom`

The solution source code can be found [in the repository](https://github.com/haskell-tools/haskell-tools/blob/master/src/refactor/Language/Haskell/Tools/Refactor/Predefined/DollarApp.hs).

module Language.Haskell.Tools.Refactor.Builtin.HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . helloRefactor)

helloRefactor :: Domain dom => RealSrcSpan -> LocalRefactoring dom
helloRefactor sp = return . (nodesContained sp .- helloExpr)

helloExpr :: Expr dom -> Expr dom
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e

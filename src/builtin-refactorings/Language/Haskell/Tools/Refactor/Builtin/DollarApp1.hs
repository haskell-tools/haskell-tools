module Language.Haskell.Tools.Refactor.Builtin.DollarApp1 where

import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

dollarApp :: Domain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = return . (nodesContained sp .- replaceExpr)

replaceExpr :: Expr dom -> Expr dom
replaceExpr (App fun (Paren arg)) = mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr e = e

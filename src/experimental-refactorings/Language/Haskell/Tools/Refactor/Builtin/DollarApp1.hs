{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Language.Haskell.Tools.Refactor.Builtin.DollarApp1 where

import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

dollarApp :: RealSrcSpan -> LocalRefactoring
dollarApp sp = return . (nodesContained sp .- replaceExpr)

replaceExpr :: Expr -> Expr
replaceExpr (App fun (Paren arg)) = mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr e = e

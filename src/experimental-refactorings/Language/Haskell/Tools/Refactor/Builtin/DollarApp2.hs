{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin.DollarApp2 where

import Language.Haskell.Tools.Refactor

import Control.Reference ((!~))
import Id (idName)
import PrelInfo (wiredInIds)
import PrelNames (dollarIdKey)
import SrcLoc (RealSrcSpan)
import Unique (getUnique)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

dollarApp :: RealSrcSpan -> LocalRefactoring
dollarApp sp = nodesContained sp !~ replaceExpr

replaceExpr :: Expr -> LocalRefactor Expr
replaceExpr (App fun (Paren arg)) = mkInfixApp fun <$> referenceOperator dollarName <*> pure arg
replaceExpr e = pure e

[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

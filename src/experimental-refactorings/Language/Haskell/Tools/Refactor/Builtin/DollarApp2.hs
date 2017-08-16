{-# LANGUAGE FlexibleContexts #-}
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

dollarApp :: (HasImportInfo dom, HasModuleInfo dom) => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = nodesContained sp !~ replaceExpr

replaceExpr :: (HasImportInfo dom, HasModuleInfo dom) => Expr dom -> LocalRefactor dom (Expr dom)
replaceExpr (App fun (Paren arg)) = mkInfixApp fun <$> referenceOperator dollarName <*> pure arg
replaceExpr e = pure e

[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

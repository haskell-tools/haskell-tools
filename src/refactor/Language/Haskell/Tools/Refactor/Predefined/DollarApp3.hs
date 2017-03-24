{-# LANGUAGE ViewPatterns, FlexibleContexts, ConstraintKinds #-}
module Language.Haskell.Tools.Refactor.Predefined.DollarApp3 where

import Language.Haskell.Tools.Refactor

import BasicTypes (Fixity(..))
import Id (idName)
import qualified Name as GHC (Name)
import PrelInfo (wiredInIds)
import PrelNames (dollarIdKey)
import SrcLoc (RealSrcSpan, SrcSpan)
import Unique (getUnique)

import Control.Monad.State
import Control.Reference ((^.), (!~), biplateRef)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

type DollarMonad dom = StateT [SrcSpan] (LocalRefactor dom)
type DollarDomain dom = (HasImportInfo dom, HasModuleInfo dom, HasFixityInfo dom, HasNameInfo dom)

dollarApp :: DollarDomain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = flip evalStateT [] . ((nodesContained sp !~ (\e -> get >>= replaceExpr e))
                                        >=> (biplateRef !~ parenExpr))

replaceExpr :: DollarDomain dom => Expr dom -> [SrcSpan] -> DollarMonad dom (Expr dom)
replaceExpr (App fun (Paren arg)) _ = do modify $ (getRange arg :)
                                         mkInfixApp fun <$> lift (referenceOperator dollarName) <*> pure arg
replaceExpr e _ = return e

parenExpr :: Expr dom -> DollarMonad dom (Expr dom)
parenExpr e = (exprLhs !~ parenDollar True) =<< (exprRhs !~ parenDollar False $ e)

parenDollar :: Bool -> Expr dom -> DollarMonad dom (Expr dom)
parenDollar lhs expr@(InfixApp _ _ arg)
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges && (lhs || getRange expr `notElem` replacedRanges)
         then return $ mkParen expr
         else return expr
parenDollar _ e = return e

dollarName :: GHC.Name
[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Language.Haskell.Tools.Refactor.Builtin.DollarApp (dollarApp, tryItOut) where

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

type DollarMonad = StateT [SrcSpan] LocalRefactor

dollarApp :: RealSrcSpan -> LocalRefactoring
dollarApp sp = flip evalStateT [] . ((nodesContained sp !~ (\e -> get >>= replaceExpr e))
                                        >=> (biplateRef !~ parenExpr))

replaceExpr :: Expr -> [SrcSpan] -> DollarMonad Expr
replaceExpr expr@(App _ (Paren (InfixApp _ op arg))) replacedRanges
  | not (getRange arg `elem` replacedRanges)
  , semanticsName (op ^. operatorName) /= Just dollarName
  , case semanticsFixity (op ^. operatorName) of Just (Fixity _ p _) | p > 0 -> False; _ -> True
  = return expr
replaceExpr (App fun (Paren arg)) _ = do modify $ (getRange arg :)
                                         mkInfixApp fun <$> lift (referenceOperator dollarName) <*> pure arg
replaceExpr e _ = return e

parenExpr :: Expr -> DollarMonad Expr
parenExpr e = (exprLhs !~ parenDollar True) =<< (exprRhs !~ parenDollar False $ e)

parenDollar :: Bool -> Expr -> DollarMonad Expr
parenDollar lhs expr@(InfixApp _ _ arg)
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges && (lhs || getRange expr `notElem` replacedRanges)
         then return $ mkParen expr
         else return expr
parenDollar _ e = return e

dollarName :: GHC.Name
[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

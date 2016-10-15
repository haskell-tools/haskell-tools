{-# LANGUAGE ViewPatterns, FlexibleContexts, ConstraintKinds #-}
module Language.Haskell.Tools.Refactor.DollarApp (dollarApp) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase

import SrcLoc (RealSrcSpan, SrcSpan)
import Unique (getUnique)
import Id (idName)
import PrelNames (dollarIdKey)
import PrelInfo (wiredInIds)
import BasicTypes (Fixity(..))

import Control.Monad.State
import Control.Reference hiding (element)
import Data.Generics.Uniplate.Data
import Debug.Trace

tryItOut moduleName sp 
  = tryRefactor (localRefactoring $ dollarApp (readSrcSpan (toFileName "." moduleName) sp)) moduleName

type DollarMonad dom = StateT [SrcSpan] (LocalRefactor dom)
type DollarDomain dom = (HasImportInfo dom, HasModuleInfo dom, HasFixityInfo dom, HasNameInfo dom)

dollarApp :: DollarDomain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = flip evalStateT [] . ((nodesContained sp !~ (\e -> get >>= replaceExpr e)) 
                                        >=> (biplateRef !~ parenExpr))

replaceExpr :: DollarDomain dom => Ann Expr dom SrcTemplateStage -> [SrcSpan] 
                                     -> DollarMonad dom (Ann Expr dom SrcTemplateStage)
replaceExpr expr@(App fun (Paren (InfixApp _ op arg))) replacedRanges
  | not (getRange arg `elem` replacedRanges)
  , sema <- op ^. element&operatorName&semantics
  , semanticsName sema /= Just dollarName 
  , case semanticsFixity sema of Just (Fixity _ p _) | p > 0 -> False; _ -> True
  = return expr
replaceExpr (App fun (Paren arg)) _ = do modify $ (getRange arg :)
                                         mkInfixApp fun <$> lift (referenceOperator dollarName) <*> pure arg
replaceExpr e _ = return e

parenExpr :: Ann Expr dom SrcTemplateStage -> DollarMonad dom (Ann Expr dom SrcTemplateStage)
parenExpr e = (element&exprLhs !~ parenDollar True) =<< (element&exprRhs !~ parenDollar False $ e)

parenDollar :: Bool -> Ann Expr dom SrcTemplateStage -> DollarMonad dom (Ann Expr dom SrcTemplateStage)
parenDollar lhs expr@(InfixApp _ _ arg) 
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges && (lhs || getRange expr `notElem` replacedRanges)
         then return $ mkParen expr
         else return expr
parenDollar _ e = return e

[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

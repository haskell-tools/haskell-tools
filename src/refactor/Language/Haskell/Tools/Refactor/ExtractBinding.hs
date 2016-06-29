
{-# LANGUAGE ViewPatterns
           , RankNTypes
           , FlexibleContexts
           , TypeApplications
           #-}
module Language.Haskell.Tools.Refactor.ExtractBinding where

import qualified GHC
import qualified Var as GHC
import qualified OccName as GHC hiding (varName)
import SrcLoc
import Unique

import Data.Char
import Data.Maybe
import Data.Generics.Uniplate.Data
import Control.Reference hiding (element)
import Control.Monad.State
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

import Debug.Trace

type STWithId = STWithNames GHC.Id

extractBinding' :: RealSrcSpan -> String -> Ann Module STWithId -> RefactoredModule GHC.Id
extractBinding' sp name mod
  = if isValidBindingName name then extractBinding (nodesContaining sp) (nodesContaining sp) name mod
                               else refactError "The given name is not a valid for the extracted binding"

extractBinding :: Simple Traversal (Ann Module STWithId) (Ann ValueBind STWithId)
                   -> Simple Traversal (Ann ValueBind STWithId) (Ann Expr STWithId)
                   -> String -> Ann Module STWithId -> RefactoredModule GHC.Id
extractBinding selectDecl selectExpr name mod
  = let conflicting = any @[] (isConflicting name) (mod ^? selectDecl & biplateRef)
        exprRange = head (mod ^? selectDecl & selectExpr & annotation & sourceInfo & sourceTemplateRange)
        declRange = last (mod ^? selectDecl & annotation & sourceInfo & sourceTemplateRange)
     in if conflicting
           then refactError "The given name causes name conflict."
           else do (res, st) <- runStateT (selectDecl&selectExpr !~ extractThatBind name $ mod) Nothing
                   case st of Just def -> return $ evalState (selectDecl&element !~ addLocalBinding declRange exprRange def $ res) False
                              Nothing -> refactError "There is no applicable expression to extract."

isConflicting :: String -> Ann SimpleName STWithId -> Bool
isConflicting name used
  = (used ^? semantics & isDefined) == Just True
      && (GHC.occNameString . GHC.getOccName <$> (used ^? semantics & nameInfo)) == Just name

-- Replaces the selected expression with a call and generates the called binding.
extractThatBind :: String -> Ann Expr STWithId -> StateT (Maybe (Ann ValueBind STWithId)) (Refactor GHC.Id) (Ann Expr STWithId)
extractThatBind name e 
  = do ret <- get
       if (isJust ret) then return e 
          else case (e ^. element) of
            Paren {} -> do modified <- doExtract name (fromJust $ e ^? element & exprInner)
                           element & exprInner != modified $ e
            Var {} -> lift $ refactError "The selected expression is too simple to be extracted."
            el | isParenLikeExpr el -> mkParen <$> doExtract name e
            el -> doExtract name e

addLocalBinding :: SrcSpan -> SrcSpan -> Ann ValueBind STWithId -> ValueBind STWithId -> State Bool (ValueBind STWithId)
addLocalBinding declRange exprRange local bind 
  = do done <- get
       if not done then do put True
                           return $ doAddBinding declRange exprRange local bind
                   else return bind 
  where
    doAddBinding declRng _ local sb@(SimpleBind {}) = valBindLocals .- insertLocalBind declRng local $ sb
    doAddBinding declRng (RealSrcSpan rng) local fb@(FunBind {}) 
      = funBindMatches & annList & filtered (isInside rng) & element & matchBinds .- insertLocalBind declRng local $ fb

insertLocalBind :: SrcSpan -> Ann ValueBind STWithId -> AnnMaybe LocalBinds STWithId -> AnnMaybe LocalBinds STWithId
insertLocalBind declRng toInsert locals 
  | isAnnNothing locals
  , RealSrcSpan rng <- declRng = -- creates the new where clause indented 2 spaces from the declaration
                                 mkLocalBinds (srcLocCol (realSrcSpanStart rng) + 2) [mkLocalValBind toInsert]
  | otherwise = annJust & element & localBinds .- insertWhere (mkLocalValBind toInsert) (const True) isNothing $ locals

-- | All expressions that are bound stronger than function application.
isParenLikeExpr :: Expr a -> Bool
isParenLikeExpr (If {}) = True
isParenLikeExpr (Paren {}) = True
isParenLikeExpr (List {}) = True
isParenLikeExpr (ParArray {}) = True
isParenLikeExpr (LeftSection {}) = True
isParenLikeExpr (RightSection {}) = True
isParenLikeExpr (RecCon {}) = True
isParenLikeExpr (RecUpdate {}) = True
isParenLikeExpr (Enum {}) = True
isParenLikeExpr (ParArrayEnum {}) = True
isParenLikeExpr (ListComp {}) = True
isParenLikeExpr (ParArrayComp {}) = True
isParenLikeExpr (BracketExpr {}) = True
isParenLikeExpr (Splice {}) = True
isParenLikeExpr (QuasiQuoteExpr {}) = True
isParenLikeExpr _ = False

doExtract :: String -> Ann Expr STWithId -> StateT (Maybe (Ann ValueBind STWithId)) (Refactor GHC.Id) (Ann Expr STWithId)
doExtract name e = do params <- lift $ getExternalBinds e
                      put (Just (generateBind name params e))
                      return (generateCall name params)

-- | Gets the values that have to be passed to the extracted definition
getExternalBinds :: Ann Expr STWithId -> Refactor GHC.Id [Ann Name STWithId]
getExternalBinds expr = map exprToName . keepFirsts <$> filterM isApplicableName (expr ^? uniplateRef)
  where isApplicableName name@(fmap GHC.varName . getExprNameInfo -> Just nm) 
          = ((nm `elem` namesDefinedOutside) &&) <$> isLocalName nm 
        isApplicableName _ = return False

        getExprNameInfo :: Ann Expr STWithId -> Maybe GHC.Var
        getExprNameInfo expr = (listToMaybe $ expr ^? element & (exprName&element&simpleName &+& exprOperator&element&operatorName) 
                                                              & semantics&nameInfo)

        exprToName :: Ann Expr STWithId -> Ann Name STWithId
        exprToName e | Just n <- e ^? element & exprName                     = n
                     | Just op <- e ^? element & exprOperator & element & operatorName = mkParenName op

        namesDefinedOutside :: [GHC.Name]
        namesDefinedOutside = expr ^? semantics & scopedLocals & traversal & traversal

        allNames :: [Ann SimpleName STWithId]
        allNames = expr ^? biplateRef

        isLocalName n = isNothing <$> GHC.lookupName n

        keepFirsts (e:rest) = e : keepFirsts (filter (/= e) rest)
        keepFirsts [] = []

-- | Generates the expression that calls the local binding
generateCall :: String -> [Ann Name STWithId] -> Ann Expr STWithId
generateCall name args = foldl (\e a -> mkApp e (mkVar a)) (mkVar $ mkNormalName $ mkSimpleName name) args

-- | Generates the local binding for the selected expression
generateBind :: String -> [Ann Name STWithId] -> Ann Expr STWithId -> Ann ValueBind STWithId
generateBind name [] e = mkSimpleBind (mkVarPat $ mkNormalName $ mkSimpleName name) (mkUnguardedRhs e) Nothing
generateBind name args e = mkFunctionBind [mkMatch (mkNormalMatchLhs (mkNormalName $ mkSimpleName name) (map mkVarPat args)) (mkUnguardedRhs e) Nothing]

isValidBindingName :: String -> Bool
isValidBindingName [] = False
isValidBindingName (firstChar:rest) = isIdStartChar firstChar && all isIdChar rest
  where isIdStartChar c = (isLetter c && isLower c && isAscii c) || c == '\'' || c == '_'
        isIdChar c = (isLetter c && isAscii c) || c == '\'' || c == '_' || isDigit c

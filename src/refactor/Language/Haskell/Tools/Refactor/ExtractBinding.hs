
{-# LANGUAGE ViewPatterns
           , ScopedTypeVariables
           , RankNTypes
           , FlexibleContexts
           , TypeApplications
           , ConstraintKinds
           , TypeFamilies
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

type Ann' e dom = Ann e dom SrcTemplateStage 
type AnnMaybe' e dom = AnnMaybe e dom SrcTemplateStage 

type ExtractBindingDomain dom = ( Domain dom, HasNameInfo (SemanticInfo' dom SameInfoNameCls), HasDefiningInfo (SemanticInfo' dom SameInfoNameCls)
                                , HasScopeInfo (SemanticInfo' dom SameInfoExprCls) )

extractBinding' :: ExtractBindingDomain dom => RealSrcSpan -> String -> LocalRefactoring dom
extractBinding' sp name mod
  = if isValidBindingName name then extractBinding (nodesContaining sp) (nodesContaining sp) name mod
                               else refactError "The given name is not a valid for the extracted binding"

extractBinding :: forall dom . ExtractBindingDomain dom => Simple Traversal (Ann' Module dom) (Ann' ValueBind dom)
                   -> Simple Traversal (Ann' ValueBind dom) (Ann' Expr dom)
                   -> String -> LocalRefactoring dom
extractBinding selectDecl selectExpr name mod
  = let conflicting = any (isConflicting name) (mod ^? selectDecl & biplateRef :: [Ann' QualifiedName dom])
        exprRange = getRange $ head (mod ^? selectDecl & selectExpr & annotation & sourceInfo)
        decl = last (mod ^? selectDecl)
        declRange = getRange $ last (mod ^? selectDecl & annotation & sourceInfo)
     in if conflicting
           then refactError "The given name causes name conflict."
           else do (res, st) <- runStateT (selectDecl&selectExpr !~ extractThatBind name (head $ decl ^? actualContainingExpr exprRange) $ mod) Nothing
                   case st of Just def -> return $ evalState (selectDecl&element !~ addLocalBinding declRange exprRange def $ res) False
                              Nothing -> refactError "There is no applicable expression to extract."

isConflicting :: ExtractBindingDomain dom => String -> Ann' QualifiedName dom -> Bool
isConflicting name used
  = semanticsDefining (used ^. semantics)
      && (GHC.occNameString . GHC.getOccName <$> semanticsName (used ^. semantics)) == Just name

-- Replaces the selected expression with a call and generates the called binding.
extractThatBind :: ExtractBindingDomain dom => String -> Ann' Expr dom -> Ann' Expr dom -> StateT (Maybe (Ann' ValueBind dom)) (LocalRefactor dom) (Ann' Expr dom)
extractThatBind name cont e 
  = do ret <- get
       if (isJust ret) then return e 
          else case (e ^. element) of
            Paren {} | hasParameter -> element & exprInner !~ doExtract name cont $ e
                     | otherwise -> doExtract name cont (fromJust $ e ^? element & exprInner)
            Var {} -> lift $ refactError "The selected expression is too simple to be extracted."
            el | isParenLikeExpr el && hasParameter -> mkParen <$> doExtract name cont e
            el -> doExtract name cont e
  where hasParameter = not (null (getExternalBinds cont e))

addLocalBinding :: SrcSpan -> SrcSpan -> Ann' ValueBind dom -> ValueBind dom SrcTemplateStage -> State Bool (ValueBind dom SrcTemplateStage)
addLocalBinding declRange exprRange local bind 
  = do done <- get
       if not done then do put True
                           return $ doAddBinding declRange exprRange local bind
                   else return bind 
  where
    doAddBinding declRng _ local sb@(SimpleBind {}) = valBindLocals .- insertLocalBind declRng local $ sb
    doAddBinding declRng (RealSrcSpan rng) local fb@(FunBind {}) 
      = funBindMatches & annList & filtered (isInside rng) & element & matchBinds .- insertLocalBind declRng local $ fb

insertLocalBind :: SrcSpan -> Ann' ValueBind dom -> AnnMaybe' LocalBinds dom -> AnnMaybe' LocalBinds dom
insertLocalBind declRng toInsert locals 
  | isAnnNothing locals
  , RealSrcSpan rng <- declRng = -- creates the new where clause indented 2 spaces from the declaration
                                 mkLocalBinds (srcLocCol (realSrcSpanStart rng) + 2) [mkLocalValBind toInsert]
  | otherwise = annJust & element & localBinds .- insertWhere (mkLocalValBind toInsert) (const True) isNothing $ locals

-- | All expressions that are bound stronger than function application.
isParenLikeExpr :: Expr dom st -> Bool
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

doExtract :: ExtractBindingDomain dom => String -> Ann' Expr dom -> Ann' Expr dom -> StateT (Maybe (Ann' ValueBind dom)) (LocalRefactor dom) (Ann' Expr dom)
doExtract name cont e@((^. element) -> lam@(Lambda {}))
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params ++ (lam ^? exprBindings&annList)) (fromJust $ lam ^? exprInner)))
       return (generateCall name params)
doExtract name cont e 
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params) e))
       return (generateCall name params)

-- | Gets the values that have to be passed to the extracted definition
getExternalBinds :: ExtractBindingDomain dom => Ann' Expr dom -> Ann' Expr dom -> [Ann' Name dom]
getExternalBinds cont expr = map exprToName $ keepFirsts $ filter isApplicableName (expr ^? uniplateRef)
  where isApplicableName name@(getExprNameInfo -> Just nm) = inScopeForOriginal nm && notInScopeForExtracted nm
        isApplicableName _ = False

        getExprNameInfo :: ExtractBindingDomain dom => Ann' Expr dom -> Maybe GHC.Name
        getExprNameInfo expr = semanticsName =<< (listToMaybe $ expr ^? element & (exprName&element&simpleName &+& exprOperator&element&operatorName) 
                                                                                & semantics)

        -- | Creates the parameter value to pass the name (operators are passed in parentheses)
        exprToName :: Ann' Expr dom -> Ann' Name dom
        exprToName e | Just n <- e ^? element & exprName                               = n
                     | Just op <- e ^? element & exprOperator & element & operatorName = mkParenName op
        
        notInScopeForExtracted :: GHC.Name -> Bool
        notInScopeForExtracted n = notElem @[] n (semanticsScope (cont ^. semantics) ^? traversal & traversal)

        inScopeForOriginal :: GHC.Name -> Bool
        inScopeForOriginal n = elem @[] n (semanticsScope (expr ^. semantics) ^? traversal & traversal)

        keepFirsts (e:rest) = e : keepFirsts (filter (/= e) rest)
        keepFirsts [] = []

actualContainingExpr :: SourceInfo st => SrcSpan -> Simple Traversal (Ann ValueBind dom st) (Ann Expr dom st)
actualContainingExpr (RealSrcSpan rng) = element & accessRhs & element & accessExpr
  where accessRhs :: SourceInfo st => Simple Traversal (ValueBind dom st) (Ann Rhs dom st)
        accessRhs = valBindRhs &+& funBindMatches & annList & filtered (isInside rng) & element & matchRhs
        accessExpr :: SourceInfo st => Simple Traversal (Rhs dom st) (Ann Expr dom st)
        accessExpr = rhsExpr &+& rhsGuards & annList & filtered (isInside rng) & element & guardExpr

-- | Generates the expression that calls the local binding
generateCall :: String -> [Ann' Name dom] -> Ann' Expr dom
generateCall name args = foldl (\e a -> mkApp e (mkVar a)) (mkVar $ mkNormalName $ mkSimpleName name) args

-- | Generates the local binding for the selected expression
generateBind :: String -> [Ann' Pattern dom] -> Ann' Expr dom -> Ann' ValueBind dom
generateBind name [] e = mkSimpleBind (mkVarPat $ mkNormalName $ mkSimpleName name) (mkUnguardedRhs e) Nothing
generateBind name args e = mkFunctionBind [mkMatch (mkNormalMatchLhs (mkNormalName $ mkSimpleName name) args) (mkUnguardedRhs e) Nothing]

isValidBindingName :: String -> Bool
isValidBindingName = nameValid Variable

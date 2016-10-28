
{-# LANGUAGE ViewPatterns
           , ScopedTypeVariables
           , RankNTypes
           , FlexibleContexts
           , TypeApplications
           , ConstraintKinds
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Refactor.Predefined.ExtractBinding where

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

import Language.Haskell.Tools.Refactor

type ExtractBindingDomain dom = ( HasNameInfo dom, HasDefiningInfo dom, HasScopeInfo dom )

extractBinding' :: ExtractBindingDomain dom => RealSrcSpan -> String -> LocalRefactoring dom
extractBinding' sp name mod
  = if isValidBindingName name then extractBinding (nodesContaining sp) (nodesContaining sp) name mod
                               else refactError "The given name is not a valid for the extracted binding"

-- | Safely performs the transformation to introduce the local binding and replace the expression with the call.
-- Checks if the introduction of the name causes a name conflict.
extractBinding :: forall dom . ExtractBindingDomain dom 
               => Simple Traversal (Module dom) (ValueBind dom)
                   -> Simple Traversal (ValueBind dom) (Expr dom)
                   -> String -> LocalRefactoring dom
extractBinding selectDecl selectExpr name mod
  = let conflicting = any (isConflicting name) (mod ^? selectDecl & biplateRef :: [QualifiedName dom])
        exprRange = getRange $ head (mod ^? selectDecl & selectExpr)
        decl = last (mod ^? selectDecl)
        declRange = getRange $ last (mod ^? selectDecl)
     in if conflicting
           then refactError "The given name causes name conflict."
           else do (res, st) <- runStateT (selectDecl&selectExpr !~ extractThatBind name (head $ decl ^? actualContainingExpr exprRange) $ mod) Nothing
                   case st of Just def -> return $ evalState (selectDecl !~ addLocalBinding declRange exprRange def $ res) False
                              Nothing -> refactError "There is no applicable expression to extract."

-- | Decides if a new name defined to be the given string will conflict with the given AST element
isConflicting :: ExtractBindingDomain dom => String -> QualifiedName dom -> Bool
isConflicting name used
  = semanticsDefining used
      && (GHC.occNameString . GHC.getOccName <$> semanticsName used) == Just name

-- Replaces the selected expression with a call and generates the called binding.
extractThatBind :: ExtractBindingDomain dom 
                => String -> Expr dom -> Expr dom -> StateT (Maybe (ValueBind dom)) (LocalRefactor dom) (Expr dom)
extractThatBind name cont e 
  = do ret <- get
       if (isJust ret) then return e 
          else case e of
            Paren {} | hasParameter -> exprInner !~ doExtract name cont $ e
                     | otherwise    -> doExtract name cont (fromJust $ e ^? exprInner)
            Var {} -> lift $ refactError "The selected expression is too simple to be extracted."
            el | isParenLikeExpr el && hasParameter -> mkParen <$> doExtract name cont e
            el -> doExtract name cont e
  where hasParameter = not (null (getExternalBinds cont e))

-- | Adds a local binding to the 
addLocalBinding :: SrcSpan -> SrcSpan -> ValueBind dom -> ValueBind dom -> State Bool (ValueBind dom)
-- this uses the state monad to only add the local binding to the first selected element
addLocalBinding declRange exprRange local bind 
  = do done <- get
       if not done then do put True
                           return $ doAddBinding declRange exprRange local bind
                   else return bind 
  where
    doAddBinding declRng _ local sb@(SimpleBind {}) = valBindLocals .- insertLocalBind declRng local $ sb
    doAddBinding declRng (RealSrcSpan rng) local fb@(FunctionBind {}) 
      = funBindMatches & annList & filtered (isInside rng) & matchBinds 
          .- insertLocalBind declRng local $ fb

-- | Puts a value definition into a list of local binds
insertLocalBind :: SrcSpan -> ValueBind dom -> MaybeLocalBinds dom -> MaybeLocalBinds dom
insertLocalBind declRng toInsert locals 
  | isAnnNothing locals
  , RealSrcSpan rng <- declRng = -- creates the new where clause indented 2 spaces from the declaration
                                 mkLocalBinds (srcLocCol (realSrcSpanStart rng) + 2) [mkLocalValBind toInsert]
  | otherwise = annJust & localBinds .- insertWhere (mkLocalValBind toInsert) (const True) isNothing $ locals

-- | All expressions that are bound stronger than function application.
isParenLikeExpr :: Expr dom -> Bool
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
isParenLikeExpr (SpliceExpr {}) = True
isParenLikeExpr (QuasiQuoteExpr {}) = True
isParenLikeExpr _ = False

-- | Replaces the expression with the call and stores the binding of the call in its state
doExtract :: ExtractBindingDomain dom 
          => String -> Expr dom -> Expr dom -> StateT (Maybe (ValueBind dom)) (LocalRefactor dom) (Expr dom)
doExtract name cont e@(Lambda (AnnList bindings) inner)
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params ++ bindings) inner))
       return (generateCall name params)
doExtract name cont e 
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params) e))
       return (generateCall name params)

-- | Gets the values that have to be passed to the extracted definition
getExternalBinds :: ExtractBindingDomain dom => Expr dom -> Expr dom -> [Name dom]
getExternalBinds cont expr = map exprToName $ keepFirsts $ filter isApplicableName (expr ^? uniplateRef)
  where isApplicableName name@(getExprNameInfo -> Just nm) = inScopeForOriginal nm && notInScopeForExtracted nm
        isApplicableName _ = False

        getExprNameInfo :: ExtractBindingDomain dom => Expr dom -> Maybe GHC.Name
        getExprNameInfo expr = semanticsName =<< (listToMaybe $ expr ^? (exprName&simpleName &+& exprOperator&operatorName))

        -- | Creates the parameter value to pass the name (operators are passed in parentheses)
        exprToName :: Expr dom -> Name dom
        exprToName e | Just n <- e ^? exprName                     = n
                     | Just op <- e ^? exprOperator & operatorName = mkParenName op
        
        notInScopeForExtracted :: GHC.Name -> Bool
        notInScopeForExtracted n = not $ n `inScope` semanticsScope cont

        inScopeForOriginal :: GHC.Name -> Bool
        inScopeForOriginal n = n `inScope` semanticsScope expr

        keepFirsts (e:rest) = e : keepFirsts (filter (/= e) rest)
        keepFirsts [] = []

actualContainingExpr :: SrcSpan -> Simple Traversal (ValueBind dom) (Expr dom)
actualContainingExpr (RealSrcSpan rng) = accessRhs & accessExpr
  where accessRhs :: Simple Traversal (ValueBind dom) (Rhs dom)
        accessRhs = valBindRhs &+& funBindMatches & annList & filtered (isInside rng) & matchRhs
        accessExpr :: Simple Traversal (Rhs dom) (Expr dom)
        accessExpr = rhsExpr &+& rhsGuards & annList & filtered (isInside rng) & guardExpr

-- | Generates the expression that calls the local binding
generateCall :: String -> [Name dom] -> Expr dom
generateCall name args = foldl (\e a -> mkApp e (mkVar a)) (mkVar $ mkNormalName $ mkSimpleName name) args

-- | Generates the local binding for the selected expression
generateBind :: String -> [Pattern dom] -> Expr dom -> ValueBind dom
generateBind name [] e = mkSimpleBind (mkVarPat $ mkNormalName $ mkSimpleName name) (mkUnguardedRhs e) Nothing
generateBind name args e = mkFunctionBind [mkMatch (mkMatchLhs (mkNormalName $ mkSimpleName name) args) (mkUnguardedRhs e) Nothing]

isValidBindingName :: String -> Bool
isValidBindingName = nameValid Variable

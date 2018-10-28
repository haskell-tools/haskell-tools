{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtractBinding
  (extractBinding', extractBindingRefactoring) where

import qualified GHC
import Name (nameModule_maybe)
import qualified OccName as GHC (occNameString)
import OccName (HasOccName(..))
import PrelNames
import RdrName (isOrig_maybe)
import SrcLoc

import Control.Monad.State
import Control.Reference
import Data.Generics.Uniplate.Data ()
import Data.List (find, intersperse)
import Data.Maybe

import Language.Haskell.Tools.Refactor


extractBindingRefactoring :: RefactoringChoice
extractBindingRefactoring = NamingRefactoringIndent "ExtractBinding" (\loc s i -> localRefactoring (extractBinding' loc s i))

--tryItOut :: String -> String -> String -> String -> IO ()
--tryItOut mod sp indent name = tryRefactor (localRefactoring . flip extractBinding' name) mod indent sp

extractBinding' :: RealSrcSpan -> String -> Maybe String -> LocalRefactoring
extractBinding' sp name indent mod
  = if isNothing (isValidBindingName name)
      then extractBinding sp (read $ fromMaybe "4" indent) (nodesContaining sp) (nodesContaining sp) name mod
      else refactError $ "The given name is not a valid for the extracted binding: " ++ fromJust (isValidBindingName name)

-- | Safely performs the transformation to introduce the local binding and replace the expression with the call.
-- Checks if the introduction of the name causes a name conflict.
extractBinding :: RealSrcSpan -> Int -> Simple Traversal Module ValueBind
                   -> Simple Traversal ValueBind Expr
                   -> String -> LocalRefactoring
extractBinding sp indent selectDecl selectExpr name mod
  = let conflicting = filter (isConflicting name) ((take 1 $ reverse $ mod ^? selectDecl) ^? biplateRef :: [QualifiedName])
        exprRanges = map getRange (mod ^? selectDecl & selectExpr)
        decl = last (mod ^? selectDecl)
        declPats = decl ^? valBindPat &+& funBindMatches & annList & matchLhs
                                            & (matchLhsArgs & annList &+& matchLhsLhs &+& matchLhsRhs &+& matchLhsArgs & annList)
     in case exprRanges of
          (reverse -> exprRange:_) ->
            if | not (null conflicting)
               -> refactError $ "The given name causes name conflict with the definition(s) at: " ++ concat (intersperse "," (map (shortShowSpanWithFile . getRange) conflicting))
               | any (`containsRange` exprRange) $ map getRange declPats
               -> refactError "Extract binding cannot be applied to view pattern expressions."
               | otherwise
               -> case decl ^? actualContainingExpr exprRange of
                    expr:_ -> do (res, st) <- runStateT (selectDecl&selectExpr !~ extractThatBind sp name expr $ mod) Nothing
                                 case st of Just def -> return $ evalState (selectDecl !~ addLocalBinding exprRange indent def $ res) False
                                            Nothing -> refactError "There is no applicable expression to extract."
                    [] -> refactError $ "There is no applicable expression to extract."
          [] -> refactError "There is no applicable expression to extract."
  where RealSrcSpan sp1 `containsRange` RealSrcSpan sp2 = sp1 `containsSpan` sp2
        _ `containsRange` _ = False

-- | Decides if a new name defined to be the given string will conflict with the given AST element
isConflicting :: String -> QualifiedName -> Bool
isConflicting name used
  = semanticsDefining used
      && (GHC.occNameString . GHC.getOccName <$> semanticsName used) == Just name

-- Replaces the selected expression with a call and generates the called binding.
extractThatBind :: RealSrcSpan -> String -> Expr -> Expr -> StateT (Maybe ValueBind) LocalRefactor Expr
extractThatBind sp name cont e
  = do ret <- get -- being in a state monad to only apply the
       if (isJust ret) then return e
          else case e of
            -- only the expression inside the parameters should be extracted
            Paren {} | hasParameter -> exprInner !~ doExtract name cont $ e
                     | otherwise    -> doExtract name cont (fromJust $ e ^? exprInner)
            -- a single variable cannot be extracted (would lead to precedence problems)
            Var {} -> lift $ refactError "The selected expression is too simple to be extracted."
            -- extract operator sections
            InfixApp lhs op rhs
               | (lhs `outside` sp) && (sp `encloses` op) && (sp `encloses` rhs)
               -> do let params = getExternalBinds cont rhs ++ opName op
                     put (Just (generateBind name (map mkVarPat params) (mkRightSection op (parenIfInfix rhs))))
                     return (mkApp (generateCall name params) (parenIfInfix lhs))
               | (sp `encloses` lhs) && (sp `encloses` op) && (rhs `outside` sp)
               -> do let params = getExternalBinds cont lhs ++ opName op
                     put (Just (generateBind name (map mkVarPat params) (mkLeftSection (parenIfInfix lhs) op)))
                     return (mkApp (generateCall name params) (parenIfInfix rhs))
              where parenIfInfix e@(InfixApp {}) = mkParen e
                    parenIfInfix e = e
            -- extract parts of known associative infix operators
            InfixApp (InfixApp lhs lop mid) rop rhs -- correction for left-associative operators
              | (Just lName, Just rName) <- (semanticsName (lop ^. operatorName), semanticsName (rop ^. operatorName))
              , (lop `outside` sp) && (sp `encloses` mid) && (sp `encloses` rhs)
                  && lName == rName && isKnownCommutativeOp lName
              -> do let params = getExternalBinds cont mid ++ opName rop ++ getExternalBinds cont rhs
                    put (Just (generateBind name (map mkVarPat params) (mkInfixApp mid rop rhs)))
                    return (mkInfixApp lhs lop (generateCall name params))
            InfixApp lhs lop (InfixApp mid rop rhs) -- correction for right-associative operators
              | (Just lName, Just rName) <- (semanticsName (lop ^. operatorName), semanticsName (rop ^. operatorName))
              , (sp `encloses` lhs) && (sp `encloses` mid) && (rop `outside` sp)
                  && lName == rName && isKnownCommutativeOp lName
              -> do let params = getExternalBinds cont lhs ++ opName lop ++ getExternalBinds cont mid
                    put (Just (generateBind name (map mkVarPat params) (mkInfixApp lhs lop mid)))
                    return (mkInfixApp (generateCall name params) rop rhs)
            -- normal case
            el | isParenLikeExpr el && hasParameter -> mkParen <$> doExtract name cont e
               | otherwise -> doExtract name cont e
  where hasParameter = not (null (getExternalBinds cont e))
        -- True if the elem is completely inside the given source range
        sp `encloses` elem = case getRange elem of RealSrcSpan enc -> sp `containsSpan` enc
                                                   _               -> False
        -- True if the elem is completely outside the given range (no overlapping)
        elem `outside` sp = case getRange elem of RealSrcSpan out -> realSrcSpanStart sp > realSrcSpanEnd out
                                                                       || realSrcSpanEnd sp < realSrcSpanStart out
                                                  _ -> False
        opName op = case semanticsName (op ^. operatorName) of
                      Nothing -> []
                      Just n -> [mkUnqualName' n | not $ n `inScope` semanticsScope cont]
        isKnownCommutativeOp :: GHC.Name -> Bool
        isKnownCommutativeOp n = isJust $ find (maybe False (\(mn, occ) -> (nameModule_maybe n) == Just mn && occName n == occ) . isOrig_maybe) ops
          where ops = [plus_RDR, times_RDR, append_RDR, and_RDR, {- or_RDR, -} compose_RDR] -- somehow or is missing... WHY?

-- | Adds a local binding to the where clause of the enclosing binding
addLocalBinding :: SrcSpan -> Int -> ValueBind -> ValueBind -> State Bool ValueBind
-- this uses the state monad to only add the local binding to the first selected element
addLocalBinding exprRange indent local bind
  = do done <- get
       if not done then do put True
                           return $ indentBody $ doAddBinding exprRange local bind
                   else return bind
  where
    doAddBinding _ local sb@(SimpleBind {}) = valBindLocals .- insertLocalBind local $ sb
    doAddBinding (RealSrcSpan rng) local fb@(FunctionBind {})
      = funBindMatches & annList & filtered (isInside rng) & matchBinds
          .- insertLocalBind local $ fb
    doAddBinding _ _ _ = error "doAddBinding: invalid expression range"

    indentBody = (valBindRhs .- updIndent) . (funBindMatches & annList & matchLhs .- updIndent) . (funBindMatches & annList & matchRhs .- updIndent)

    updIndent :: SourceInfoTraversal elem => elem dom SrcTemplateStage -> elem dom SrcTemplateStage
    updIndent = setMinimalIndent indent

-- | Puts a value definition into a list of local binds
insertLocalBind :: ValueBind -> MaybeLocalBinds -> MaybeLocalBinds
insertLocalBind toInsert locals
  | isAnnNothing locals = mkLocalBinds [mkLocalValBind toInsert]
  | otherwise = annJust & localBinds .- insertWhere True (mkLocalValBind toInsert) (const True) isNothing $ locals

-- | All expressions that are bound stronger than function application.
isParenLikeExpr :: Expr -> Bool
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
doExtract :: String -> Expr -> Expr -> StateT (Maybe ValueBind) LocalRefactor Expr
doExtract name cont e@(Lambda (AnnList bindings) inner)
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params ++ bindings) inner))
       return (generateCall name params)
doExtract name cont e
  = do let params = getExternalBinds cont e
       put (Just (generateBind name (map mkVarPat params) e))
       return (generateCall name params)

-- | Gets the values that have to be passed to the extracted definition
getExternalBinds :: Expr -> Expr -> [Name]
getExternalBinds cont expr = map exprToName $ keepFirsts $ filter isApplicableName (expr ^? uniplateRef)
  where isApplicableName (getExprNameInfo -> Just nm) = inScopeForOriginal nm && notInScopeForExtracted nm
        isApplicableName _ = False

        getExprNameInfo :: Expr -> Maybe GHC.Name
        getExprNameInfo expr = semanticsName =<< (listToMaybe $ expr ^? (exprName&simpleName &+& exprOperator&operatorName))

        -- | Creates the parameter value to pass the name (operators are passed in parentheses)
        exprToName :: Expr -> Name
        exprToName e | Just n <- e ^? exprName                     = n
                     | Just op <- e ^? exprOperator & operatorName = mkParenName op
                     | otherwise                                   = error "exprToName: name not found"

        notInScopeForExtracted :: GHC.Name -> Bool
        notInScopeForExtracted n = not $ n `inScope` semanticsScope cont

        inScopeForOriginal :: GHC.Name -> Bool
        inScopeForOriginal n = n `inScope` semanticsScope expr

        keepFirsts (e:rest) = e : keepFirsts (filter (/= e) rest)
        keepFirsts [] = []

actualContainingExpr :: SrcSpan -> Simple Traversal ValueBind Expr
actualContainingExpr (RealSrcSpan rng) = accessRhs & accessExpr
  where accessRhs :: Simple Traversal ValueBind Rhs
        accessRhs = valBindRhs &+& funBindMatches & annList & filtered (rng `isInside`) & matchRhs
        accessExpr :: Simple Traversal Rhs Expr
        accessExpr = rhsExpr &+& rhsGuards & annList & filtered (rng `isInside`) & guardExpr
actualContainingExpr _ = error "actualContainingExpr: not a real range"

-- | Generates the expression that calls the local binding
generateCall :: String -> [Name] -> Expr
generateCall name args = foldl (\e a -> mkApp e (mkVar a)) (mkVar $ mkNormalName $ mkSimpleName name) args

-- | Generates the local binding for the selected expression
generateBind :: String -> [Pattern] -> Expr -> ValueBind
generateBind name [] e = mkSimpleBind (mkVarPat $ mkNormalName $ mkSimpleName name) (mkUnguardedRhs e) Nothing
generateBind name args e = mkFunctionBind [mkMatch (mkMatchLhs (mkNormalName $ mkSimpleName name) args) (mkUnguardedRhs e) Nothing]

isValidBindingName :: String -> Maybe String
isValidBindingName = nameValid Variable

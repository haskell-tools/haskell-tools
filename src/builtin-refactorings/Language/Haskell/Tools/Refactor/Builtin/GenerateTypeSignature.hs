{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables, TupleSections, TypeApplications, ViewPatterns #-}

module Language.Haskell.Tools.Refactor.Builtin.GenerateTypeSignature
  ( generateTypeSignature, generateTypeSignature', tryItOut
  , generateTypeSignatureRefactoring) where

import GHC hiding (Module)
import Id as GHC
import OccName as GHC (isSymOcc)
import Outputable as GHC (Outputable(..), showSDocUnsafe)
import TyCon as GHC (TyCon(..), isTupleTyCon)
import Type as GHC
import TysWiredIn as GHC (listTyCon, charTyCon)

import Control.Monad
import Control.Monad.State
import Control.Reference
import Data.Generics.Uniplate.Data (universeBi)
import Data.List
import Data.Maybe (Maybe(..), catMaybes)

import Language.Haskell.Tools.Refactor as AST

generateTypeSignatureRefactoring :: RefactoringChoice
generateTypeSignatureRefactoring = SelectionRefactoring "GenerateSignature" (localRefactoring . generateTypeSignature')

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . generateTypeSignature')

generateTypeSignature' :: RealSrcSpan -> LocalRefactoring
generateTypeSignature' sp = generateTypeSignature (nodesContaining sp) (nodesContaining sp) (getValBindInList sp)

-- | Perform the refactoring on either local or top-level definition
generateTypeSignature :: Simple Traversal Module DeclList
                                -- ^ Access for a top-level definition if it is the selected definition
                           -> Simple Traversal Module LocalBindList
                                -- ^ Access for a definition list if it contains the selected definition
                           -> (forall d . (BindingElem d) => AnnList d -> Maybe ValueBind)
                                -- ^ Selector for either local or top-level declaration in the definition list
                           -> LocalRefactoring
generateTypeSignature topLevelRef localRef vbAccess mod
  = let typeSigs = universeBi mod
        bindings = universeBi mod
        findTypeSigFor id = find (\ts -> any (id ==) $ map semanticsId (ts ^? tsName & annList & simpleName))
        bindsWithSigs = catMaybes $ concatMap (\b -> map (\n -> let id = semanticsId n in fmap (id,,b) (findTypeSigFor id typeSigs)) (b ^? bindingName)) bindings
        scopedSigs = hasScopedTypeSignatures mod
     in do (mod', done) <- flip runStateT False .
                             (topLevelRef !~ genTypeSig scopedSigs bindsWithSigs vbAccess
                                <=< localRef !~ genTypeSig scopedSigs bindsWithSigs vbAccess) $ mod
           if done
             then return mod'
             else refactError "No binding without type signature is found at the selection."


hasScopedTypeSignatures :: Module -> Bool
hasScopedTypeSignatures mod = "ScopedTypeVariables" `elem` (mod ^? filePragmas & annList & lpPragmas & annList & langExt :: [String])

genTypeSig :: (BindingElem d) => Bool -> [(GHC.Var, TypeSignature, ValueBind)] -> (AnnList d -> Maybe ValueBind)
                                   -> AnnList d -> StateT Bool LocalRefactor (AnnList d)
genTypeSig scopedSigs sigBinds vbAccess ls
  | Just vb <- vbAccess ls
  , not (typeSignatureAlreadyExist ls vb)
    = if isSimpleBinding vb
        then
          do let id = getBindingName vb
                 isTheBind (Just decl)
                   = isBinding decl && map semanticsId (decl ^? elementName) == map semanticsId (vb ^? bindingName)
                 isTheBind _ = False

             alreadyGenerated <- get
             if alreadyGenerated
               then return ls
               else do put True
                       -- checking for possible situations when we cannot generate signature because of
                       -- an implicitly passed value
                       let dangerousTypeVars = dangerousTVs scopedSigs sigBinds
                           myTvs = concatMap @[] (getExternalTVs . idType . semanticsId) (vb ^? bindingName)
                       if not $ null @[] $ myTvs `intersect` dangerousTypeVars
                         then refactError $ "Could not generate type signature: the type variable(s) "
                                              ++ concat (intersperse ", " $ map (showSDocUnsafe . ppr) (myTvs `intersect` dangerousTypeVars))
                                              ++ " cannot be captured. (Use ScopedTypeVariables and forall-ed type signatures)"
                         else do
                           typeSig <- lift $ generateTSFor (getName id) (idType id)
                           return $ insertWhere True (createTypeSig typeSig) (const True) isTheBind ls
        else refactError "Signature can only be generated for simple value bindings."
  | otherwise = return ls
  where isSimpleBinding vb = case vb of SimpleBind (AST.VarPat {}) _ _ -> True
                                        SimpleBind _ _ _ -> False
                                        _ -> True
        dangerousTVs scopedSigs sigBinds
          = let dangerousDecls = if scopedSigs then filter (\(_,ts,_) -> not $ isForalledTS ts) sigBinds else sigBinds
                dangerousNames = map (\(_,_,bn) -> bn ^? (valBindPats & biplateRef &+& bindingName)) dangerousDecls
             in concatMap (concatMap @[] (getExternalTVs . idType . semanticsId @QualifiedName)) dangerousNames

generateTSFor :: GHC.Name -> GHC.Type -> LocalRefactor TypeSignature
generateTSFor n t = mkTypeSignature (mkUnqualName' n) <$> generateTypeFor (-1) (dropForAlls t)

-- | Generates the source-level type for a GHC internal type
generateTypeFor :: Int -> GHC.Type -> LocalRefactor AST.Type
generateTypeFor prec t
  -- context
  | (break (not . isPredTy) -> (preds, other), rt) <- splitFunTys t
  , not (null preds)
  = do ctx <- case preds of [pred] -> mkContext <$> generateAssertionFor pred
                            _      -> mkContext <$> (mkTupleAssertion <$> mapM generateAssertionFor preds)
       wrapParen 0 <$> (mkCtxType ctx <$> generateTypeFor 0 (mkFunTys other rt))
  -- function
  | Just (at, rt) <- splitFunTy_maybe t
  = wrapParen 0 <$> (mkFunctionType <$> generateTypeFor 10 at <*> generateTypeFor 0 rt)
  -- type operator (we don't know the precedences, so always use parentheses)
  | (op, [at,rt]) <- splitAppTys t
  , Just tc <- tyConAppTyCon_maybe op
  , isSymOcc (getOccName (getName tc))
  = wrapParen 0 <$> (mkInfixTypeApp <$> generateTypeFor 10 at <*> referenceOperator (idName $ getTCId tc) <*> generateTypeFor 10 rt)
  -- tuple types
  | Just (tc, tas) <- splitTyConApp_maybe t
  , isTupleTyCon tc
  = mkTupleType <$> mapM (generateTypeFor (-1)) tas
  -- string type
  | Just (ls, [et]) <- splitTyConApp_maybe t
  , Just ch <- tyConAppTyCon_maybe et
  , listTyCon == ls
  , charTyCon == ch
  = return $ mkVarType (mkNormalName $ mkSimpleName "String")
  -- list types
  | Just (tc, [et]) <- splitTyConApp_maybe t
  , listTyCon == tc
  = mkListType <$> generateTypeFor (-1) et
  -- type application
  | Just (tf, ta) <- splitAppTy_maybe t
  = wrapParen 10 <$> (mkTypeApp <$> generateTypeFor 10 tf <*> generateTypeFor 11 ta)
  -- type constructor
  | Just tc <- tyConAppTyCon_maybe t
  = mkVarType <$> referenceName (idName $ getTCId tc)
  -- type variable
  | Just tv <- getTyVar_maybe t
  = mkVarType <$> referenceName (idName tv)
  -- forall type
  | (tvs@(_:_), t') <- splitForAllTys t
  = wrapParen (-1) <$> (mkForallType (map (mkTypeVar' . getName) tvs) <$> generateTypeFor 0 t')
  | otherwise = error ("Cannot represent type: " ++ showSDocUnsafe (ppr t))
  where wrapParen :: Int -> AST.Type -> AST.Type
        wrapParen prec' node = if prec' < prec then mkParenType node else node

        getTCId :: GHC.TyCon -> GHC.Id
        getTCId tc = GHC.mkVanillaGlobal (GHC.tyConName tc) (tyConKind tc)

        generateAssertionFor :: GHC.Type -> LocalRefactor Assertion
        generateAssertionFor t
          | Just (tc, types) <- splitTyConApp_maybe t
          = mkClassAssert <$> referenceName (idName $ getTCId tc) <*> mapM (generateTypeFor 0) types
          | otherwise = error "generateAssertionFor: type not supported yet."

-- | Check whether the definition already has a type signature
typeSignatureAlreadyExist :: (BindingElem d) => AnnList d -> ValueBind -> Bool
typeSignatureAlreadyExist ls vb =
  getBindingName vb `elem` (map semanticsId $ concatMap (^? elementName) (filter isTypeSig $ ls ^? annList))

getBindingName :: ValueBind -> GHC.Id
getBindingName vb = case nub $ map semanticsId $ vb ^? bindingName of
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"

-- * Checking for type variable constraints

getExternalTVs :: GHC.Type -> [GHC.Var]
getExternalTVs t
  | Just tv <- getTyVar_maybe t = [tv]
  | Just (op, arg) <- splitAppTy_maybe t = getExternalTVs op `union` getExternalTVs arg
  | Just (tv, t') <- splitForAllTy_maybe t = delete tv $ getExternalTVs t'
  | otherwise = []

isForalledTS :: TypeSignature -> Bool
isForalledTS ts = not $ null @[] $ ts ^? tsType & typeBounded & annList

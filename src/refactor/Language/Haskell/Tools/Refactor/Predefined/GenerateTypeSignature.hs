{-# LANGUAGE ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes 
           , TypeApplications
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature (generateTypeSignature, generateTypeSignature', GenerateSignatureDomain) where

import GHC hiding (Module)
import Type as GHC
import TyCon as GHC
import OccName as GHC
import Outputable as GHC
import TysWiredIn as GHC
import Id as GHC

import Data.List
import Data.Maybe
import Data.Data
import Data.Generics.Uniplate.Data
import Control.Monad
import Control.Monad.State
import Control.Reference

import Language.Haskell.Tools.Refactor as AST

type GenerateSignatureDomain dom = ( HasModuleInfo dom, HasIdInfo dom, HasImportInfo dom ) 

generateTypeSignature' :: GenerateSignatureDomain dom => RealSrcSpan -> LocalRefactoring dom
generateTypeSignature' sp = generateTypeSignature (nodesContaining sp) (nodesContaining sp) (getValBindInList sp) 

-- | Perform the refactoring on either local or top-level definition
generateTypeSignature :: GenerateSignatureDomain dom => Simple Traversal (Module dom) (DeclList dom) 
                                -- ^ Access for a top-level definition if it is the selected definition
                           -> Simple Traversal (Module dom) (LocalBindList dom) 
                                -- ^ Access for a definition list if it contains the selected definition
                           -> (forall d . (BindingElem d) => AnnList d dom -> Maybe (ValueBind dom)) 
                                -- ^ Selector for either local or top-level declaration in the definition list
                           -> LocalRefactoring dom
generateTypeSignature topLevelRef localRef vbAccess
  = flip evalStateT False .
     (topLevelRef !~ genTypeSig vbAccess
        <=< localRef !~ genTypeSig vbAccess)
  
genTypeSig :: (GenerateSignatureDomain dom, BindingElem d) => (AnnList d dom -> Maybe (ValueBind dom))  
                -> AnnList d dom -> StateT Bool (LocalRefactor dom) (AnnList d dom)
genTypeSig vbAccess ls 
  | Just vb <- vbAccess ls 
  , not (typeSignatureAlreadyExist ls vb)
    = do let id = getBindingName vb
             isTheBind (Just decl) 
               = isBinding decl && map semanticsId (decl ^? elementName) == map semanticsId (vb ^? bindingName)
             isTheBind _ = False
             
         alreadyGenerated <- get
         if alreadyGenerated 
           then return ls
           else do put True
                   typeSig <- lift $ generateTSFor (getName id) (idType id)
                   return $ insertWhere (createTypeSig typeSig) (const True) isTheBind ls
  | otherwise = return ls


generateTSFor :: GenerateSignatureDomain dom => GHC.Name -> GHC.Type -> LocalRefactor dom (TypeSignature dom)
generateTSFor n t = mkTypeSignature (mkUnqualName' n) <$> generateTypeFor (-1) (dropForAlls t)

-- | Generates the source-level type for a GHC internal type
generateTypeFor :: GenerateSignatureDomain dom => Int -> GHC.Type -> LocalRefactor dom (AST.Type dom) 
generateTypeFor prec t 
  -- context
  | (break (not . isPredTy) -> (preds, other), rt) <- splitFunTys t
  , not (null preds)
  = do ctx <- case preds of [pred] -> mkContextOne <$> generateAssertionFor pred
                            _ -> mkContextMulti <$> mapM generateAssertionFor preds
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
  where wrapParen :: Int -> AST.Type dom -> AST.Type dom
        wrapParen prec' node = if prec' < prec then mkParenType node else node

        getTCId :: GHC.TyCon -> GHC.Id
        getTCId tc = GHC.mkVanillaGlobal (GHC.tyConName tc) (tyConKind tc)

        generateAssertionFor :: GenerateSignatureDomain dom => GHC.Type -> LocalRefactor dom (Assertion dom)
        generateAssertionFor t 
          | Just (tc, types) <- splitTyConApp_maybe t
          = mkClassAssert <$> referenceName (idName $ getTCId tc) <*> mapM (generateTypeFor 0) types
        -- TODO: infix things
    
-- | Check whether the definition already has a type signature
typeSignatureAlreadyExist :: (GenerateSignatureDomain dom, BindingElem d) => AnnList d dom -> ValueBind dom -> Bool
typeSignatureAlreadyExist ls vb = 
  getBindingName vb `elem` (map semanticsId $ concatMap (^? elementName) (filter isTypeSig $ ls ^? annList))
  
getBindingName :: GenerateSignatureDomain dom => ValueBind dom -> GHC.Id
getBindingName vb = case nub $ map semanticsId $ vb ^? bindingName of 
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"

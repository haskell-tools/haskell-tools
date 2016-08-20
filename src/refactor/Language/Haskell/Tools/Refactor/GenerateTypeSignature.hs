{-# LANGUAGE ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes 
           , TypeApplications
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.GenerateTypeSignature (generateTypeSignature, generateTypeSignature', GenerateSignatureDomain) where

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
import Control.Reference hiding (element)
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.RefactorBase

type Ann' e dom = Ann e dom SrcTemplateStage
type AnnList' e dom = AnnList e dom SrcTemplateStage
type GenerateSignatureDomain dom = ( Domain dom, HasIdInfo (SemanticInfo' dom SameInfoNameCls), Eq (SemanticInfo' dom SameInfoNameCls)
                                   , SemanticInfo' dom SameInfoImportCls ~ ImportInfo Id ) 

generateTypeSignature' :: GenerateSignatureDomain dom => RealSrcSpan -> LocalRefactoring dom
generateTypeSignature' sp = generateTypeSignature (nodesContaining sp) (nodesContaining sp) (getValBindInList sp) 

-- | Perform the refactoring on either local or top-level definition
generateTypeSignature :: GenerateSignatureDomain dom => Simple Traversal (Ann' Module dom) (AnnList' Decl dom) 
                                -- ^ Access for a top-level definition if it is the selected definition
                           -> Simple Traversal (Ann' Module dom) (AnnList' LocalBind dom) 
                                -- ^ Access for a definition list if it contains the selected definition
                           -> (forall d . (Show (d dom SrcTemplateStage), Data (d dom SrcTemplateStage), Typeable d, BindingElem d) 
                                => AnnList' d dom -> Maybe (Ann' ValueBind dom)) 
                                -- ^ Selector for either local or top-level declaration in the definition list
                           -> LocalRefactoring dom
generateTypeSignature topLevelRef localRef vbAccess
  = flip evalStateT False .
     (topLevelRef !~ genTypeSig vbAccess
        <=< localRef !~ genTypeSig vbAccess)
  
genTypeSig :: (GenerateSignatureDomain dom, BindingElem d) => (AnnList' d dom -> Maybe (Ann' ValueBind dom))  
                -> AnnList' d dom -> StateT Bool (LocalRefactor dom) (AnnList' d dom)
genTypeSig vbAccess ls 
  | Just vb <- vbAccess ls 
  , not (typeSignatureAlreadyExist ls vb)
    = do let id = getBindingName vb
             isTheBind (Just ((^.element) -> decl)) 
               = isBinding decl && map semanticsId (decl ^? bindName) == map semanticsId (vb ^? bindingName)
             isTheBind _ = False
             
         alreadyGenerated <- get
         if alreadyGenerated 
           then return ls
           else do put True
                   typeSig <- lift $ generateTSFor (getName id) (idType id)
                   return $ insertWhere (wrapperAnn $ createTypeSig typeSig) (const True) isTheBind ls
  | otherwise = return ls


generateTSFor :: GenerateSignatureDomain dom => GHC.Name -> GHC.Type -> LocalRefactor dom (Ann' TypeSignature dom)
generateTSFor n t = mkTypeSignature (mkUnqualName' n) <$> generateTypeFor (-1) (dropForAlls t)

-- | Generates the source-level type for a GHC internal type
generateTypeFor :: GenerateSignatureDomain dom => Int -> GHC.Type -> LocalRefactor dom (Ann' AST.Type dom) 
generateTypeFor prec t 
  -- context
  | (break (not . isPredTy) -> (preds, other), rt) <- splitFunTys t
  , not (null preds)
  = do ctx <- case preds of [pred] -> mkContextOne <$> generateAssertionFor pred
                            _ -> mkContextMulti <$> mapM generateAssertionFor preds
       wrapParen 0 <$> (mkTyCtx ctx <$> generateTypeFor 0 (mkFunTys other rt))
  -- function
  | Just (at, rt) <- splitFunTy_maybe t
  = wrapParen 0 <$> (mkTyFun <$> generateTypeFor 10 at <*> generateTypeFor 0 rt)
  -- type operator (we don't know the precedences, so always use parentheses)
  | (op, [at,rt]) <- splitAppTys t
  , Just tc <- tyConAppTyCon_maybe op
  , isSymOcc (getOccName (getName tc))
  = wrapParen 0 <$> (mkTyInfix <$> generateTypeFor 10 at <*> referenceOperator (getTCId tc) <*> generateTypeFor 10 rt)
  -- tuple types
  | Just (tc, tas) <- splitTyConApp_maybe t
  , isTupleTyCon tc
  = mkTyTuple <$> mapM (generateTypeFor (-1)) tas
  -- string type
  | Just (ls, [et]) <- splitTyConApp_maybe t
  , Just ch <- tyConAppTyCon_maybe et
  , listTyCon == ls
  , charTyCon == ch
  = return $ mkTyVar (mkNormalName $ mkSimpleName "String")
  -- list types
  | Just (tc, [et]) <- splitTyConApp_maybe t
  , listTyCon == tc
  = mkTyList <$> generateTypeFor (-1) et
  -- type application
  | Just (tf, ta) <- splitAppTy_maybe t
  = wrapParen 10 <$> (mkTyApp <$> generateTypeFor 10 tf <*> generateTypeFor 11 ta)
  -- type constructor
  | Just tc <- tyConAppTyCon_maybe t
  = mkTyVar <$> referenceName (getTCId tc)
  -- type variable
  | Just tv <- getTyVar_maybe t
  = mkTyVar <$> referenceName tv
  -- forall type
  | (tvs@(_:_), t') <- splitForAllTys t
  = wrapParen (-1) <$> (mkTyForall (mkTypeVarList (map getName tvs)) <$> generateTypeFor 0 t')
  | otherwise = error ("Cannot represent type: " ++ showSDocUnsafe (ppr t))
  where wrapParen :: Int -> Ann' AST.Type dom -> Ann' AST.Type dom
        wrapParen prec' node = if prec' < prec then mkTyParen node else node

        getTCId :: GHC.TyCon -> GHC.Id
        getTCId tc = GHC.mkVanillaGlobal (GHC.tyConName tc) (tyConKind tc)

        generateAssertionFor :: GenerateSignatureDomain dom => GHC.Type -> LocalRefactor dom (Ann' AST.Assertion dom)
        generateAssertionFor t 
          | Just (tc, types) <- splitTyConApp_maybe t
          = mkClassAssert <$> referenceName (getTCId tc) <*> mapM (generateTypeFor 0) types
        -- TODO: infix things
    
-- | Check whether the definition already has a type signature
typeSignatureAlreadyExist :: (GenerateSignatureDomain dom, BindingElem d) => AnnList' d dom -> Ann' ValueBind dom -> Bool
typeSignatureAlreadyExist ls vb = 
  getBindingName vb `elem` (map semanticsId $ concatMap (^? bindName) (filter isTypeSig $ ls ^? annList&element))
  
getBindingName :: GenerateSignatureDomain dom => Ann' ValueBind dom -> GHC.Id
getBindingName vb = case map semanticsId $ nub $ vb ^? bindingName of 
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"

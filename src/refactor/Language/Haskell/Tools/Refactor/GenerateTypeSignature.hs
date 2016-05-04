{-# LANGUAGE ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes 
           #-}
module Language.Haskell.Tools.Refactor.GenerateTypeSignature (generateTypeSignature, generateTypeSignature') where

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

type RefactorId = Refactor GHC.Id
type STWithId = STWithNames GHC.Id

generateTypeSignature' :: RealSrcSpan -> Ann Module STWithId -> RefactoredModule GHC.Id
generateTypeSignature' sp = generateTypeSignature (nodesContaining sp) (nodesContaining sp) (getValBindInList sp) 

generateTypeSignature :: Simple Traversal (Ann Module STWithId) (AnnList Decl STWithId)
                           -> Simple Traversal (Ann Module STWithId) (AnnList LocalBind STWithId)
                           -> (forall d . (Show (d (NodeInfo STWithId SourceTemplate)), Data (d STWithId), Typeable d, BindingElem d) 
                                => AnnList d STWithId -> Maybe (Ann ValueBind STWithId))
                           -> Ann Module STWithId -> RefactoredModule GHC.Id
generateTypeSignature topLevelRef localRef vbAccess
  = flip evalStateT False .
     (topLevelRef !~ genTypeSig vbAccess
        <=< localRef !~ genTypeSig vbAccess)
  
genTypeSig :: (Show (d (NodeInfo (SemanticInfo Id) SourceTemplate)), BindingElem d) => (AnnList d STWithId -> Maybe (Ann ValueBind STWithId))  
                -> AnnList d STWithId -> StateT Bool RefactorId (AnnList d STWithId)
genTypeSig vbAccess ls 
  | Just vb <- vbAccess ls 
  , not (typeSignatureAlreadyExist ls vb)
    = do let id = getBindingName vb
             isTheBind (Just ((^.element) -> decl)) 
               = isBinding decl && (decl ^? bindName) == (vb ^? bindingName :: [GHC.Id])
             isTheBind _ = False
             
         alreadyGenerated <- get
         if alreadyGenerated 
           then return ls
           else do put True
                   typeSig <- lift $ generateTSFor (getName id) (idType id)
                   return $ insertWhere (wrapperAnn $ createTypeSig typeSig) (const True) isTheBind ls
  | otherwise = return ls

generateTSFor :: GHC.Name -> GHC.Type -> RefactorId (Ann TypeSignature STWithId)
generateTSFor n t = mkTypeSignature (mkUnqualName' n) <$> generateTypeFor (-1) (dropForAlls t)

generateTypeFor :: Int -> GHC.Type -> RefactorId (Ann AST.Type STWithId) 
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
  = return $ mkTyVar (mkUnqualName "String")
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
  = wrapParen (-1) <$> (mkTyForall (mkTypeVarList (map getName tvs)) noth <$> generateTypeFor 0 t')
  | otherwise = error ("Cannot represent type: " ++ showSDocUnsafe (ppr t))
  where wrapParen :: Int -> Ann AST.Type STWithId -> Ann AST.Type STWithId
        wrapParen prec' node = if prec' < prec then mkTyParen node else node

        getTCId :: GHC.TyCon -> GHC.Id
        getTCId tc = GHC.mkVanillaGlobal (GHC.tyConName tc) (tyConKind tc)

        generateAssertionFor :: GHC.Type -> RefactorId (Ann AST.Assertion STWithId)
        generateAssertionFor t 
          | Just (tc, types) <- splitTyConApp_maybe t
          = mkClassAssert <$> referenceName (getTCId tc) <*> mapM (generateTypeFor 0) types
        -- TODO: infix things
    
typeSignatureAlreadyExist :: forall d . BindingElem d => AnnList d STWithId -> Ann ValueBind STWithId -> Bool
typeSignatureAlreadyExist ls vb = 
  getBindingName vb `elem` concatMap (^? bindName) (filter isTypeSig $ ls ^? annList&element)
  
getBindingName :: Ann ValueBind STWithId -> GHC.Id
getBindingName vb = case nub $ vb ^? bindingName of 
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"

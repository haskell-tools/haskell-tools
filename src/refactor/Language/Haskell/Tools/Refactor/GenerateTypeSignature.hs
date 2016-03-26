{-# LANGUAGE ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes 
           #-}
module Language.Haskell.Tools.Refactor.GenerateTypeSignature where

import GHC as GHC hiding (Module)
import Type as GHC

import Data.List
import Data.Maybe
import Data.Data
import Control.Monad
import Control.Reference hiding (element)
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.AST.Gen.Modules
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST as AST

type STWithNames = NodeInfo SemanticInfo SourceTemplate

generateTypeSignature :: Simple Traversal (Ann Module STWithNames) (AnnList Decl STWithNames)
                           -> Simple Traversal (Ann Module STWithNames) (AnnList LocalBind STWithNames)
                           -> (forall d . (Data (d STWithNames), Typeable d) => AnnList d STWithNames -> Ann ValueBind STWithNames)
                           -> Ann Module STWithNames -> Ghc (Ann Module STWithNames)
generateTypeSignature topLevelRef localRef vbRef mod
  = topLevelRef !~ genTypeSig vbRef
      <=< localRef !~ genTypeSig vbRef 
            $ mod
  
genTypeSig :: BindingElem d => (AnnList d STWithNames -> Ann ValueBind STWithNames)  
                -> AnnList d STWithNames -> Ghc (AnnList d STWithNames)
genTypeSig vbAccess ls 
  | typeSignatureAlreadyExist ls vb = fail "Type signature already exists"
  | otherwise 
  = do let name = getBindingName vb
       Just (AnId id) <- lookupName name
       let typeSig = generateTSFor name (idType id)
       return $ insertWhere (wrapperAnn $ createTypeSig typeSig) (const True) isTheBind ls
  where vb = vbAccess ls
        isTheBind :: BindingElem d => Maybe (Ann d STWithNames) -> Bool
        isTheBind (Just ((^.element) -> decl)) 
          = isBinding decl && (decl ^? bindName) == (vb ^? bindingName :: [GHC.Name])
        isTheBind _ = False

generateTSFor :: GHC.Name -> GHC.Type -> Ann TypeSignature STWithNames 
generateTSFor n t = mkTypeSignature (mkUnqualName' n) (generateTypeFor t)

generateTypeFor :: GHC.Type -> Ann AST.Type STWithNames 
generateTypeFor t 
  -- TODO: parentheses where needed
  | Just (tf, ta) <- splitAppTy_maybe t
  = mkTyApp (generateTypeFor tf) (generateTypeFor ta)
  | Just (at, rt) <- splitFunTy_maybe t
  = mkTyFun (generateTypeFor at) (generateTypeFor rt)
  | Just tc <- tyConAppTyCon_maybe t
  = mkTypeVarType' (getName tc)
  | Just tv <- getTyVar_maybe t
  = mkTypeVarType' (getName tv)
  | (tvs@(_:_), t') <- splitForAllTys t
  = mkTyForall (mkTypeVarList (map getName tvs)) noth (generateTypeFor t')
       
typeSignatureAlreadyExist :: forall d . BindingElem d => AnnList d STWithNames -> Ann ValueBind STWithNames -> Bool
typeSignatureAlreadyExist ls vb = 
  getBindingName vb `elem` concatMap (^? bindName) (filter isTypeSig $ ls ^? annList&element)
  
getBindingName :: Ann ValueBind STWithNames -> GHC.Name
getBindingName vb = case nub $ vb ^? bindingName of 
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"

{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           #-}
-- | Utilities for transformations that work on both top-level and local definitions
module Language.Haskell.Tools.Refactor.BindingElem where

import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Rewrite
import SrcLoc (RealSrcSpan)

-- | A type class for handling definitions that can appear as both top-level and local definitions
class NamedElement d => BindingElem d where

  -- | Accesses a type signature definition in a local or top-level definition
  sigBind :: Simple Partial (Ann d dom SrcTemplateStage) (TypeSignature dom)

  -- | Accesses a value or function definition in a local or top-level definition
  valBind :: Simple Partial (Ann d dom SrcTemplateStage) (ValueBind dom)

  -- | Accesses a type signature definition in a local or top-level definition
  fixitySig :: Simple Partial (Ann d dom SrcTemplateStage) (FixitySignature dom)

  -- | Creates a new definition from a type signature
  createTypeSig :: TypeSignature dom -> Ann d dom SrcTemplateStage

  -- | Creates a new definition from a value or function definition
  createBinding :: ValueBind dom -> Ann d dom SrcTemplateStage

  -- | Creates a new fixity signature
  createFixitySig :: FixitySignature dom -> Ann d dom SrcTemplateStage

  -- | Checks if a given definition is a type signature
  isTypeSig :: Ann d dom SrcTemplateStage -> Bool
  
  -- | Checks if a given definition is a function or value binding
  isBinding :: Ann d dom SrcTemplateStage -> Bool

  -- | Checks if a given definition is a fixity signature
  isFixitySig :: Ann d dom SrcTemplateStage -> Bool
  
instance BindingElem UDecl where
  sigBind = declTypeSig
  valBind = declValBind
  fixitySig = declFixity
  createTypeSig = mkTypeSigDecl
  createBinding = mkValueBinding
  createFixitySig = mkFixityDecl
  isTypeSig TypeSigDecl {} = True
  isTypeSig _ = False
  isBinding ValueBinding {} = True
  isBinding _ = False
  isFixitySig FixityDecl {} = True
  isFixitySig _ = False

instance BindingElem ULocalBind where
  sigBind = localSig
  valBind = localVal
  fixitySig = localFixity
  createTypeSig = mkLocalTypeSig
  createBinding = mkLocalValBind
  createFixitySig = mkLocalFixity
  isTypeSig LocalTypeSig {} = True
  isTypeSig _ = False
  isBinding LocalValBind {} = True
  isBinding _ = False
  isFixitySig LocalFixity {} = True
  isFixitySig _ = False
     
getValBindInList :: (BindingElem d) => RealSrcSpan -> AnnListG d dom SrcTemplateStage -> Maybe (ValueBind dom)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"

valBindsInList :: BindingElem d => Simple Traversal (AnnListG d dom SrcTemplateStage) (ValueBind dom)
valBindsInList = annList & valBind

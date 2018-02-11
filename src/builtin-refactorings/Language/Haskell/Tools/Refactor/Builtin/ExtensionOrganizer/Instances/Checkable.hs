{-# LANGUAGE FlexibleInstances #-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.Checkable where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- | Global checks
instance Checkable Module where
  check = globalChkNamesForTypeEq

instance Checkable Decl where
  check = chkFlexibleInstancesDecl
      >=> chkDerivings
      >=> chkTypeFamiliesDecl
      >=> chkMultiParamTypeClassesDecl
      >=> chkConstraintKindsDecl
      >=> chkConstrainedClassMethodsDecl
      >=> chkTypeSynonymInstancesDecl

instance Checkable Pattern where
  check = chkBangPatterns
      >=> chkViewPatterns
      >=> chkUnboxedTuplesPat

instance Checkable Expr where
  check = chkTupleSections
      >=> chkUnboxedTuplesExpr
      >=> chkLambdaCase
      >=> chkRecursiveDoExpr
      >=> chkArrowsExpr
      >=> chkParallelListComp

instance Checkable Type where
  check = chkUnboxedTuplesType
      >=> chkExplicitForAllType

instance Checkable PatternField where
  check = chkRecordWildCardsPatField

instance Checkable FieldUpdate where
  check = chkRecordWildCardsFieldUpdate

instance Checkable PatternSynonym where
  check = chkPatternSynonymsSyn

instance Checkable PatternSignature where
  check = chkPatternSynonymsTypeSig

instance Checkable Literal where
  check = chkMagicHashLiteral
      >=> chkOverloadedStringsLiteral

instance Checkable NamePart where
  check = chkMagicHashNamePart
      >=> chkTemplateHaskellhNamePart

instance Checkable Kind where
  check = chkMagicHashKind
      >=> chkKindSignaturesKind

instance Checkable Splice where
  check = chkTemplateHaskellSplice

instance Checkable QuasiQuote where
  check = chkTemplateHaskellQuasiQuote

instance Checkable Bracket where
  check = chkTemplateHaskellBracket

instance Checkable FunDepList where
  check = chkFunDeps

instance Checkable ClassElement where
  check = chkDefaultSigs
      >=> chkTypeFamiliesClassElement

instance Checkable Stmt where
  check = chkRecursiveDoStmt

instance Checkable Cmd where
  check = chkArrowsCmd

instance Checkable InstBodyDecl where
  check = chkTypeFamiliesInstBodyDecl

instance Checkable IESpec where
  check = chkExplicitNamespacesIESpec

instance Checkable Operator where
  check = chkOperatorForTypeEq

instance Checkable GadtConDecl where
  check = chkGADTsGadtConDecl
      >=> chkExplicitForAllGadtConDecl

instance Checkable ConDecl where
  check = chkConDeclForExistentials
      >=> chkExplicitForAllConDecl

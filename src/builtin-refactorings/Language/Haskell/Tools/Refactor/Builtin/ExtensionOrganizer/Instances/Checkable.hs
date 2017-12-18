{-# LANGUAGE TypeSynonymInstances
            , FlexibleInstances
            #-}

module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.Checkable where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers

instance Checkable Decl where
  check = chkFlexibleInstances >=> chkDerivings

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

instance Checkable NamePart where
  check = chkMagicHashNamePart
      >=> chkTemplateHaskellhNamePart

instance Checkable Kind where
  check = chkMagicHashKind

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

instance Checkable Stmt where
  check = chkRecursiveDoStmt

instance Checkable Cmd where
  check = chkArrowsCmd

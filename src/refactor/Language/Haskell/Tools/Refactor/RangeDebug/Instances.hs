{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric, UndecidableInstances #-}
module Language.Haskell.Tools.Refactor.RangeDebug.Instances where

import GHC.Generics
import SrcLoc
import Language.Haskell.Tools.Refactor.RangeDebug

import Language.Haskell.Tools.AST.Instances
import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
instance (RangeDebug e, Show (e SrcSpan)) => RangeDebug (Ann e) where
  rangeDebug' i (Ann a e) = identLine i ++ shortShowSpan a ++ " " ++ take 20 (show e) ++ "..." ++ rangeDebug' (i+1) e
  
identLine :: Int -> String
identLine i = "\n" ++ replicate (i*2) ' '
  
instance (RangeDebug e, Show (e SrcSpan)) => RangeDebug (AnnList e) where
  rangeDebug' i (AnnList ls) = concatMap (rangeDebug' i) ls 
  
instance (RangeDebug e, Show (e SrcSpan)) => RangeDebug (AnnMaybe e) where
  rangeDebug' i (AnnMaybe (Just e)) = rangeDebug' i e
  rangeDebug' i (AnnMaybe Nothing) = ""

deriving instance Generic SrcSpan
  
-- Modules
instance RangeDebug Module
instance RangeDebug ModuleHead
instance RangeDebug ExportSpecList
instance RangeDebug ExportSpec
instance RangeDebug IESpec
instance RangeDebug SubSpec
instance RangeDebug ModulePragma
instance RangeDebug ImportDecl
instance RangeDebug ImportSpec
instance RangeDebug ImportQualified
instance RangeDebug ImportSource
instance RangeDebug ImportSafe
instance RangeDebug TypeNamespace
instance RangeDebug ImportRenaming

-- Declarations
instance RangeDebug Decl
instance RangeDebug ClassBody
instance RangeDebug GadtDeclList
instance RangeDebug ClassElement
instance RangeDebug DeclHead
instance RangeDebug InstBody
instance RangeDebug InstBodyDecl
instance RangeDebug GadtDecl
instance RangeDebug GadtField
instance RangeDebug FunDeps
instance RangeDebug FunDep
instance RangeDebug ConDecl
instance RangeDebug FieldDecl
instance RangeDebug Deriving
instance RangeDebug InstanceRule
instance RangeDebug InstanceHead
instance RangeDebug TypeEqn
instance RangeDebug KindConstraint
instance RangeDebug TyVar
instance RangeDebug Type
instance RangeDebug Kind
instance RangeDebug Context
instance RangeDebug Assertion
instance RangeDebug Expr
instance RangeDebug Stmt
instance RangeDebug CompStmt
instance RangeDebug FunBind
instance RangeDebug Pattern
instance RangeDebug PatternField
instance RangeDebug Splice
instance RangeDebug QQString
instance RangeDebug Match
instance RangeDebug Alt
instance RangeDebug Binds
instance RangeDebug Rhs
instance RangeDebug GuardedRhs
instance RangeDebug FieldUpdate
instance RangeDebug Bracket
instance RangeDebug TopLevelPragma
instance RangeDebug Rule
instance RangeDebug Annotation
instance RangeDebug MinimalFormula
instance RangeDebug ExprPragma
instance RangeDebug SourceRange
instance RangeDebug Number

-- Literal
instance RangeDebug Literal
instance RangeDebug Promoted

-- Base
instance RangeDebug Name
instance RangeDebug SimpleName
instance RangeDebug StringNode
instance RangeDebug DataOrNewtypeKeyword
instance RangeDebug DoKind
instance RangeDebug TypeKeyword
instance RangeDebug OverlapPragma
instance RangeDebug CallConv
instance RangeDebug ArrowAppl
instance RangeDebug Safety
instance RangeDebug Assoc
instance RangeDebug Precedence
instance RangeDebug PhaseControl
instance RangeDebug PhaseNumber
instance RangeDebug PhaseInvert
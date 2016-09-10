{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveGeneric
           , UndecidableInstances 
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Refactor.ASTDebug.Instances where

import Language.Haskell.Tools.Refactor.ASTDebug

import GHC.Generics
import Control.Reference

import Language.Haskell.Tools.AST

-- Annotations
instance {-# OVERLAPPING #-} (ASTDebug QualifiedName dom st) => ASTDebug (Ann QualifiedName) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (NameInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug Expr dom st) => ASTDebug (Ann Expr) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ExprInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug ImportDecl dom st) => ASTDebug (Ann ImportDecl) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ImportInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug Module dom st) => ASTDebug (Ann Module) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ModuleInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug FieldWildcard dom st) => ASTDebug (Ann FieldWildcard) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ImplicitFieldInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance ASTDebug e dom st => ASTDebug (Ann e) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= DefaultInfoType (getRange (a ^. sourceInfo)) $ astDebug' e

-- FIXME: WHY do I have to write it separately?
instance {-# OVERLAPPING #-} (ASTDebug ImportDecl dom st) => ASTDebug (AnnList ImportDecl) dom st where
  astDebug' (AnnList a ls) = [TreeNode "" (TreeDebugNode "*" (DefaultInfoType (getRange (a ^. sourceInfo))) (concatMap astDebug' ls))]

instance (ASTDebug e dom st) => ASTDebug (AnnList e) dom st where
  astDebug' (AnnList a ls) = [TreeNode "" (TreeDebugNode "*" (DefaultInfoType (getRange (a ^. sourceInfo))) (concatMap astDebug' ls))]
  
instance (ASTDebug e dom st) => ASTDebug (AnnMaybe e) dom st where
  astDebug' (AnnMaybe a e) = [TreeNode "" (TreeDebugNode "?" (DefaultInfoType (getRange (a ^. sourceInfo))) (maybe [] astDebug' e))]

-- Modules
instance (Domain dom, SourceInfo st) => ASTDebug Module dom st
instance (Domain dom, SourceInfo st) => ASTDebug ModuleHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug ExportSpecList dom st
instance (Domain dom, SourceInfo st) => ASTDebug ExportSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug IESpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug SubSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug ModulePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug FilePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportQualified dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportSource dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportSafe dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeNamespace dom st
instance (Domain dom, SourceInfo st) => ASTDebug ImportRenaming dom st

-- Declarations
instance (Domain dom, SourceInfo st) => ASTDebug Decl dom st
instance (Domain dom, SourceInfo st) => ASTDebug ClassBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug ClassElement dom st
instance (Domain dom, SourceInfo st) => ASTDebug DeclHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug InstBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug InstBodyDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug GadtConDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug GadtConType dom st
instance (Domain dom, SourceInfo st) => ASTDebug GadtField dom st
instance (Domain dom, SourceInfo st) => ASTDebug FieldWildcard dom st
instance (Domain dom, SourceInfo st) => ASTDebug FunDeps dom st
instance (Domain dom, SourceInfo st) => ASTDebug FunDep dom st
instance (Domain dom, SourceInfo st) => ASTDebug ConDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug FieldDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug Deriving dom st
instance (Domain dom, SourceInfo st) => ASTDebug InstanceRule dom st
instance (Domain dom, SourceInfo st) => ASTDebug InstanceHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeEqn dom st
instance (Domain dom, SourceInfo st) => ASTDebug KindConstraint dom st
instance (Domain dom, SourceInfo st) => ASTDebug TyVar dom st
instance (Domain dom, SourceInfo st) => ASTDebug Type dom st
instance (Domain dom, SourceInfo st) => ASTDebug Kind dom st
instance (Domain dom, SourceInfo st) => ASTDebug Context dom st
instance (Domain dom, SourceInfo st) => ASTDebug Assertion dom st
instance (Domain dom, SourceInfo st) => ASTDebug Expr dom st
instance (Domain dom, SourceInfo st) => ASTDebug (Stmt' Expr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug (Stmt' Cmd) dom st
instance (Domain dom, SourceInfo st) => ASTDebug CompStmt dom st
instance (Domain dom, SourceInfo st) => ASTDebug ValueBind dom st
instance (Domain dom, SourceInfo st) => ASTDebug Pattern dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatternField dom st
instance (Domain dom, SourceInfo st) => ASTDebug Splice dom st
instance (Domain dom, SourceInfo st) => ASTDebug QQString dom st
instance (Domain dom, SourceInfo st) => ASTDebug Match dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (Alt' expr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug Rhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug GuardedRhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug FieldUpdate dom st
instance (Domain dom, SourceInfo st) => ASTDebug Bracket dom st
instance (Domain dom, SourceInfo st) => ASTDebug TopLevelPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug Rule dom st
instance (Domain dom, SourceInfo st) => ASTDebug AnnotationSubject dom st
instance (Domain dom, SourceInfo st) => ASTDebug MinimalFormula dom st
instance (Domain dom, SourceInfo st) => ASTDebug ExprPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug SourceRange dom st
instance (Domain dom, SourceInfo st) => ASTDebug Number dom st
instance (Domain dom, SourceInfo st) => ASTDebug QuasiQuote dom st
instance (Domain dom, SourceInfo st) => ASTDebug RhsGuard dom st
instance (Domain dom, SourceInfo st) => ASTDebug LocalBind dom st
instance (Domain dom, SourceInfo st) => ASTDebug LocalBinds dom st
instance (Domain dom, SourceInfo st) => ASTDebug FixitySignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeSignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug ListCompBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug TupSecElem dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeFamily dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeFamilySpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug InjectivityAnn dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (CaseRhs' expr) dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (GuardedCaseRhs' expr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatternSynonym dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatSynRhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatSynLhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatSynWhere dom st
instance (Domain dom, SourceInfo st) => ASTDebug PatternTypeSignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug Role dom st
instance (Domain dom, SourceInfo st) => ASTDebug Cmd dom st
instance (Domain dom, SourceInfo st) => ASTDebug LanguageExtension dom st
instance (Domain dom, SourceInfo st) => ASTDebug MatchLhs dom st

-- Literal
instance (Domain dom, SourceInfo st) => ASTDebug Literal dom st
instance (Domain dom, SourceInfo st, ASTDebug k dom st, Generic (k dom st)) => ASTDebug (Promoted k) dom st

-- Base
instance (Domain dom, SourceInfo st) => ASTDebug Operator dom st
instance (Domain dom, SourceInfo st) => ASTDebug Name dom st
instance (Domain dom, SourceInfo st) => ASTDebug QualifiedName dom st
instance (Domain dom, SourceInfo st) => ASTDebug ModuleName dom st
instance (Domain dom, SourceInfo st) => ASTDebug UnqualName dom st
instance (Domain dom, SourceInfo st) => ASTDebug StringNode dom st
instance (Domain dom, SourceInfo st) => ASTDebug DataOrNewtypeKeyword dom st
instance (Domain dom, SourceInfo st) => ASTDebug DoKind dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeKeyword dom st
instance (Domain dom, SourceInfo st) => ASTDebug OverlapPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug CallConv dom st
instance (Domain dom, SourceInfo st) => ASTDebug ArrowAppl dom st
instance (Domain dom, SourceInfo st) => ASTDebug Safety dom st
instance (Domain dom, SourceInfo st) => ASTDebug ConlikeAnnot dom st
instance (Domain dom, SourceInfo st) => ASTDebug Assoc dom st
instance (Domain dom, SourceInfo st) => ASTDebug Precedence dom st
instance (Domain dom, SourceInfo st) => ASTDebug LineNumber dom st
instance (Domain dom, SourceInfo st) => ASTDebug PhaseControl dom st
instance (Domain dom, SourceInfo st) => ASTDebug PhaseNumber dom st
instance (Domain dom, SourceInfo st) => ASTDebug PhaseInvert dom st

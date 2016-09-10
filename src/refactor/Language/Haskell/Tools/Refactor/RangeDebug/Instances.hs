{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveGeneric
           , UndecidableInstances 
           #-}
module Language.Haskell.Tools.Refactor.RangeDebug.Instances where

import Language.Haskell.Tools.Refactor.RangeDebug

import GHC.Generics
import Control.Reference

import Language.Haskell.Tools.AST

-- Annotations
instance TreeDebug e dom st => TreeDebug (Ann e) dom st where
  treeDebug' i (Ann a e) = identLine i ++ show (a ^. sourceInfo) ++ " " ++ take 40 (show e) ++ "..." ++ treeDebug' (i+1) e
  
identLine :: Int -> String
identLine i = "\n" ++ replicate (i*2) ' '
  
instance TreeDebug e dom st => TreeDebug (AnnList e) dom st where
  treeDebug' i (AnnList a ls) = identLine i ++ show (a ^. sourceInfo) ++ " <*>" ++ concatMap (treeDebug' (i + 1)) ls 
  
instance TreeDebug e dom st => TreeDebug (AnnMaybe e) dom st where
  treeDebug' i (AnnMaybe a e) = identLine i ++ show (a ^. sourceInfo) ++ " <?>" ++ maybe "" (\e -> treeDebug' (i + 1) e) e
  
-- Modules
instance (SourceInfo st, Domain dom) => TreeDebug Module dom st
instance (SourceInfo st, Domain dom) => TreeDebug ModuleHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug ExportSpecList dom st
instance (SourceInfo st, Domain dom) => TreeDebug ExportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug IESpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug SubSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug ModulePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug FilePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportQualified dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportSource dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportSafe dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeNamespace dom st
instance (SourceInfo st, Domain dom) => TreeDebug ImportRenaming dom st

-- Declarations
instance (SourceInfo st, Domain dom) => TreeDebug Decl dom st
instance (SourceInfo st, Domain dom) => TreeDebug ClassBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug ClassElement dom st
instance (SourceInfo st, Domain dom) => TreeDebug DeclHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug InstBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug InstBodyDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug GadtConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug GadtConType dom st
instance (SourceInfo st, Domain dom) => TreeDebug GadtField dom st
instance (SourceInfo st, Domain dom) => TreeDebug FieldWildcard dom st
instance (SourceInfo st, Domain dom) => TreeDebug FunDeps dom st
instance (SourceInfo st, Domain dom) => TreeDebug FunDep dom st
instance (SourceInfo st, Domain dom) => TreeDebug ConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug FieldDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug Deriving dom st
instance (SourceInfo st, Domain dom) => TreeDebug InstanceRule dom st
instance (SourceInfo st, Domain dom) => TreeDebug InstanceHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeEqn dom st
instance (SourceInfo st, Domain dom) => TreeDebug KindConstraint dom st
instance (SourceInfo st, Domain dom) => TreeDebug TyVar dom st
instance (SourceInfo st, Domain dom) => TreeDebug Type dom st
instance (SourceInfo st, Domain dom) => TreeDebug Kind dom st
instance (SourceInfo st, Domain dom) => TreeDebug Context dom st
instance (SourceInfo st, Domain dom) => TreeDebug Assertion dom st
instance (SourceInfo st, Domain dom) => TreeDebug Expr dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (Stmt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug CompStmt dom st
instance (SourceInfo st, Domain dom) => TreeDebug ValueBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug Pattern dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatternField dom st
instance (SourceInfo st, Domain dom) => TreeDebug Splice dom st
instance (SourceInfo st, Domain dom) => TreeDebug QQString dom st
instance (SourceInfo st, Domain dom) => TreeDebug Match dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (Alt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug Rhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug GuardedRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug FieldUpdate dom st
instance (SourceInfo st, Domain dom) => TreeDebug Bracket dom st
instance (SourceInfo st, Domain dom) => TreeDebug TopLevelPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug Rule dom st
instance (SourceInfo st, Domain dom) => TreeDebug AnnotationSubject dom st
instance (SourceInfo st, Domain dom) => TreeDebug MinimalFormula dom st
instance (SourceInfo st, Domain dom) => TreeDebug ExprPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug SourceRange dom st
instance (SourceInfo st, Domain dom) => TreeDebug Number dom st
instance (SourceInfo st, Domain dom) => TreeDebug QuasiQuote dom st
instance (SourceInfo st, Domain dom) => TreeDebug RhsGuard dom st
instance (SourceInfo st, Domain dom) => TreeDebug LocalBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug LocalBinds dom st
instance (SourceInfo st, Domain dom) => TreeDebug FixitySignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug ListCompBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug TupSecElem dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeFamily dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeFamilySpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug InjectivityAnn dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (CaseRhs' expr) dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (GuardedCaseRhs' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatternSynonym dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatSynRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatSynLhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatSynWhere dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatternTypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug Role dom st
instance (SourceInfo st, Domain dom) => TreeDebug Cmd dom st
instance (SourceInfo st, Domain dom) => TreeDebug LanguageExtension dom st
instance (SourceInfo st, Domain dom) => TreeDebug MatchLhs dom st

-- Literal
instance (SourceInfo st, Domain dom) => TreeDebug Literal dom st
instance (SourceInfo st, Domain dom, TreeDebug k dom st, Generic (k dom st)) => TreeDebug (Promoted k) dom st

-- Base
instance (SourceInfo st, Domain dom) => TreeDebug Operator dom st
instance (SourceInfo st, Domain dom) => TreeDebug Name dom st
instance (SourceInfo st, Domain dom) => TreeDebug QualifiedName dom st
instance (SourceInfo st, Domain dom) => TreeDebug ModuleName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UnqualName dom st
instance (SourceInfo st, Domain dom) => TreeDebug StringNode dom st
instance (SourceInfo st, Domain dom) => TreeDebug DataOrNewtypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug DoKind dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug OverlapPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug CallConv dom st
instance (SourceInfo st, Domain dom) => TreeDebug ArrowAppl dom st
instance (SourceInfo st, Domain dom) => TreeDebug Safety dom st
instance (SourceInfo st, Domain dom) => TreeDebug ConlikeAnnot dom st
instance (SourceInfo st, Domain dom) => TreeDebug Assoc dom st
instance (SourceInfo st, Domain dom) => TreeDebug Precedence dom st
instance (SourceInfo st, Domain dom) => TreeDebug LineNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseControl dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseInvert dom st
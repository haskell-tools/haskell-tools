-- | Generic instance for Haskell AST representation
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric #-}
module Language.Haskell.Tools.AST.Instances.Generic where

import GHC.Generics

import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.TH
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Stmts
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (Ann e dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (AnnMaybe e dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (AnnList e dom stage)

-- Modules
deriving instance (Domain dom, SourceInfo stage) => Generic (Module dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExportSpecList dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (IESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (SubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Generic (Decl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (DeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (InstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (InstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (GadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (GadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (GadtField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Deriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (InstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (InstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (KindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Type dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Kind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Context dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Assertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Expr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (Stmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (CompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Pattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Splice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Match dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (Alt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Rhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (GuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Bracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Rule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (AnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (MinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (SourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (RhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (InjectivityAnn dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (CaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (GuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Role dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Cmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (MatchLhs dom stage)


-- Literal
deriving instance (Domain dom, SourceInfo stage) => Generic (Literal dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (k dom stage)) => Generic (Promoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Generic (Operator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Name dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UnqualName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (StringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (DataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (DoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (OverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (CallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Safety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseInvert dom stage)
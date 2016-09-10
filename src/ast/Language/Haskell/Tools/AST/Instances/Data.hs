-- | Data instances for Haskell AST (used for generics)
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveDataTypeable #-}
module Language.Haskell.Tools.AST.Instances.Data where

import Data.Data

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
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (Ann e dom stage)
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (AnnMaybe e dom stage)
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (AnnList e dom stage)

-- Modules
deriving instance (Domain dom, SourceInfo stage) => Data (Module dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExportSpecList dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (IESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (SubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Data (Decl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (DeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (InstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (InstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (GadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (GadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (GadtField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Deriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (InstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (InstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (KindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Type dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Kind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Context dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Assertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (Stmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (CompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Pattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Splice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Match dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (Alt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Rhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (GuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Bracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Rule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (AnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (MinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (SourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (RhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (InjectivityAnn dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (CaseRhs' expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage))=> Data (GuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Role dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Cmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (MatchLhs dom stage)

-- Literal
deriving instance (Domain dom, SourceInfo stage) => Data (Literal dom stage)
deriving instance (DomainWith k dom, SourceInfo stage, Typeable k, Data (k dom stage)) => Data (Promoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Data (Operator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Name dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UnqualName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (StringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (DataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (DoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (OverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (CallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Safety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseInvert dom stage)
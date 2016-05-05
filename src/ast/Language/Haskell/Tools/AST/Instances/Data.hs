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
deriving instance (Typeable a, Data a, Typeable e, Data (e a)) => Data (Ann e a)
deriving instance (Typeable a, Data a, Typeable e, Data (e a)) => Data (AnnMaybe e a)
deriving instance (Typeable a, Data a, Typeable e, Data (e a)) => Data (AnnList e a)

-- Modules
deriving instance Data a => Data (Module a)
deriving instance Data a => Data (ModuleHead a)
deriving instance Data a => Data (ExportSpecList a)
deriving instance Data a => Data (ExportSpec a)
deriving instance Data a => Data (IESpec a)
deriving instance Data a => Data (SubSpec a)
deriving instance Data a => Data (ModulePragma a)
deriving instance Data a => Data (ImportDecl a)
deriving instance Data a => Data (ImportSpec a)
deriving instance Data a => Data (ImportQualified a)
deriving instance Data a => Data (ImportSource a)
deriving instance Data a => Data (ImportSafe a)
deriving instance Data a => Data (TypeNamespace a)
deriving instance Data a => Data (ImportRenaming a)

-- Declarations
deriving instance Data a => Data (Decl a)
deriving instance Data a => Data (ClassBody a)
deriving instance Data a => Data (GadtDeclList a)
deriving instance Data a => Data (ClassElement a)
deriving instance Data a => Data (DeclHead a)
deriving instance Data a => Data (InstBody a)
deriving instance Data a => Data (InstBodyDecl a)
deriving instance Data a => Data (GadtDecl a)
deriving instance Data a => Data (GadtField a)
deriving instance Data a => Data (FunDeps a)
deriving instance Data a => Data (FunDep a)
deriving instance Data a => Data (ConDecl a)
deriving instance Data a => Data (FieldDecl a)
deriving instance Data a => Data (Deriving a)
deriving instance Data a => Data (InstanceRule a)
deriving instance Data a => Data (InstanceHead a)
deriving instance Data a => Data (TypeEqn a)
deriving instance Data a => Data (KindConstraint a)
deriving instance Data a => Data (TyVar a)
deriving instance Data a => Data (Type a)
deriving instance Data a => Data (Kind a)
deriving instance Data a => Data (Context a)
deriving instance Data a => Data (Assertion a)
deriving instance Data a => Data (Expr a)
deriving instance (Data a, Typeable expr, Data (expr a)) => Data (Stmt' expr a)
deriving instance Data a => Data (CompStmt a)
deriving instance Data a => Data (ValueBind a)
deriving instance Data a => Data (Pattern a)
deriving instance Data a => Data (PatternField a)
deriving instance Data a => Data (Splice a)
deriving instance Data a => Data (QQString a)
deriving instance Data a => Data (Match a)
deriving instance (Data a, Typeable expr, Data (expr a)) => Data (Alt' expr a)
deriving instance Data a => Data (Rhs a)
deriving instance Data a => Data (GuardedRhs a)
deriving instance Data a => Data (FieldUpdate a)
deriving instance Data a => Data (Bracket a)
deriving instance Data a => Data (TopLevelPragma a)
deriving instance Data a => Data (Rule a)
deriving instance Data a => Data (Annotation a)
deriving instance Data a => Data (MinimalFormula a)
deriving instance Data a => Data (ExprPragma a)
deriving instance Data a => Data (SourceRange a)
deriving instance Data a => Data (Number a)
deriving instance Data a => Data (QuasiQuote a)
deriving instance Data a => Data (RhsGuard a)
deriving instance Data a => Data (LocalBind a)
deriving instance Data a => Data (LocalBinds a)
deriving instance Data a => Data (FixitySignature a)
deriving instance Data a => Data (TypeSignature a)
deriving instance Data a => Data (ListCompBody a)
deriving instance Data a => Data (TupSecElem a)
deriving instance Data a => Data (TypeFamily a)
deriving instance (Data a, Typeable expr, Data (expr a)) => Data (CaseRhs' expr a)
deriving instance (Data a, Typeable expr, Data (expr a))=> Data (GuardedCaseRhs' expr a)
deriving instance Data a => Data (PatternSynonym a)
deriving instance Data a => Data (PatSynRhs a)
deriving instance Data a => Data (PatSynWhere a)
deriving instance Data a => Data (PatternTypeSignature a)
deriving instance Data a => Data (Role a)
deriving instance Data a => Data (Cmd a)
deriving instance Data a => Data (CmdStmt a)
deriving instance Data a => Data (LanguageExtension a)

-- Literal
deriving instance Data a => Data (Literal a)
deriving instance Data a => Data (Promoted a)

-- Base
deriving instance Data a => Data (Operator a)
deriving instance Data a => Data (Name a)
deriving instance Data a => Data (SimpleName a)
deriving instance Data a => Data (UnqualName a)
deriving instance Data a => Data (StringNode a)
deriving instance Data a => Data (DataOrNewtypeKeyword a)
deriving instance Data a => Data (DoKind a)
deriving instance Data a => Data (TypeKeyword a)
deriving instance Data a => Data (OverlapPragma a)
deriving instance Data a => Data (CallConv a)
deriving instance Data a => Data (ArrowAppl a)
deriving instance Data a => Data (Safety a)
deriving instance Data a => Data (Assoc a)
deriving instance Data a => Data (Precedence a)
deriving instance Data a => Data (PhaseControl a)
deriving instance Data a => Data (PhaseNumber a)
deriving instance Data a => Data (PhaseInvert a)
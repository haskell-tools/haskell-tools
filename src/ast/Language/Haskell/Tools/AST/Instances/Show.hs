-- | Show instance for Haskell AST representation ignoring source and semantic information
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Language.Haskell.Tools.AST.Instances.Show where

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
instance (Show (e a)) => Show (Ann e a) where
  show (Ann _ e) = show e

instance (Show (e a)) => Show (AnnMaybe e a) where
  show (AnnMaybe _ e) = show e
  
instance (Show (e a)) => Show (AnnList e a) where
  show (AnnList _ e) = show e

-- Modules
deriving instance Show (Module a)
deriving instance Show (ModuleHead a)
deriving instance Show (ExportSpecList a)
deriving instance Show (ExportSpec a)
deriving instance Show (IESpec a)
deriving instance Show (SubSpec a)
deriving instance Show (ModulePragma a)
deriving instance Show (FilePragma a)
deriving instance Show (ImportDecl a)
deriving instance Show (ImportSpec a)
deriving instance Show (ImportQualified a)
deriving instance Show (ImportSource a)
deriving instance Show (ImportSafe a)
deriving instance Show (TypeNamespace a)
deriving instance Show (ImportRenaming a)

-- Declarations
deriving instance Show (Decl a)
deriving instance Show (ClassBody a)
deriving instance Show (ClassElement a)
deriving instance Show (DeclHead a)
deriving instance Show (InstBody a)
deriving instance Show (InstBodyDecl a)
deriving instance Show (GadtConDecl a)
deriving instance Show (GadtConType a)
deriving instance Show (GadtField a)
deriving instance Show (FunDeps a)
deriving instance Show (FunDep a)
deriving instance Show (ConDecl a)
deriving instance Show (FieldDecl a)
deriving instance Show (Deriving a)
deriving instance Show (InstanceRule a)
deriving instance Show (InstanceHead a)
deriving instance Show (TypeEqn a)
deriving instance Show (KindConstraint a)
deriving instance Show (TyVar a)
deriving instance Show (Type a)
deriving instance Show (Kind a)
deriving instance Show (Context a)
deriving instance Show (Assertion a)
deriving instance Show (Expr a)
deriving instance Show (expr a) => Show (Stmt' expr a)
deriving instance Show (CompStmt a)
deriving instance Show (ValueBind a)
deriving instance Show (Pattern a)
deriving instance Show (PatternField a)
deriving instance Show (Splice a)
deriving instance Show (QQString a)
deriving instance Show (Match a)
deriving instance Show (expr a) => Show (Alt' expr a)
deriving instance Show (Rhs a)
deriving instance Show (GuardedRhs a)
deriving instance Show (FieldUpdate a)
deriving instance Show (Bracket a)
deriving instance Show (TopLevelPragma a)
deriving instance Show (Rule a)
deriving instance Show (Annotation a)
deriving instance Show (MinimalFormula a)
deriving instance Show (ExprPragma a)
deriving instance Show (SourceRange a)
deriving instance Show (Number a)
deriving instance Show (QuasiQuote a)
deriving instance Show (RhsGuard a)
deriving instance Show (LocalBind a)
deriving instance Show (LocalBinds a)
deriving instance Show (FixitySignature a)
deriving instance Show (TypeSignature a)
deriving instance Show (ListCompBody a)
deriving instance Show (TupSecElem a)
deriving instance Show (TypeFamily a)
deriving instance Show (TypeFamilySpec a)
deriving instance Show (InjectivityAnn a)
deriving instance Show (expr a) => Show (CaseRhs' expr a)
deriving instance Show (expr a) => Show (GuardedCaseRhs' expr a)
deriving instance Show (PatternSynonym a)
deriving instance Show (PatSynRhs a)
deriving instance Show (PatSynLhs a)
deriving instance Show (PatSynWhere a)
deriving instance Show (PatternTypeSignature a)
deriving instance Show (Role a)
deriving instance Show (Cmd a)
deriving instance Show (LanguageExtension a)
deriving instance Show (MatchLhs a)


-- Literal
deriving instance Show (Literal a)
deriving instance Show (k a) => Show (Promoted k a)

-- Base
deriving instance Show (Operator a)
deriving instance Show (Name a)
deriving instance Show (SimpleName a)
deriving instance Show (UnqualName a)
deriving instance Show (StringNode a)
deriving instance Show (DataOrNewtypeKeyword a)
deriving instance Show (DoKind a)
deriving instance Show (TypeKeyword a)
deriving instance Show (OverlapPragma a)
deriving instance Show (CallConv a)
deriving instance Show (ArrowAppl a)
deriving instance Show (Safety a)
deriving instance Show (ConlikeAnnot a)
deriving instance Show (Assoc a)
deriving instance Show (Precedence a)
deriving instance Show (LineNumber a)
deriving instance Show (PhaseControl a)
deriving instance Show (PhaseNumber a)
deriving instance Show (PhaseInvert a)
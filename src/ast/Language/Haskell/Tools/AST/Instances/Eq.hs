-- | Equality check of AST nodes that ignore the source and semantic information.
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Language.Haskell.Tools.AST.Instances.Eq where

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
instance (Eq (e a)) => Eq (Ann e a) where
  Ann _ e1 == Ann _ e2 = e1 == e2

instance (Eq (e a)) => Eq (AnnMaybe e a) where
  AnnMaybe _ e1 == AnnMaybe _ e2 = e1 == e2

instance (Eq (e a)) => Eq (AnnList e a) where
  AnnList _ e1 == AnnList _ e2 = e1 == e2

-- Modules
deriving instance Eq (Module a)
deriving instance Eq (ModuleHead a)
deriving instance Eq (ExportSpecList a)
deriving instance Eq (ExportSpec a)
deriving instance Eq (IESpec a)
deriving instance Eq (SubSpec a)
deriving instance Eq (ModulePragma a)
deriving instance Eq (ImportDecl a)
deriving instance Eq (ImportSpec a)
deriving instance Eq (ImportQualified a)
deriving instance Eq (ImportSource a)
deriving instance Eq (ImportSafe a)
deriving instance Eq (TypeNamespace a)
deriving instance Eq (ImportRenaming a)

-- Declarations
deriving instance Eq (Decl a)
deriving instance Eq (ClassBody a)
deriving instance Eq (GadtDeclList a)
deriving instance Eq (ClassElement a)
deriving instance Eq (DeclHead a)
deriving instance Eq (InstBody a)
deriving instance Eq (InstBodyDecl a)
deriving instance Eq (GadtDecl a)
deriving instance Eq (GadtField a)
deriving instance Eq (FunDeps a)
deriving instance Eq (FunDep a)
deriving instance Eq (ConDecl a)
deriving instance Eq (FieldDecl a)
deriving instance Eq (Deriving a)
deriving instance Eq (InstanceRule a)
deriving instance Eq (InstanceHead a)
deriving instance Eq (TypeEqn a)
deriving instance Eq (KindConstraint a)
deriving instance Eq (TyVar a)
deriving instance Eq (Type a)
deriving instance Eq (Kind a)
deriving instance Eq (Context a)
deriving instance Eq (Assertion a)
deriving instance Eq (Expr a)
deriving instance Eq (expr a) => Eq (Stmt' expr a)
deriving instance Eq (CompStmt a)
deriving instance Eq (ValueBind a)
deriving instance Eq (Pattern a)
deriving instance Eq (PatternField a)
deriving instance Eq (Splice a)
deriving instance Eq (QQString a)
deriving instance Eq (Match a)
deriving instance Eq (expr a) => Eq (Alt' expr a)
deriving instance Eq (Rhs a)
deriving instance Eq (GuardedRhs a)
deriving instance Eq (FieldUpdate a)
deriving instance Eq (Bracket a)
deriving instance Eq (TopLevelPragma a)
deriving instance Eq (Rule a)
deriving instance Eq (Annotation a)
deriving instance Eq (MinimalFormula a)
deriving instance Eq (ExprPragma a)
deriving instance Eq (SourceRange a)
deriving instance Eq (Number a)
deriving instance Eq (QuasiQuote a)
deriving instance Eq (RhsGuard a)
deriving instance Eq (LocalBind a)
deriving instance Eq (LocalBinds a)
deriving instance Eq (FixitySignature a)
deriving instance Eq (TypeSignature a)
deriving instance Eq (ListCompBody a)
deriving instance Eq (TupSecElem a)
deriving instance Eq (TypeFamily a)
deriving instance Eq (expr a) => Eq (CaseRhs' expr a)
deriving instance Eq (expr a) => Eq (GuardedCaseRhs' expr a)
deriving instance Eq (PatternSynonym a)
deriving instance Eq (PatSynRhs a)
deriving instance Eq (PatSynWhere a)
deriving instance Eq (PatternTypeSignature a)
deriving instance Eq (Role a)
deriving instance Eq (Cmd a)
deriving instance Eq (CmdStmt a)
deriving instance Eq (LanguageExtension a)

-- Literal
deriving instance Eq (Literal a)
deriving instance Eq (Promoted a)

-- Base
deriving instance Eq (Name a)
deriving instance Eq (SimpleName a)
deriving instance Eq (StringNode a)
deriving instance Eq (DataOrNewtypeKeyword a)
deriving instance Eq (DoKind a)
deriving instance Eq (TypeKeyword a)
deriving instance Eq (OverlapPragma a)
deriving instance Eq (CallConv a)
deriving instance Eq (ArrowAppl a)
deriving instance Eq (Safety a)
deriving instance Eq (Assoc a)
deriving instance Eq (Precedence a)
deriving instance Eq (PhaseControl a)
deriving instance Eq (PhaseNumber a)
deriving instance Eq (PhaseInvert a)
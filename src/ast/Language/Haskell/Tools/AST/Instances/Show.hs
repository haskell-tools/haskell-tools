{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Language.Haskell.Tools.AST.Instances.Show where

import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
deriving instance (Show a, Show (e a)) => Show (Ann e a)
deriving instance (Show a, Show (e a)) => Show (AnnMaybe e a)
deriving instance (Show a, Show (e a)) => Show (AnnList e a)

instance (Functor elem) => Functor (Ann elem) where
  fmap f ann = ann { annotation = f (annotation ann), element = fmap f (element ann) }
instance (Functor elem) => Functor (AnnList elem) where
  fmap f annList = annList { fromAnnList = fmap (fmap f) (fromAnnList annList) }
instance (Functor elem) => Functor (AnnMaybe elem) where
  fmap f annMaybe = annMaybe { fromAnnMaybe = fmap (fmap f) (fromAnnMaybe annMaybe) }

-- Modules
deriving instance Show a => Show (Module a)
deriving instance Show a => Show (ModuleHead a)
deriving instance Show a => Show (ExportSpecList a)
deriving instance Show a => Show (ExportSpec a)
deriving instance Show a => Show (IESpec a)
deriving instance Show a => Show (SubSpec a)
deriving instance Show a => Show (ModulePragma a)
deriving instance Show a => Show (ImportDecl a)
deriving instance Show a => Show (ImportSpec a)
deriving instance Show a => Show (ImportQualified a)
deriving instance Show a => Show (ImportSource a)
deriving instance Show a => Show (ImportSafe a)
deriving instance Show a => Show (TypeNamespace a)
deriving instance Show a => Show (ImportRenaming a)

-- Declarations
deriving instance Show a => Show (Decl a)
deriving instance Show a => Show (ClassBody a)
deriving instance Show a => Show (GadtDeclList a)
deriving instance Show a => Show (ClassElement a)
deriving instance Show a => Show (DeclHead a)
deriving instance Show a => Show (InstBody a)
deriving instance Show a => Show (InstBodyDecl a)
deriving instance Show a => Show (GadtDecl a)
deriving instance Show a => Show (GadtField a)
deriving instance Show a => Show (FunDeps a)
deriving instance Show a => Show (FunDep a)
deriving instance Show a => Show (ConDecl a)
deriving instance Show a => Show (FieldDecl a)
deriving instance Show a => Show (Deriving a)
deriving instance Show a => Show (InstanceRule a)
deriving instance Show a => Show (InstanceHead a)
deriving instance Show a => Show (TypeEqn a)
deriving instance Show a => Show (KindConstraint a)
deriving instance Show a => Show (TyVar a)
deriving instance Show a => Show (Type a)
deriving instance Show a => Show (Kind a)
deriving instance Show a => Show (Context a)
deriving instance Show a => Show (Assertion a)
deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Stmt a)
deriving instance Show a => Show (CompStmt a)
deriving instance Show a => Show (FunBind a)
deriving instance Show a => Show (Pattern a)
deriving instance Show a => Show (PatternField a)
deriving instance Show a => Show (Splice a)
deriving instance Show a => Show (QQString a)
deriving instance Show a => Show (Match a)
deriving instance Show a => Show (Alt a)
deriving instance Show a => Show (Binds a)
deriving instance Show a => Show (Rhs a)
deriving instance Show a => Show (GuardedRhs a)
deriving instance Show a => Show (FieldUpdate a)
deriving instance Show a => Show (Bracket a)
deriving instance Show a => Show (TopLevelPragma a)
deriving instance Show a => Show (Rule a)
deriving instance Show a => Show (Annotation a)
deriving instance Show a => Show (MinimalFormula a)
deriving instance Show a => Show (ExprPragma a)
deriving instance Show a => Show (SourceRange a)
deriving instance Show a => Show (Number a)
deriving instance Show a => Show (QuasiQuote a)

-- Literal
deriving instance Show a => Show (Literal a)
deriving instance Show a => Show (Promoted a)

-- Base
deriving instance Show a => Show (Name a)
deriving instance Show a => Show (SimpleName a)
deriving instance Show a => Show (StringNode a)
deriving instance Show a => Show (DataOrNewtypeKeyword a)
deriving instance Show a => Show (DoKind a)
deriving instance Show a => Show (TypeKeyword a)
deriving instance Show a => Show (OverlapPragma a)
deriving instance Show a => Show (CallConv a)
deriving instance Show a => Show (ArrowAppl a)
deriving instance Show a => Show (Safety a)
deriving instance Show a => Show (Assoc a)
deriving instance Show a => Show (Precedence a)
deriving instance Show a => Show (PhaseControl a)
deriving instance Show a => Show (PhaseNumber a)
deriving instance Show a => Show (PhaseInvert a)
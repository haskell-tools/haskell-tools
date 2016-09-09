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
instance (Eq (e dom stage)) => Eq (Ann e dom stage) where
  Ann _ e1 == Ann _ e2 = e1 == e2

instance (Eq (e dom stage)) => Eq (AnnMaybe e dom stage) where
  AnnMaybe _ e1 == AnnMaybe _ e2 = e1 == e2

instance (Eq (e dom stage)) => Eq (AnnList e dom stage) where
  AnnList _ e1 == AnnList _ e2 = e1 == e2

-- Modules
deriving instance Eq (Module dom stage)
deriving instance Eq (ModuleHead dom stage)
deriving instance Eq (ExportSpecList dom stage)
deriving instance Eq (ExportSpec dom stage)
deriving instance Eq (IESpec dom stage)
deriving instance Eq (SubSpec dom stage)
deriving instance Eq (ModulePragma dom stage)
deriving instance Eq (FilePragma dom stage)
deriving instance Eq (ImportDecl dom stage)
deriving instance Eq (ImportSpec dom stage)
deriving instance Eq (ImportQualified dom stage)
deriving instance Eq (ImportSource dom stage)
deriving instance Eq (ImportSafe dom stage)
deriving instance Eq (TypeNamespace dom stage)
deriving instance Eq (ImportRenaming dom stage)

-- Declarations
deriving instance Eq (Decl dom stage)
deriving instance Eq (ClassBody dom stage)
deriving instance Eq (ClassElement dom stage)
deriving instance Eq (DeclHead dom stage)
deriving instance Eq (InstBody dom stage)
deriving instance Eq (InstBodyDecl dom stage)
deriving instance Eq (GadtConDecl dom stage)
deriving instance Eq (GadtConType dom stage)
deriving instance Eq (GadtField dom stage)
deriving instance Eq (FieldWildcard dom stage)
deriving instance Eq (FunDeps dom stage)
deriving instance Eq (FunDep dom stage)
deriving instance Eq (ConDecl dom stage)
deriving instance Eq (FieldDecl dom stage)
deriving instance Eq (Deriving dom stage)
deriving instance Eq (InstanceRule dom stage)
deriving instance Eq (InstanceHead dom stage)
deriving instance Eq (TypeEqn dom stage)
deriving instance Eq (KindConstraint dom stage)
deriving instance Eq (TyVar dom stage)
deriving instance Eq (Type dom stage)
deriving instance Eq (Kind dom stage)
deriving instance Eq (Context dom stage)
deriving instance Eq (Assertion dom stage)
deriving instance Eq (Expr dom stage)
deriving instance Eq (expr dom stage) => Eq (Stmt' expr dom stage)
deriving instance Eq (CompStmt dom stage)
deriving instance Eq (ValueBind dom stage)
deriving instance Eq (Pattern dom stage)
deriving instance Eq (PatternField dom stage)
deriving instance Eq (Splice dom stage)
deriving instance Eq (QQString dom stage)
deriving instance Eq (Match dom stage)
deriving instance Eq (expr dom stage) => Eq (Alt' expr dom stage)
deriving instance Eq (Rhs dom stage)
deriving instance Eq (GuardedRhs dom stage)
deriving instance Eq (FieldUpdate dom stage)
deriving instance Eq (Bracket dom stage)
deriving instance Eq (TopLevelPragma dom stage)
deriving instance Eq (Rule dom stage)
deriving instance Eq (AnnotationSubject dom stage)
deriving instance Eq (MinimalFormula dom stage)
deriving instance Eq (ExprPragma dom stage)
deriving instance Eq (SourceRange dom stage)
deriving instance Eq (Number dom stage)
deriving instance Eq (QuasiQuote dom stage)
deriving instance Eq (RhsGuard dom stage)
deriving instance Eq (LocalBind dom stage)
deriving instance Eq (LocalBinds dom stage)
deriving instance Eq (FixitySignature dom stage)
deriving instance Eq (TypeSignature dom stage)
deriving instance Eq (ListCompBody dom stage)
deriving instance Eq (TupSecElem dom stage)
deriving instance Eq (TypeFamily dom stage)
deriving instance Eq (TypeFamilySpec dom stage)
deriving instance Eq (InjectivityAnn dom stage)
deriving instance Eq (expr dom stage) => Eq (CaseRhs' expr dom stage)
deriving instance Eq (expr dom stage) => Eq (GuardedCaseRhs' expr dom stage)
deriving instance Eq (PatternSynonym dom stage)
deriving instance Eq (PatSynRhs dom stage)
deriving instance Eq (PatSynLhs dom stage)
deriving instance Eq (PatSynWhere dom stage)
deriving instance Eq (PatternTypeSignature dom stage)
deriving instance Eq (Role dom stage)
deriving instance Eq (Cmd dom stage)
deriving instance Eq (LanguageExtension dom stage)
deriving instance Eq (MatchLhs dom stage)

-- Literal
deriving instance Eq (Literal dom stage)
deriving instance Eq (k dom stage) => Eq (Promoted k dom stage)

-- Base
deriving instance Eq (Operator dom stage)
deriving instance Eq (Name dom stage)
deriving instance Eq (QualifiedName dom stage)
deriving instance Eq (ModuleName dom stage)
deriving instance Eq (UnqualName dom stage)
deriving instance Eq (StringNode dom stage)
deriving instance Eq (DataOrNewtypeKeyword dom stage)
deriving instance Eq (DoKind dom stage)
deriving instance Eq (TypeKeyword dom stage)
deriving instance Eq (OverlapPragma dom stage)
deriving instance Eq (CallConv dom stage)
deriving instance Eq (ArrowAppl dom stage)
deriving instance Eq (Safety dom stage)
deriving instance Eq (ConlikeAnnot dom stage)
deriving instance Eq (Assoc dom stage)
deriving instance Eq (Precedence dom stage)
deriving instance Eq (LineNumber dom stage)
deriving instance Eq (PhaseControl dom stage)
deriving instance Eq (PhaseNumber dom stage)
deriving instance Eq (PhaseInvert dom stage)
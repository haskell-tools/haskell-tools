{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveGeneric
           , UndecidableInstances 
           #-}
module Language.Haskell.Tools.Refactor.ASTDebug.Instances where

import Language.Haskell.Tools.Refactor.ASTDebug

import GHC.Generics
import Control.Reference

import Language.Haskell.Tools.AST

-- Annotations
instance (ASTDebug e a, Show (e a)) => ASTDebug (Ann e) a where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= a $ astDebug' e
  
instance (ASTDebug e a, Show (e a)) => ASTDebug (AnnList e) a where
  astDebug' (AnnList a ls) = [TreeNode "" (TreeDebugNode "*" a (concatMap astDebug' ls))]
  
instance (ASTDebug e a, Show (e a)) => ASTDebug (AnnMaybe e) a where
  astDebug' (AnnMaybe a e) = [TreeNode "" (TreeDebugNode "?" a (maybe [] astDebug' e))]
  
-- Modules
instance (Generic a, Show a) => ASTDebug Module a
instance (Generic a, Show a) => ASTDebug ModuleHead a
instance (Generic a, Show a) => ASTDebug ExportSpecList a
instance (Generic a, Show a) => ASTDebug ExportSpec a
instance (Generic a, Show a) => ASTDebug IESpec a
instance (Generic a, Show a) => ASTDebug SubSpec a
instance (Generic a, Show a) => ASTDebug ModulePragma a
instance (Generic a, Show a) => ASTDebug ImportDecl a
instance (Generic a, Show a) => ASTDebug ImportSpec a
instance (Generic a, Show a) => ASTDebug ImportQualified a
instance (Generic a, Show a) => ASTDebug ImportSource a
instance (Generic a, Show a) => ASTDebug ImportSafe a
instance (Generic a, Show a) => ASTDebug TypeNamespace a
instance (Generic a, Show a) => ASTDebug ImportRenaming a

-- Declarations
instance (Generic a, Show a) => ASTDebug Decl a
instance (Generic a, Show a) => ASTDebug ClassBody a
instance (Generic a, Show a) => ASTDebug ClassElement a
instance (Generic a, Show a) => ASTDebug DeclHead a
instance (Generic a, Show a) => ASTDebug InstBody a
instance (Generic a, Show a) => ASTDebug InstBodyDecl a
instance (Generic a, Show a) => ASTDebug GadtConDecl a
instance (Generic a, Show a) => ASTDebug GadtConType a
instance (Generic a, Show a) => ASTDebug GadtField a
instance (Generic a, Show a) => ASTDebug FunDeps a
instance (Generic a, Show a) => ASTDebug FunDep a
instance (Generic a, Show a) => ASTDebug ConDecl a
instance (Generic a, Show a) => ASTDebug FieldDecl a
instance (Generic a, Show a) => ASTDebug Deriving a
instance (Generic a, Show a) => ASTDebug InstanceRule a
instance (Generic a, Show a) => ASTDebug InstanceHead a
instance (Generic a, Show a) => ASTDebug TypeEqn a
instance (Generic a, Show a) => ASTDebug KindConstraint a
instance (Generic a, Show a) => ASTDebug TyVar a
instance (Generic a, Show a) => ASTDebug Type a
instance (Generic a, Show a) => ASTDebug Kind a
instance (Generic a, Show a) => ASTDebug Context a
instance (Generic a, Show a) => ASTDebug Assertion a
instance (Generic a, Show a) => ASTDebug Expr a
instance (Generic a, Show a, ASTDebug expr a, Show (expr a), Generic (expr a)) => ASTDebug (Stmt' expr) a
instance (Generic a, Show a) => ASTDebug CompStmt a
instance (Generic a, Show a) => ASTDebug ValueBind a
instance (Generic a, Show a) => ASTDebug Pattern a
instance (Generic a, Show a) => ASTDebug PatternField a
instance (Generic a, Show a) => ASTDebug Splice a
instance (Generic a, Show a) => ASTDebug QQString a
instance (Generic a, Show a) => ASTDebug Match a
instance (Generic a, Show a, ASTDebug expr a, Show (expr a), Generic (expr a)) => ASTDebug (Alt' expr) a
instance (Generic a, Show a) => ASTDebug Rhs a
instance (Generic a, Show a) => ASTDebug GuardedRhs a
instance (Generic a, Show a) => ASTDebug FieldUpdate a
instance (Generic a, Show a) => ASTDebug Bracket a
instance (Generic a, Show a) => ASTDebug TopLevelPragma a
instance (Generic a, Show a) => ASTDebug Rule a
instance (Generic a, Show a) => ASTDebug Annotation a
instance (Generic a, Show a) => ASTDebug MinimalFormula a
instance (Generic a, Show a) => ASTDebug ExprPragma a
instance (Generic a, Show a) => ASTDebug SourceRange a
instance (Generic a, Show a) => ASTDebug Number a
instance (Generic a, Show a) => ASTDebug QuasiQuote a
instance (Generic a, Show a) => ASTDebug RhsGuard a
instance (Generic a, Show a) => ASTDebug LocalBind a
instance (Generic a, Show a) => ASTDebug LocalBinds a
instance (Generic a, Show a) => ASTDebug FixitySignature a
instance (Generic a, Show a) => ASTDebug TypeSignature a
instance (Generic a, Show a) => ASTDebug ListCompBody a
instance (Generic a, Show a) => ASTDebug TupSecElem a
instance (Generic a, Show a) => ASTDebug TypeFamily a
instance (Generic a, Show a) => ASTDebug TypeFamilySpec a
instance (Generic a, Show a) => ASTDebug InjectivityAnn a
instance (Generic a, Show a, ASTDebug expr a, Show (expr a), Generic (expr a)) => ASTDebug (CaseRhs' expr) a
instance (Generic a, Show a, ASTDebug expr a, Show (expr a), Generic (expr a)) => ASTDebug (GuardedCaseRhs' expr) a
instance (Generic a, Show a) => ASTDebug PatternSynonym a
instance (Generic a, Show a) => ASTDebug PatSynRhs a
instance (Generic a, Show a) => ASTDebug PatSynLhs a
instance (Generic a, Show a) => ASTDebug PatSynWhere a
instance (Generic a, Show a) => ASTDebug PatternTypeSignature a
instance (Generic a, Show a) => ASTDebug Role a
instance (Generic a, Show a) => ASTDebug Cmd a
instance (Generic a, Show a) => ASTDebug LanguageExtension a
instance (Generic a, Show a) => ASTDebug MatchLhs a

-- Literal
instance (Generic a, Show a) => ASTDebug Literal a
instance (Generic a, Show a, ASTDebug k a, Show (k a), Generic (k a)) => ASTDebug (Promoted k) a

-- Base
instance (Generic a, Show a) => ASTDebug Operator a
instance (Generic a, Show a) => ASTDebug Name a
instance (Generic a, Show a) => ASTDebug SimpleName a
instance (Generic a, Show a) => ASTDebug UnqualName a
instance (Generic a, Show a) => ASTDebug StringNode a
instance (Generic a, Show a) => ASTDebug DataOrNewtypeKeyword a
instance (Generic a, Show a) => ASTDebug DoKind a
instance (Generic a, Show a) => ASTDebug TypeKeyword a
instance (Generic a, Show a) => ASTDebug OverlapPragma a
instance (Generic a, Show a) => ASTDebug CallConv a
instance (Generic a, Show a) => ASTDebug ArrowAppl a
instance (Generic a, Show a) => ASTDebug Safety a
instance (Generic a, Show a) => ASTDebug Assoc a
instance (Generic a, Show a) => ASTDebug Precedence a
instance (Generic a, Show a) => ASTDebug PhaseControl a
instance (Generic a, Show a) => ASTDebug PhaseNumber a
instance (Generic a, Show a) => ASTDebug PhaseInvert a
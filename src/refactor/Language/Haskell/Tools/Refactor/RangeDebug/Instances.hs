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

import Language.Haskell.Tools.AST

-- Annotations
instance (TreeDebug e a, Show (e a)) => TreeDebug (Ann e) a where
  treeDebug' f i (Ann a e) = identLine i ++ f a ++ " " ++ take 40 (show e) ++ "..." ++ treeDebug' f (i+1) e
  
identLine :: Int -> String
identLine i = "\n" ++ replicate (i*2) ' '
  
instance (TreeDebug e a, Show (e a)) => TreeDebug (AnnList e) a where
  treeDebug' f i (AnnList a ls) = identLine i ++ f a ++ " <*>" ++ concatMap (treeDebug' f (i + 1)) ls 
  
instance (TreeDebug e a, Show (e a)) => TreeDebug (AnnMaybe e) a where
  treeDebug' f i (AnnMaybe a e) = identLine i ++ f a ++ " <?>" ++ maybe "" (\e -> treeDebug' f (i + 1) e) e
  
-- Modules
instance (Generic a, Show a) => TreeDebug Module a
instance (Generic a, Show a) => TreeDebug ModuleHead a
instance (Generic a, Show a) => TreeDebug ExportSpecList a
instance (Generic a, Show a) => TreeDebug ExportSpec a
instance (Generic a, Show a) => TreeDebug IESpec a
instance (Generic a, Show a) => TreeDebug SubSpec a
instance (Generic a, Show a) => TreeDebug ModulePragma a
instance (Generic a, Show a) => TreeDebug ImportDecl a
instance (Generic a, Show a) => TreeDebug ImportSpec a
instance (Generic a, Show a) => TreeDebug ImportQualified a
instance (Generic a, Show a) => TreeDebug ImportSource a
instance (Generic a, Show a) => TreeDebug ImportSafe a
instance (Generic a, Show a) => TreeDebug TypeNamespace a
instance (Generic a, Show a) => TreeDebug ImportRenaming a

-- Declarations
instance (Generic a, Show a) => TreeDebug Decl a
instance (Generic a, Show a) => TreeDebug ClassBody a
instance (Generic a, Show a) => TreeDebug GadtDeclList a
instance (Generic a, Show a) => TreeDebug ClassElement a
instance (Generic a, Show a) => TreeDebug DeclHead a
instance (Generic a, Show a) => TreeDebug InstBody a
instance (Generic a, Show a) => TreeDebug InstBodyDecl a
instance (Generic a, Show a) => TreeDebug GadtDecl a
instance (Generic a, Show a) => TreeDebug GadtField a
instance (Generic a, Show a) => TreeDebug FunDeps a
instance (Generic a, Show a) => TreeDebug FunDep a
instance (Generic a, Show a) => TreeDebug ConDecl a
instance (Generic a, Show a) => TreeDebug FieldDecl a
instance (Generic a, Show a) => TreeDebug Deriving a
instance (Generic a, Show a) => TreeDebug InstanceRule a
instance (Generic a, Show a) => TreeDebug InstanceHead a
instance (Generic a, Show a) => TreeDebug TypeEqn a
instance (Generic a, Show a) => TreeDebug KindConstraint a
instance (Generic a, Show a) => TreeDebug TyVar a
instance (Generic a, Show a) => TreeDebug Type a
instance (Generic a, Show a) => TreeDebug Kind a
instance (Generic a, Show a) => TreeDebug Context a
instance (Generic a, Show a) => TreeDebug Assertion a
instance (Generic a, Show a) => TreeDebug Expr a
instance (Generic a, Show a, TreeDebug expr a, Show (expr a), Generic (expr a)) => TreeDebug (Stmt' expr) a
instance (Generic a, Show a) => TreeDebug CompStmt a
instance (Generic a, Show a) => TreeDebug ValueBind a
instance (Generic a, Show a) => TreeDebug Pattern a
instance (Generic a, Show a) => TreeDebug PatternField a
instance (Generic a, Show a) => TreeDebug Splice a
instance (Generic a, Show a) => TreeDebug QQString a
instance (Generic a, Show a) => TreeDebug Match a
instance (Generic a, Show a, TreeDebug expr a, Show (expr a), Generic (expr a)) => TreeDebug (Alt' expr) a
instance (Generic a, Show a) => TreeDebug Rhs a
instance (Generic a, Show a) => TreeDebug GuardedRhs a
instance (Generic a, Show a) => TreeDebug FieldUpdate a
instance (Generic a, Show a) => TreeDebug Bracket a
instance (Generic a, Show a) => TreeDebug TopLevelPragma a
instance (Generic a, Show a) => TreeDebug Rule a
instance (Generic a, Show a) => TreeDebug Annotation a
instance (Generic a, Show a) => TreeDebug MinimalFormula a
instance (Generic a, Show a) => TreeDebug ExprPragma a
instance (Generic a, Show a) => TreeDebug SourceRange a
instance (Generic a, Show a) => TreeDebug Number a
instance (Generic a, Show a) => TreeDebug QuasiQuote a
instance (Generic a, Show a) => TreeDebug RhsGuard a
instance (Generic a, Show a) => TreeDebug LocalBind a
instance (Generic a, Show a) => TreeDebug LocalBinds a
instance (Generic a, Show a) => TreeDebug FixitySignature a
instance (Generic a, Show a) => TreeDebug TypeSignature a
instance (Generic a, Show a) => TreeDebug ListCompBody a
instance (Generic a, Show a) => TreeDebug TupSecElem a
instance (Generic a, Show a) => TreeDebug TypeFamily a
instance (Generic a, Show a, TreeDebug expr a, Show (expr a), Generic (expr a)) => TreeDebug (CaseRhs' expr) a
instance (Generic a, Show a, TreeDebug expr a, Show (expr a), Generic (expr a)) => TreeDebug (GuardedCaseRhs' expr) a
instance (Generic a, Show a) => TreeDebug PatternSynonym a
instance (Generic a, Show a) => TreeDebug PatSynRhs a
instance (Generic a, Show a) => TreeDebug PatSynWhere a
instance (Generic a, Show a) => TreeDebug PatternTypeSignature a
instance (Generic a, Show a) => TreeDebug Role a
instance (Generic a, Show a) => TreeDebug Cmd a
instance (Generic a, Show a) => TreeDebug LanguageExtension a
instance (Generic a, Show a) => TreeDebug MatchLhs a

-- Literal
instance (Generic a, Show a) => TreeDebug Literal a
instance (Generic a, Show a, TreeDebug k a, Show (k a), Generic (k a)) => TreeDebug (Promoted k) a

-- Base
instance (Generic a, Show a) => TreeDebug Operator a
instance (Generic a, Show a) => TreeDebug Name a
instance (Generic a, Show a) => TreeDebug SimpleName a
instance (Generic a, Show a) => TreeDebug UnqualName a
instance (Generic a, Show a) => TreeDebug StringNode a
instance (Generic a, Show a) => TreeDebug DataOrNewtypeKeyword a
instance (Generic a, Show a) => TreeDebug DoKind a
instance (Generic a, Show a) => TreeDebug TypeKeyword a
instance (Generic a, Show a) => TreeDebug OverlapPragma a
instance (Generic a, Show a) => TreeDebug CallConv a
instance (Generic a, Show a) => TreeDebug ArrowAppl a
instance (Generic a, Show a) => TreeDebug Safety a
instance (Generic a, Show a) => TreeDebug Assoc a
instance (Generic a, Show a) => TreeDebug Precedence a
instance (Generic a, Show a) => TreeDebug PhaseControl a
instance (Generic a, Show a) => TreeDebug PhaseNumber a
instance (Generic a, Show a) => TreeDebug PhaseInvert a
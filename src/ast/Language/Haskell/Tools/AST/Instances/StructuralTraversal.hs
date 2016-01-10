{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Language.Haskell.Tools.AST.Instances.StructuralTraversal where

import Control.Applicative
import Data.StructuralTraversal

import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
instance StructuralTraversable elem => StructuralTraversable (Ann elem) where
  structTraverse desc asc f (Ann ann e) = flip Ann <$> (desc *> structTraverse desc asc f e <* asc) <*> f ann
  
instance StructuralTraversable elem => StructuralTraversable (AnnMaybe elem) where
  structTraverse desc asc f (AnnMaybe (Just annotated)) = AnnMaybe . Just <$> (structTraverse desc asc f annotated)
  structTraverse desc asc f (AnnMaybe Nothing) = pure (AnnMaybe Nothing)

instance StructuralTraversable elem => StructuralTraversable (AnnList elem) where
  structTraverse desc asc f (AnnList ls) = AnnList <$> sequenceA (map (structTraverse desc asc f) ls)

-- Modules
deriveStructTrav ''Module
deriveStructTrav ''ModuleHead
deriveStructTrav ''ExportSpecList
deriveStructTrav ''ExportSpec
deriveStructTrav ''IESpec
deriveStructTrav ''SubSpec
deriveStructTrav ''ModulePragma
deriveStructTrav ''ImportDecl
deriveStructTrav ''ImportSpec
deriveStructTrav ''ImportQualified
deriveStructTrav ''ImportSource
deriveStructTrav ''ImportSafe
deriveStructTrav ''TypeNamespace
deriveStructTrav ''ImportRenaming

-- Declarations
deriveStructTrav ''Decl
deriveStructTrav ''ClassBody
deriveStructTrav ''GadtDeclList
deriveStructTrav ''ClassElement
deriveStructTrav ''DeclHead
deriveStructTrav ''InstBody
deriveStructTrav ''InstBodyDecl
deriveStructTrav ''GadtDecl
deriveStructTrav ''GadtField
deriveStructTrav ''FunDeps
deriveStructTrav ''FunDep
deriveStructTrav ''ConDecl
deriveStructTrav ''FieldDecl
deriveStructTrav ''Deriving
deriveStructTrav ''InstanceRule
deriveStructTrav ''InstanceHead
deriveStructTrav ''TypeEqn
deriveStructTrav ''KindConstraint
deriveStructTrav ''TyVar
deriveStructTrav ''Type
deriveStructTrav ''Kind
deriveStructTrav ''Context
deriveStructTrav ''Assertion
deriveStructTrav ''Expr
deriveStructTrav ''Stmt
deriveStructTrav ''CompStmt
deriveStructTrav ''FunBind
deriveStructTrav ''Pattern
deriveStructTrav ''PatternField
deriveStructTrav ''Splice
deriveStructTrav ''QQString
deriveStructTrav ''Match
deriveStructTrav ''Alt
deriveStructTrav ''Binds
deriveStructTrav ''Rhs
deriveStructTrav ''GuardedRhs
deriveStructTrav ''FieldUpdate
deriveStructTrav ''Bracket
deriveStructTrav ''TopLevelPragma
deriveStructTrav ''Rule
deriveStructTrav ''Annotation
deriveStructTrav ''MinimalFormula
deriveStructTrav ''ExprPragma
deriveStructTrav ''SourceRange
deriveStructTrav ''Number

-- Literal
deriveStructTrav ''Literal
deriveStructTrav ''Promoted

-- Base
deriveStructTrav ''Name
deriveStructTrav ''SimpleName
deriveStructTrav ''StringNode
deriveStructTrav ''DataOrNewtypeKeyword
deriveStructTrav ''DoKind
deriveStructTrav ''TypeKeyword
deriveStructTrav ''OverlapPragma
deriveStructTrav ''CallConv
deriveStructTrav ''ArrowAppl
deriveStructTrav ''Safety
deriveStructTrav ''Assoc
deriveStructTrav ''Precedence
deriveStructTrav ''PhaseControl
deriveStructTrav ''PhaseNumber
deriveStructTrav ''PhaseInvert
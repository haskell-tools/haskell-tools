-- | Generating StructuralTraversal instances for Haskell Representation
{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Language.Haskell.Tools.AST.Instances.StructuralTraversal where

import Control.Applicative
import Data.StructuralTraversal

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
instance StructuralTraversable elem => StructuralTraversable (Ann elem) where
  traverseUp desc asc f (Ann ann e) = flip Ann <$> (desc *> traverseUp desc asc f e <* asc) <*> f ann
  traverseDown desc asc f (Ann ann e) = Ann <$> f ann <*> (desc *> traverseDown desc asc f e <* asc)
  
instance StructuralTraversable elem => StructuralTraversable (AnnMaybe elem) where
  traverseUp desc asc f (AnnMaybe a (Just annotated)) 
    = flip AnnMaybe <$> (Just <$> (desc *> traverseUp desc asc f annotated <* asc)) <*> f a
  traverseUp desc asc f (AnnMaybe a Nothing) = AnnMaybe <$> f a <*> pure Nothing
  
  traverseDown desc asc f (AnnMaybe a (Just annotated)) 
    = AnnMaybe <$> f a <*> (Just <$> (desc *> traverseDown desc asc f annotated <* asc))
  traverseDown desc asc f (AnnMaybe a Nothing) = AnnMaybe <$> f a <*> pure Nothing

instance StructuralTraversable elem => StructuralTraversable (AnnList elem) where
  traverseUp desc asc f (AnnList a ls) 
    = flip AnnList <$> sequenceA (map (\e -> desc *> traverseUp desc asc f e <* asc) ls) <*> f a
  traverseDown desc asc f (AnnList a ls) 
    = AnnList <$> f a <*> sequenceA (map (\e -> desc *> traverseDown desc asc f e <* asc) ls)

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
deriveStructTrav ''CompStmt
deriveStructTrav ''ValueBind
deriveStructTrav ''Pattern
deriveStructTrav ''PatternField
deriveStructTrav ''Splice
deriveStructTrav ''QQString
deriveStructTrav ''Match
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
deriveStructTrav ''QuasiQuote
deriveStructTrav ''RhsGuard
deriveStructTrav ''LocalBind
deriveStructTrav ''LocalBinds
deriveStructTrav ''FixitySignature
deriveStructTrav ''TypeSignature
deriveStructTrav ''ListCompBody
deriveStructTrav ''TupSecElem
deriveStructTrav ''TypeFamily
deriveStructTrav ''PatternSynonym
deriveStructTrav ''PatSynRhs
deriveStructTrav ''PatSynWhere
deriveStructTrav ''PatternTypeSignature
deriveStructTrav ''Role
deriveStructTrav ''Cmd
deriveStructTrav ''CmdStmt
deriveStructTrav ''LanguageExtension


-- FIXME: structural traversal deriving does not respect the instance requirements for type like Ann expr a
instance StructuralTraversable expr => StructuralTraversable (Stmt' expr) where
  traverseUp desc asc f (BindStmt p e) = BindStmt <$> traverseUp desc asc f p <*> traverseUp desc asc f e
  traverseUp desc asc f (ExprStmt e) = ExprStmt <$> traverseUp desc asc f e
  traverseUp desc asc f (LetStmt bs) = LetStmt <$> traverseUp desc asc f bs
  traverseDown desc asc f (BindStmt p e) = BindStmt <$> traverseDown desc asc f p <*> traverseDown desc asc f e
  traverseDown desc asc f (ExprStmt e) = ExprStmt <$> traverseDown desc asc f e
  traverseDown desc asc f (LetStmt bs) = LetStmt <$> traverseDown desc asc f bs

instance StructuralTraversable expr => StructuralTraversable (Alt' expr) where
  traverseUp desc asc f (Alt p r b) = Alt <$> traverseUp desc asc f p <*> traverseUp desc asc f r <*> traverseUp desc asc f b
  traverseDown desc asc f (Alt p r b) = Alt <$> traverseDown desc asc f p <*> traverseDown desc asc f r <*> traverseDown desc asc f b

-- FIXME: structural traversal deriving does not respect the instance requirements for type like Ann expr a
instance StructuralTraversable expr => StructuralTraversable (CaseRhs' expr) where
  traverseUp desc asc f (UnguardedCaseRhs e) = UnguardedCaseRhs <$> traverseUp desc asc f e
  traverseUp desc asc f (GuardedCaseRhss g) = GuardedCaseRhss <$> traverseUp desc asc f g
  traverseDown desc asc f (UnguardedCaseRhs e) = UnguardedCaseRhs <$> traverseDown desc asc f e
  traverseDown desc asc f (GuardedCaseRhss g) = GuardedCaseRhss <$> traverseDown desc asc f g

instance StructuralTraversable expr => StructuralTraversable (GuardedCaseRhs' expr) where
  traverseUp desc asc f (GuardedCaseRhs g e) = GuardedCaseRhs <$> traverseUp desc asc f g <*> traverseUp desc asc f e
  traverseDown desc asc f (GuardedCaseRhs g e) = GuardedCaseRhs <$> traverseDown desc asc f g <*> traverseDown desc asc f e

-- Literal
deriveStructTrav ''Literal
deriveStructTrav ''Promoted

-- Base
deriveStructTrav ''Operator
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
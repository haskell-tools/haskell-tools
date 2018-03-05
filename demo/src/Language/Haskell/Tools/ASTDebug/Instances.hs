{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, UndecidableInstances #-}

module Language.Haskell.Tools.ASTDebug.Instances where

import Language.Haskell.Tools.ASTDebug

import Control.Reference
import GHC.Generics (Generic(..))

import Language.Haskell.Tools.AST

-- Annotations
instance {-# OVERLAPPING #-} (ASTDebug UQualifiedName dom st) => ASTDebug (Ann UQualifiedName) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (NameInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug UExpr dom st) => ASTDebug (Ann UExpr) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ExprInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug UImportDecl dom st) => ASTDebug (Ann UImportDecl) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ImportInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug UModule dom st) => ASTDebug (Ann UModule) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ModuleInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance {-# OVERLAPPING #-} (ASTDebug UFieldWildcard dom st) => ASTDebug (Ann UFieldWildcard) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= (ImplicitFieldInfoType (a ^. semanticInfo) (getRange (a ^. sourceInfo))) $ astDebug' e

instance ASTDebug e dom st => ASTDebug (Ann e) dom st where
  astDebug' (Ann a e) = traversal&nodeSubtree&nodeInfo .= DefaultInfoType (getRange (a ^. sourceInfo)) $ astDebug' e

-- FIXME: WHY do I have to write it separately?
instance {-# OVERLAPPING #-} (ASTDebug UImportDecl dom st) => ASTDebug (AnnListG UImportDecl) dom st where
  astDebug' (AnnListG a ls) = [TreeNode "" (TreeDebugNode "*" (DefaultInfoType (getRange (a ^. sourceInfo))) (concatMap astDebug' ls))]

instance (ASTDebug e dom st) => ASTDebug (AnnListG e) dom st where
  astDebug' (AnnListG a ls) = [TreeNode "" (TreeDebugNode "*" (DefaultInfoType (getRange (a ^. sourceInfo))) (concatMap astDebug' ls))]

instance (ASTDebug e dom st) => ASTDebug (AnnMaybeG e) dom st where
  astDebug' (AnnMaybeG a e) = [TreeNode "" (TreeDebugNode "?" (DefaultInfoType (getRange (a ^. sourceInfo))) (maybe [] astDebug' e))]

-- Modules
instance (Domain dom, SourceInfo st) => ASTDebug UModule dom st
instance (Domain dom, SourceInfo st) => ASTDebug UModuleHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug UExportSpecs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UExportSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug UIESpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug USubSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug UModulePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFilePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportSpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportModifier dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportQualified dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportSource dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportSafe dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTypeNamespace dom st
instance (Domain dom, SourceInfo st) => ASTDebug UImportRenaming dom st

-- Declarations
instance (Domain dom, SourceInfo st) => ASTDebug UDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UClassBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug UClassElement dom st
instance (Domain dom, SourceInfo st) => ASTDebug UDeclHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInstBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInstBodyDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UGadtConDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UGadtConType dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFieldWildcard dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFunDeps dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFunDep dom st
instance (Domain dom, SourceInfo st) => ASTDebug UConDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFieldDecl dom st
instance (Domain dom, SourceInfo st) => ASTDebug UDeriving dom st
instance (Domain dom, SourceInfo st) => ASTDebug UDeriveStrategy dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInstanceRule dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInstanceHead dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTypeEqn dom st
instance (Domain dom, SourceInfo st) => ASTDebug UKindConstraint dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTyVar dom st
instance (Domain dom, SourceInfo st) => ASTDebug UType dom st
instance (Domain dom, SourceInfo st) => ASTDebug UKind dom st
instance (Domain dom, SourceInfo st) => ASTDebug UContext dom st
instance (Domain dom, SourceInfo st) => ASTDebug UAssertion dom st
instance (Domain dom, SourceInfo st) => ASTDebug UExpr dom st
instance (Domain dom, SourceInfo st) => ASTDebug (UStmt' UExpr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug (UStmt' UCmd) dom st
instance (Domain dom, SourceInfo st) => ASTDebug UCompStmt dom st
instance (Domain dom, SourceInfo st) => ASTDebug UValueBind dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPattern dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatternField dom st
instance (Domain dom, SourceInfo st) => ASTDebug USplice dom st
instance (Domain dom, SourceInfo st) => ASTDebug QQString dom st
instance (Domain dom, SourceInfo st) => ASTDebug UMatch dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (UAlt' expr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug URhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UGuardedRhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFieldUpdate dom st
instance (Domain dom, SourceInfo st) => ASTDebug UBracket dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTopLevelPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug URule dom st
instance (Domain dom, SourceInfo st) => ASTDebug URuleVar dom st
instance (Domain dom, SourceInfo st) => ASTDebug UAnnotationSubject dom st
instance (Domain dom, SourceInfo st) => ASTDebug UMinimalFormula dom st
instance (Domain dom, SourceInfo st) => ASTDebug UExprPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug USourceRange dom st
instance (Domain dom, SourceInfo st) => ASTDebug Number dom st
instance (Domain dom, SourceInfo st) => ASTDebug UQuasiQuote dom st
instance (Domain dom, SourceInfo st) => ASTDebug URhsGuard dom st
instance (Domain dom, SourceInfo st) => ASTDebug ULocalBind dom st
instance (Domain dom, SourceInfo st) => ASTDebug ULocalBinds dom st
instance (Domain dom, SourceInfo st) => ASTDebug UFixitySignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTypeSignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug UListCompBody dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTupSecElem dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTypeFamily dom st
instance (Domain dom, SourceInfo st) => ASTDebug UTypeFamilySpec dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInjectivityAnn dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (UCaseRhs' expr) dom st
instance (Domain dom, SourceInfo st, ASTDebug expr dom st, Generic (expr dom st)) => ASTDebug (UGuardedCaseRhs' expr) dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatternSynonym dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatSynRhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatSynLhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatSynWhere dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPatternTypeSignature dom st
instance (Domain dom, SourceInfo st) => ASTDebug URole dom st
instance (Domain dom, SourceInfo st) => ASTDebug UCmd dom st
instance (Domain dom, SourceInfo st) => ASTDebug ULanguageExtension dom st
instance (Domain dom, SourceInfo st) => ASTDebug UMatchLhs dom st
instance (Domain dom, SourceInfo st) => ASTDebug UInlinePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug USpecializePragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug UUnboxedSumPlaceHolder dom st

-- ULiteral
instance (Domain dom, SourceInfo st) => ASTDebug ULiteral dom st
instance (Domain dom, SourceInfo st, ASTDebug k dom st, Generic (k dom st)) => ASTDebug (UPromoted k) dom st

-- Base
instance (Domain dom, SourceInfo st) => ASTDebug UOperator dom st
instance (Domain dom, SourceInfo st) => ASTDebug UName dom st
instance (Domain dom, SourceInfo st) => ASTDebug UQualifiedName dom st
instance (Domain dom, SourceInfo st) => ASTDebug UModuleName dom st
instance (Domain dom, SourceInfo st) => ASTDebug UNamePart dom st
instance (Domain dom, SourceInfo st) => ASTDebug UStringNode dom st
instance (Domain dom, SourceInfo st) => ASTDebug UDataOrNewtypeKeyword dom st
instance (Domain dom, SourceInfo st) => ASTDebug UDoKind dom st
instance (Domain dom, SourceInfo st) => ASTDebug TypeKeyword dom st
instance (Domain dom, SourceInfo st) => ASTDebug UOverlapPragma dom st
instance (Domain dom, SourceInfo st) => ASTDebug UCallConv dom st
instance (Domain dom, SourceInfo st) => ASTDebug UArrowAppl dom st
instance (Domain dom, SourceInfo st) => ASTDebug USafety dom st
instance (Domain dom, SourceInfo st) => ASTDebug UConlikeAnnot dom st
instance (Domain dom, SourceInfo st) => ASTDebug Assoc dom st
instance (Domain dom, SourceInfo st) => ASTDebug Precedence dom st
instance (Domain dom, SourceInfo st) => ASTDebug LineNumber dom st
instance (Domain dom, SourceInfo st) => ASTDebug UPhaseControl dom st
instance (Domain dom, SourceInfo st) => ASTDebug PhaseNumber dom st
instance (Domain dom, SourceInfo st) => ASTDebug PhaseInvert dom st

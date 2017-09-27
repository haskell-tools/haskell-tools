{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveGeneric
           , UndecidableInstances
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Debug.RangeDebugInstances where

import Language.Haskell.Tools.Debug.RangeDebug (TreeDebug(..))

import Control.Reference ((^.))
import GHC.Generics (Generic(..))

import Language.Haskell.Tools.AST

-- Annotations
instance TreeDebug e dom st => TreeDebug (Ann e) dom st where
  treeDebug' i (Ann a e) = identLine i ++ show (a ^. sourceInfo) ++ " " ++ take 40 (show e) ++ "..." ++ treeDebug' (i+1) e

identLine :: Int -> String
identLine i = "\n" ++ replicate (i*2) ' '

instance TreeDebug e dom st => TreeDebug (AnnListG e) dom st where
  treeDebug' i (AnnListG a ls) = identLine i ++ show (a ^. sourceInfo) ++ " <*>" ++ concatMap (treeDebug' (i + 1)) ls

instance TreeDebug e dom st => TreeDebug (AnnMaybeG e) dom st where
  treeDebug' i (AnnMaybeG a e) = identLine i ++ show (a ^. sourceInfo) ++ " <?>" ++ maybe "" (\e -> treeDebug' (i + 1) e) e

-- Modules
instance (SourceInfo st, Domain dom) => TreeDebug UModule dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModuleHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExportSpecs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UIESpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug USubSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModulePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFilePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportModifier dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportQualified dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSource dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSafe dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeNamespace dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportRenaming dom st

-- Declarations
instance (SourceInfo st, Domain dom) => TreeDebug UDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UClassBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UClassElement dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDeclHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstBodyDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGadtConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGadtConType dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldWildcard dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFunDeps dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFunDep dom st
instance (SourceInfo st, Domain dom) => TreeDebug UConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDeriving dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDeriveStrategy dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstanceRule dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstanceHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeEqn dom st
instance (SourceInfo st, Domain dom) => TreeDebug UKindConstraint dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTyVar dom st
instance (SourceInfo st, Domain dom) => TreeDebug UType dom st
instance (SourceInfo st, Domain dom) => TreeDebug UKind dom st
instance (SourceInfo st, Domain dom) => TreeDebug UContext dom st
instance (SourceInfo st, Domain dom) => TreeDebug UAssertion dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExpr dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UStmt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug UCompStmt dom st
instance (SourceInfo st, Domain dom) => TreeDebug UValueBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPattern dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatternField dom st
instance (SourceInfo st, Domain dom) => TreeDebug USplice dom st
instance (SourceInfo st, Domain dom) => TreeDebug QQString dom st
instance (SourceInfo st, Domain dom) => TreeDebug UMatch dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UAlt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug URhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGuardedRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldUpdate dom st
instance (SourceInfo st, Domain dom) => TreeDebug UBracket dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTopLevelPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug URule dom st
instance (SourceInfo st, Domain dom) => TreeDebug URuleVar dom st
instance (SourceInfo st, Domain dom) => TreeDebug UAnnotationSubject dom st
instance (SourceInfo st, Domain dom) => TreeDebug UMinimalFormula dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExprPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug USourceRange dom st
instance (SourceInfo st, Domain dom) => TreeDebug Number dom st
instance (SourceInfo st, Domain dom) => TreeDebug UQuasiQuote dom st
instance (SourceInfo st, Domain dom) => TreeDebug URhsGuard dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULocalBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULocalBinds dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFixitySignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug UListCompBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTupSecElem dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeFamily dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeFamilySpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInjectivityAnn dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UCaseRhs' expr) dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UGuardedCaseRhs' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatternSynonym dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynLhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynWhere dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatternTypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug URole dom st
instance (SourceInfo st, Domain dom) => TreeDebug UCmd dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULanguageExtension dom st
instance (SourceInfo st, Domain dom) => TreeDebug UMatchLhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInlinePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug USpecializePragma dom st

-- ULiteral
instance (SourceInfo st, Domain dom) => TreeDebug ULiteral dom st
instance (SourceInfo st, Domain dom, TreeDebug k dom st, Generic (k dom st)) => TreeDebug (UPromoted k) dom st

-- Base
instance (SourceInfo st, Domain dom) => TreeDebug UOperator dom st
instance (SourceInfo st, Domain dom) => TreeDebug UName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UQualifiedName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModuleName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UNamePart dom st
instance (SourceInfo st, Domain dom) => TreeDebug UStringNode dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDataOrNewtypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDoKind dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug UOverlapPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug UCallConv dom st
instance (SourceInfo st, Domain dom) => TreeDebug UArrowAppl dom st
instance (SourceInfo st, Domain dom) => TreeDebug USafety dom st
instance (SourceInfo st, Domain dom) => TreeDebug UConlikeAnnot dom st
instance (SourceInfo st, Domain dom) => TreeDebug Assoc dom st
instance (SourceInfo st, Domain dom) => TreeDebug Precedence dom st
instance (SourceInfo st, Domain dom) => TreeDebug LineNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPhaseControl dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseInvert dom st

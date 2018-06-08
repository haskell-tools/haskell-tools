-- | Data instances for Haskell AST (used for generics)
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Tools.AST.Instances.Data () where

import Data.Data

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Kinds (UPromoted(..), UKind(..), UKindConstraint(..))
import Language.Haskell.Tools.AST.Representation.Literals (ULiteral(..))
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns (UPatternField(..), UPattern(..))
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Types

-- Making the annotation an opaque attribute in annotated elements.
-- Needed because otherwise uniplate traverses the annotations, resulting in unnecessary
-- evaluation and storage.

instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage))
      => Data (Ann e dom stage) where
    gfoldl k z (Ann annot elem) = z (Ann annot) `k` elem -- ANNOT IS HIDDEN
    gunfold k z c = case constrIndex c of 1 -> k (k (z Ann))
    toConstr (Ann{}) = con_Ann
    dataTypeOf _ = ty_Ann

con_Ann = mkConstr ty_Ann "Ann" [] Prefix
ty_Ann   = mkDataType "Language.Haskell.Tools.AST.Ann.Ann" [con_Ann]

instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage))
      => Data (AnnMaybeG e dom stage) where
    gfoldl k z (AnnMaybeG annot elem) = z (AnnMaybeG annot) `k` elem -- ANNOT IS HIDDEN
    gunfold k z c = case constrIndex c of 1 -> k (k (z AnnMaybeG))
    toConstr (AnnMaybeG{}) = con_AnnMaybeG
    dataTypeOf _ = ty_AnnMaybeG

con_AnnMaybeG = mkConstr ty_AnnMaybeG "AnnMaybeG" [] Prefix
ty_AnnMaybeG   = mkDataType "Language.Haskell.Tools.AST.Ann.AnnMaybeG" [con_AnnMaybeG]

instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage))
      => Data (AnnListG e dom stage) where
    gfoldl k z (AnnListG annot elem) = z (AnnListG annot) `k` elem -- ANNOT IS HIDDEN
    gunfold k z c = case constrIndex c of 1 -> k (k (z AnnListG))
    toConstr (AnnListG{}) = con_AnnListG
    dataTypeOf _ = ty_AnnListG

con_AnnListG = mkConstr ty_AnnListG "AnnListG" [] Prefix
ty_AnnListG   = mkDataType "Language.Haskell.Tools.AST.Ann.AnnListG" [con_AnnListG]

-- Modules
deriving instance (Domain dom, SourceInfo stage) => Data (UModule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UExportSpecs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UIESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (USubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportModifier dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Data (UDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDeriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDeriveStrategy dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UKindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UContext dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UAssertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UExpr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (UStmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UCompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (USplice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UMatch dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (UAlt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UBracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URuleVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UAnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UMinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (USourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UQuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ULocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ULocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInjectivityAnn dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (UCaseRhs' expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage))=> Data (UGuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URole dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UCmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ULanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UMatchLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInlinePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (USpecializePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UUnboxedSumPlaceHolder dom stage)

-- ULiteral
deriving instance (Domain dom, SourceInfo stage) => Data (ULiteral dom stage)
deriving instance (DomainWith k dom, SourceInfo stage, Typeable k, Data (k dom stage)) => Data (UPromoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Data (UOperator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UQualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UNamePart dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UStringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UOverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UCallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (USafety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseInvert dom stage)

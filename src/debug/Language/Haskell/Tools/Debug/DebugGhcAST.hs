{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, UndecidableInstances #-}

-- | A module for showing GHC's syntax tree representation.
module Language.Haskell.Tools.Debug.DebugGhcAST where

import Language.Haskell.Tools.AST (shortShowSpan)

import Bag (bagToList, Bag(..))
import BasicTypes
import BooleanFormula (BooleanFormula(..))
import Coercion (Var(..), Role(..))
import ConLike (ConLike(..))
import CoreSyn (Tickish(..))
import FieldLabel (FieldLbl(..))
import ForeignCall
import GHC
import Module (Module(..), ModuleName(..))
import OccName (OccName(..))
import Outputable (Outputable(..), showSDocUnsafe)
import PatSyn (PatSyn(..))
import RdrName (RdrName(..))
import TcEvidence
import UniqFM (UniqFM(..))
import UniqSet (UniqSet(..))
import HsExtension

instance Show a => Show (Located a) where
  show (L l a) = "L(" ++ shortShowSpan l ++ ") (" ++ show a ++ ")"

deriving instance Show (ABExport GhcPs)
deriving instance Show (AmbiguousFieldOcc GhcPs)
deriving instance Show (AnnDecl GhcPs)
deriving instance Show (AnnProvenance RdrName)
deriving instance Show (ApplicativeArg GhcPs GhcPs)
deriving instance Show (ArithSeqInfo GhcPs)
deriving instance Show (BooleanFormula (Located RdrName))
deriving instance Show (ClsInstDecl GhcPs)
deriving instance Show (ConDecl GhcPs)
deriving instance Show (ConDeclField GhcPs)
deriving instance Show (DataFamInstDecl GhcPs)
deriving instance Show (DefaultDecl GhcPs)
deriving instance Show (DerivDecl GhcPs)
deriving instance Show (FamilyDecl GhcPs)
deriving instance Show (FamilyInfo GhcPs)
deriving instance Show (FamilyResultSig GhcPs)
deriving instance Show (FieldLbl RdrName)
deriving instance Show (FieldOcc GhcPs)
deriving instance Show (FixitySig GhcPs)
deriving instance Show (ForeignDecl GhcPs)
deriving instance Show a => Show (GRHS GhcPs a)
deriving instance Show a => Show (GRHSs GhcPs a)
deriving instance Show (InjectivityAnn GhcPs)
deriving instance Show (HsAppType GhcPs)
deriving instance Show (HsBindLR GhcPs GhcPs)
deriving instance Show (HsBracket GhcPs)
deriving instance Show (HsCmd GhcPs)
deriving instance Show (HsCmdTop GhcPs)
deriving instance Show (HsConDeclDetails GhcPs)
deriving instance Show (HsConPatDetails GhcPs)
deriving instance Show (HsDataDefn GhcPs)
deriving instance Show (HsDerivingClause GhcPs)
deriving instance Show (HsDecl GhcPs)
deriving instance Show (HsExpr GhcPs)
deriving instance Show (HsGroup GhcPs)
deriving instance Show (HsLit GhcPs)
deriving instance Show (HsLocalBindsLR GhcPs GhcPs)
deriving instance Show (HsModule GhcPs)
deriving instance Show (HsOverLit GhcPs)
deriving instance Show (HsPatSynDetails (Located RdrName))
deriving instance Show (HsPatSynDir GhcPs)
deriving instance Show (HsRecFields GhcPs (LPat GhcPs))
deriving instance Show (HsRecordBinds GhcPs)
deriving instance Show (HsSplice GhcPs)
deriving instance Show (HsSplicedThing GhcPs)
deriving instance Show (HsStmtContext RdrName)
deriving instance Show (HsTupArg GhcPs)
deriving instance Show (HsTyVarBndr GhcPs)
deriving instance Show (HsType GhcPs)
deriving instance Show (HsValBindsLR GhcPs GhcPs)
deriving instance Show (HsWildCardInfo GhcPs)
deriving instance Show (IE GhcPs)
deriving instance Show (IEWrappedName RdrName)
deriving instance Show (ImportDecl GhcPs)
deriving instance Show (InstDecl GhcPs)
deriving instance Show (LHsQTyVars GhcPs)
deriving instance Show a => Show (Match GhcPs a)
deriving instance Show (HsMatchContext RdrName)
deriving instance Show a => Show (MatchGroup GhcPs a)
deriving instance Show (ParStmtBlock GhcPs GhcPs)
deriving instance Show (Pat GhcPs)
deriving instance Show (PatSynBind GhcPs GhcPs)
deriving instance Show (RecordPatSynField (Located RdrName))
deriving instance Show (RoleAnnotDecl GhcPs)
deriving instance Show (RuleBndr GhcPs)
deriving instance Show (RuleDecl GhcPs)
deriving instance Show (RuleDecls GhcPs)
deriving instance Show (Sig GhcPs)
deriving instance Show (SpliceDecl GhcPs)
deriving instance Show (SyntaxExpr GhcPs)
deriving instance Show a => Show (StmtLR GhcPs GhcPs a)
deriving instance Show (TyClDecl GhcPs)
deriving instance Show (TyClGroup GhcPs)
deriving instance (Show a, Show b) => Show (FamEqn GhcPs a b)
deriving instance Show (TyFamInstDecl GhcPs)
deriving instance Show (VectDecl GhcPs)
deriving instance Show (WarnDecl GhcPs)
deriving instance Show (WarnDecls GhcPs)


deriving instance Show (ABExport GhcRn)
deriving instance Show (AmbiguousFieldOcc GhcRn)
deriving instance Show (AnnDecl GhcRn)
deriving instance Show (AnnProvenance Name)
deriving instance Show (ApplicativeArg GhcRn GhcRn)
deriving instance Show (ArithSeqInfo GhcRn)
deriving instance Show (BooleanFormula (Located Name))
deriving instance Show (ClsInstDecl GhcRn)
deriving instance Show (ConDecl GhcRn)
deriving instance Show (ConDeclField GhcRn)
deriving instance Show (DataFamInstDecl GhcRn)
deriving instance Show (DefaultDecl GhcRn)
deriving instance Show (DerivDecl GhcRn)
deriving instance Show (FamilyDecl GhcRn)
deriving instance Show (FamilyInfo GhcRn)
deriving instance Show (FamilyResultSig GhcRn)
deriving instance Show (FieldLbl Name)
deriving instance Show (FieldOcc GhcRn)
deriving instance Show (FixitySig GhcRn)
deriving instance Show (ForeignDecl GhcRn)
deriving instance Show a => Show (GRHS GhcRn a)
deriving instance Show a => Show (GRHSs GhcRn a)
deriving instance Show (InjectivityAnn GhcRn)
deriving instance Show (HsAppType GhcRn)
deriving instance Show (HsBindLR GhcRn GhcRn)
deriving instance Show (HsBracket GhcRn)
deriving instance Show (HsCmd GhcRn)
deriving instance Show (HsCmdTop GhcRn)
deriving instance Show (HsConDeclDetails GhcRn)
deriving instance Show (HsConPatDetails GhcRn)
deriving instance Show (HsDataDefn GhcRn)
deriving instance Show (HsDerivingClause GhcRn)
deriving instance Show (HsDecl GhcRn)
deriving instance Show (HsExpr GhcRn)
deriving instance Show (HsGroup GhcRn)
deriving instance Show (HsLit GhcRn)
deriving instance Show (HsLocalBindsLR GhcRn GhcRn)
deriving instance Show (HsMatchContext Name)
deriving instance Show (HsModule GhcRn)
deriving instance Show (HsOverLit GhcRn)
deriving instance Show (HsPatSynDetails (Located Name))
deriving instance Show (HsPatSynDir GhcRn)
deriving instance Show (HsRecFields GhcRn (LPat GhcRn))
deriving instance Show (HsRecordBinds GhcRn)
deriving instance Show (HsSplice GhcRn)
deriving instance Show (HsSplicedThing GhcRn)
deriving instance Show (HsStmtContext Name)
deriving instance Show (HsTupArg GhcRn)
deriving instance Show (HsTyVarBndr GhcRn)
deriving instance Show (HsType GhcRn)
deriving instance Show (HsValBindsLR GhcRn GhcRn)
deriving instance Show (HsWildCardInfo GhcRn)
deriving instance Show (IE GhcRn)
deriving instance Show (IEWrappedName Name)
deriving instance Show (ImportDecl GhcRn)
deriving instance Show (InstDecl GhcRn)
deriving instance Show (LHsQTyVars GhcRn)
deriving instance Show a => Show (Match GhcRn a)
deriving instance Show a => Show (MatchGroup GhcRn a)
deriving instance Show (ParStmtBlock GhcRn GhcRn)
deriving instance Show (Pat GhcRn)
deriving instance Show (PatSynBind GhcRn GhcRn)
deriving instance Show (RecordPatSynField (Located Name))
deriving instance Show (RoleAnnotDecl GhcRn)
deriving instance Show (RuleBndr GhcRn)
deriving instance Show (RuleDecl GhcRn)
deriving instance Show (RuleDecls GhcRn)
deriving instance Show (Sig GhcRn)
deriving instance Show (SpliceDecl GhcRn)
deriving instance Show (SyntaxExpr GhcRn)
deriving instance Show a => Show (StmtLR GhcRn GhcRn a)
deriving instance Show (TyClDecl GhcRn)
deriving instance Show (TyClGroup GhcRn)
deriving instance (Show a, Show b) => Show (FamEqn GhcRn a b)
deriving instance Show (TyFamInstDecl GhcRn)
deriving instance Show (VectDecl GhcRn)
deriving instance Show (WarnDecl GhcRn)
deriving instance Show (WarnDecls GhcRn)


deriving instance Show (ABExport GhcTc)
deriving instance Show (AmbiguousFieldOcc GhcTc)
deriving instance Show (AnnDecl GhcTc)
deriving instance Show (AnnProvenance Id)
deriving instance Show (ApplicativeArg GhcTc GhcTc)
deriving instance Show (ArithSeqInfo GhcTc)
deriving instance Show (BooleanFormula (Located Id))
deriving instance Show (ClsInstDecl GhcTc)
deriving instance Show (ConDecl GhcTc)
deriving instance Show (ConDeclField GhcTc)
deriving instance Show (DataFamInstDecl GhcTc)
deriving instance Show (DefaultDecl GhcTc)
deriving instance Show (DerivDecl GhcTc)
deriving instance Show (FamilyDecl GhcTc)
deriving instance Show (FamilyInfo GhcTc)
deriving instance Show (FamilyResultSig GhcTc)
deriving instance Show (FieldLbl Id)
deriving instance Show (FieldOcc GhcTc)
deriving instance Show (FixitySig GhcTc)
deriving instance Show (ForeignDecl GhcTc)
deriving instance Show a => Show (GRHS GhcTc a)
deriving instance Show a => Show (GRHSs GhcTc a)
deriving instance Show (InjectivityAnn GhcTc)
deriving instance Show (HsAppType GhcTc)
deriving instance Show (HsBindLR GhcTc GhcTc)
deriving instance Show (HsBracket GhcTc)
deriving instance Show (HsCmd GhcTc)
deriving instance Show (HsCmdTop GhcTc)
deriving instance Show (HsConDeclDetails GhcTc)
deriving instance Show (HsConPatDetails GhcTc)
deriving instance Show (HsDataDefn GhcTc)
deriving instance Show (HsDerivingClause GhcTc)
deriving instance Show (HsDecl GhcTc)
deriving instance Show (HsExpr GhcTc)
deriving instance Show (HsGroup GhcTc)
deriving instance Show (HsLit GhcTc)
deriving instance Show (HsLocalBindsLR GhcTc GhcTc)
deriving instance Show (HsMatchContext Id)
deriving instance Show (HsModule GhcTc)
deriving instance Show (HsOverLit GhcTc)
deriving instance Show (HsPatSynDetails (Located Id))
deriving instance Show (HsPatSynDir GhcTc)
deriving instance Show (HsRecFields GhcTc (LPat GhcTc))
deriving instance Show (HsRecordBinds GhcTc)
deriving instance Show (HsSplice GhcTc)
deriving instance Show (HsSplicedThing GhcTc)
deriving instance Show (HsStmtContext Id)
deriving instance Show (HsTupArg GhcTc)
deriving instance Show (HsTyVarBndr GhcTc)
deriving instance Show (HsType GhcTc)
deriving instance Show (HsValBindsLR GhcTc GhcTc)
deriving instance Show (HsWildCardInfo GhcTc)
deriving instance Show (IE GhcTc)
deriving instance Show (IEWrappedName Id)
deriving instance Show (ImportDecl GhcTc)
deriving instance Show (InstDecl GhcTc)
deriving instance Show (LHsQTyVars GhcTc)
deriving instance Show a => Show (Match GhcTc a)
deriving instance Show a => Show (MatchGroup GhcTc a)
deriving instance Show (ParStmtBlock GhcTc GhcTc)
deriving instance Show (Pat GhcTc)
deriving instance Show (PatSynBind GhcTc GhcTc)
deriving instance Show (RecordPatSynField (Located Id))
deriving instance Show (RoleAnnotDecl GhcTc)
deriving instance Show (RuleBndr GhcTc)
deriving instance Show (RuleDecl GhcTc)
deriving instance Show (RuleDecls GhcTc)
deriving instance Show (Sig GhcTc)
deriving instance Show (SpliceDecl GhcTc)
deriving instance Show (SyntaxExpr GhcTc)
deriving instance Show a => Show (StmtLR GhcTc GhcTc a)
deriving instance Show (TyClDecl GhcTc)
deriving instance Show (TyClGroup GhcTc)
deriving instance (Show a, Show b) => Show (FamEqn GhcTc a b)
deriving instance Show (TyFamInstDecl GhcTc)
deriving instance Show (VectDecl GhcTc)
deriving instance Show (WarnDecl GhcTc)
deriving instance Show (WarnDecls GhcTc)

deriving instance Show Activation
deriving instance Show HsArrAppType
deriving instance Show Boxity
deriving instance Show CType
deriving instance Show CImportSpec
deriving instance Show CExportSpec
deriving instance Show CCallConv
deriving instance Show CCallTarget
deriving instance Show ConLike
deriving instance Show DocDecl
deriving instance Show Fixity
deriving instance Show FixityDirection
deriving instance Show ForeignImport
deriving instance Show ForeignExport
deriving instance Show Header
deriving instance Show HsIPName
deriving instance Show HsTupleSort
deriving instance Show HsSrcBang
deriving instance Show InlinePragma
deriving instance Show NewOrData
deriving instance Show Origin
deriving instance Show OverLitVal
deriving instance Show OverlapMode
deriving instance Show PlaceHolder
deriving instance Show Role
deriving instance Show RecFlag
deriving instance Show SpliceExplicitFlag
deriving instance Show TcSpecPrag
deriving instance Show TcSpecPrags
deriving instance Show TransForm
deriving instance Show WarningTxt
deriving instance Show PendingRnSplice
deriving instance Show PendingTcSplice

instance Show UnboundVar where
  show (OutOfScope n _) = "OutOfScope " ++ show n
  show (TrueExprHole n) = "TrueExprHole " ++ show n



instance Show ModuleName where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
instance Show ClsInst where
  show = showSDocUnsafe . ppr
instance Show Type where
  show = showSDocUnsafe . ppr
instance Show OccName where
  show = showSDocUnsafe . ppr
-- instance Show RdrName where
  -- show = showSDocUnsafe . ppr

deriving instance Show RdrName
deriving instance Show Module
deriving instance Show StringLiteral
deriving instance Show UntypedSpliceFlavour
deriving instance Show SrcUnpackedness
deriving instance Show SrcStrictness
deriving instance Show IEWildcard

deriving instance (Show t) => Show (HsImplicitBndrs GhcPs t)
deriving instance (Show t) => Show (HsImplicitBndrs GhcRn t)
deriving instance (Show t) => Show (HsImplicitBndrs GhcTc t)
deriving instance (Show t) => Show (HsWildCardBndrs GhcPs t)
deriving instance (Show t) => Show (HsWildCardBndrs GhcRn t)
deriving instance (Show t) => Show (HsWildCardBndrs GhcTc t)
deriving instance (Show a, Show b) => Show (HsRecField' a b)

instance Show Name where
  show = showSDocUnsafe . ppr
instance Show HsTyLit where
  show = showSDocUnsafe . ppr
instance Show Var where
  show = showSDocUnsafe . ppr
instance Show DataCon where
  show = showSDocUnsafe . ppr
instance Show PatSyn where
  show = showSDocUnsafe . ppr
instance Show TcEvBinds where
  show = showSDocUnsafe . ppr
instance Show HsWrapper where
  show = showSDocUnsafe . ppr
instance Show Class where
  show = showSDocUnsafe . ppr
instance Show TcCoercion where
  show = showSDocUnsafe . ppr
instance Show DerivStrategy where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (UniqFM a) where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (UniqSet a) where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (Tickish a) where
  show = showSDocUnsafe . ppr
instance (OutputableBndrId a, SourceTextX a) => Show (HsIPBinds a) where
  show = showSDocUnsafe . ppr
instance Show LexicalFixity where
  show = showSDocUnsafe . ppr

instance Show a => Show (Bag a) where
  show = show . bagToList

instance Show ThModFinalizers where
  show _ = ""

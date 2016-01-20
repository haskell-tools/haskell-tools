{-# LANGUAGE StandaloneDeriving
           , TypeSynonymInstances 
           , FlexibleInstances 
           #-}
module Language.Haskell.Tools.Refactor.DebugGhcAST where

import Language.Haskell.Tools.Refactor.RangeDebug

import GHC
import HsSyn
import HsDecls
import Module
import Coercion
import SrcLoc
import RdrName
import BasicTypes
import Outputable
import TyCon
import PlaceHolder
import ForeignCall
import Var
import ConLike
import PatSyn
import TcEvidence
import Bag
import BooleanFormula
import CoreSyn
import UniqFM
import OccName

instance Show a => Show (Located a) where
  show (L l a) = "L(" ++ shortShowSpan l ++ ") (" ++ show a ++ ")"

deriving instance Show (ABExport RdrName)
deriving instance Show (AnnDecl RdrName)
deriving instance Show (AnnProvenance RdrName)
deriving instance Show (ArithSeqInfo RdrName)
deriving instance Show (BooleanFormula (Located RdrName))
deriving instance Show (ClsInstDecl RdrName)
deriving instance Show (ConDecl RdrName)
deriving instance Show (ConDeclField RdrName)
deriving instance Show (DataFamInstDecl RdrName)
deriving instance Show (DefaultDecl RdrName)
deriving instance Show (DerivDecl RdrName)
deriving instance Show (FamilyDecl RdrName)
deriving instance Show (FamilyInfo RdrName)
deriving instance Show (FixitySig RdrName)
deriving instance Show (ForeignDecl RdrName)
deriving instance Show (GRHS RdrName (LHsCmd RdrName))
deriving instance Show (GRHS RdrName (LHsExpr RdrName))
deriving instance Show (GRHSs RdrName (LHsCmd RdrName))
deriving instance Show (GRHSs RdrName (LHsExpr RdrName))
deriving instance Show (HsBindLR RdrName RdrName)
deriving instance Show (HsBracket RdrName)
deriving instance Show (HsCmd RdrName)
deriving instance Show (HsCmdTop RdrName)
deriving instance Show (HsConDeclDetails RdrName)
deriving instance Show (HsConPatDetails RdrName)
deriving instance Show (HsDataDefn RdrName)
deriving instance Show (HsDecl RdrName)
deriving instance Show (HsExpr RdrName)
deriving instance Show (HsGroup RdrName)
deriving instance Show (HsLocalBindsLR RdrName RdrName)
deriving instance Show (HsMatchContext RdrName)
deriving instance Show (HsModule RdrName)
deriving instance Show (HsOverLit RdrName)
deriving instance Show (HsPatSynDetails (Located RdrName))
deriving instance Show (HsPatSynDir RdrName)
deriving instance Show (HsQuasiQuote RdrName)
deriving instance Show (HsRecField RdrName (LHsExpr RdrName))
deriving instance Show (HsRecField RdrName (LPat RdrName))
deriving instance Show (HsRecFields RdrName (LPat RdrName))
deriving instance Show (HsRecordBinds RdrName)
deriving instance Show (HsSplice RdrName)
deriving instance Show (HsStmtContext RdrName)
deriving instance Show (HsTupArg RdrName)
deriving instance Show (HsTyPats RdrName)
deriving instance Show (HsTyVarBndr RdrName)
deriving instance Show (HsType RdrName)
deriving instance Show (HsValBindsLR RdrName RdrName)
deriving instance Show (HsWithBndrs RdrName (LHsType RdrName))
deriving instance Show (IE RdrName)
deriving instance Show (ImportDecl RdrName)
deriving instance Show (InstDecl RdrName)
deriving instance Show (LHsTyVarBndrs RdrName)
deriving instance Show (Match RdrName (LHsCmd RdrName))
deriving instance Show (Match RdrName (LHsExpr RdrName))
deriving instance Show (MatchGroup RdrName (LHsCmd RdrName))
deriving instance Show (MatchGroup RdrName (LHsExpr RdrName))
deriving instance Show (ParStmtBlock RdrName RdrName)
deriving instance Show (Pat RdrName)
deriving instance Show (PatSynBind RdrName RdrName)
deriving instance Show (PendingSplice RdrName)
deriving instance Show (ResType (LHsType RdrName))
deriving instance Show (RoleAnnotDecl RdrName)
deriving instance Show (RuleBndr RdrName)
deriving instance Show (RuleDecl RdrName)
deriving instance Show (RuleDecls RdrName)
deriving instance Show (Sig RdrName)
deriving instance Show (SpliceDecl RdrName)
deriving instance Show (StmtLR RdrName RdrName (LHsCmd RdrName))
deriving instance Show (StmtLR RdrName RdrName (LHsExpr RdrName))
deriving instance Show (TyClDecl RdrName)
deriving instance Show (TyClGroup RdrName)
deriving instance Show (TyFamEqn RdrName (HsTyPats RdrName))
deriving instance Show (TyFamEqn RdrName (LHsTyVarBndrs RdrName))
deriving instance Show (TyFamInstDecl RdrName)
deriving instance Show (VectDecl RdrName)
deriving instance Show (WarnDecl RdrName)
deriving instance Show (WarnDecls RdrName)


deriving instance Show (ABExport Name)
deriving instance Show (AnnDecl Name)
deriving instance Show (AnnProvenance Name)
deriving instance Show (ArithSeqInfo Name)
deriving instance Show (BooleanFormula (Located Name))
deriving instance Show (ClsInstDecl Name)
deriving instance Show (ConDecl Name)
deriving instance Show (ConDeclField Name)
deriving instance Show (DataFamInstDecl Name)
deriving instance Show (DefaultDecl Name)
deriving instance Show (DerivDecl Name)
deriving instance Show (FamilyDecl Name)
deriving instance Show (FamilyInfo Name)
deriving instance Show (FixitySig Name)
deriving instance Show (ForeignDecl Name)
deriving instance Show (GRHS Name (LHsCmd Name))
deriving instance Show (GRHS Name (LHsExpr Name))
deriving instance Show (GRHSs Name (LHsCmd Name))
deriving instance Show (GRHSs Name (LHsExpr Name))
deriving instance Show (HsBindLR Name Name)
deriving instance Show (HsBracket Name)
deriving instance Show (HsCmd Name)
deriving instance Show (HsCmdTop Name)
deriving instance Show (HsConDeclDetails Name)
deriving instance Show (HsConPatDetails Name)
deriving instance Show (HsDataDefn Name)
deriving instance Show (HsDecl Name)
deriving instance Show (HsExpr Name)
deriving instance Show (HsGroup Name)
deriving instance Show (HsLocalBindsLR Name Name)
deriving instance Show (HsMatchContext Name)
deriving instance Show (HsModule Name)
deriving instance Show (HsOverLit Name)
deriving instance Show (HsPatSynDetails (Located Name))
deriving instance Show (HsPatSynDir Name)
deriving instance Show (HsQuasiQuote Name)
deriving instance Show (HsRecField Name (LHsExpr Name))
deriving instance Show (HsRecField Name (LPat Name))
deriving instance Show (HsRecFields Name (LPat Name))
deriving instance Show (HsRecordBinds Name)
deriving instance Show (HsSplice Name)
deriving instance Show (HsStmtContext Name)
deriving instance Show (HsTupArg Name)
deriving instance Show (HsTyPats Name)
deriving instance Show (HsTyVarBndr Name)
deriving instance Show (HsType Name)
deriving instance Show (HsValBindsLR Name Name)
deriving instance Show (HsWithBndrs Name (LHsType Name))
deriving instance Show (IE Name)
deriving instance Show (ImportDecl Name)
deriving instance Show (InstDecl Name)
deriving instance Show (LHsTyVarBndrs Name)
deriving instance Show (Match Name (LHsCmd Name))
deriving instance Show (Match Name (LHsExpr Name))
deriving instance Show (MatchGroup Name (LHsCmd Name))
deriving instance Show (MatchGroup Name (LHsExpr Name))
deriving instance Show (ParStmtBlock Name Name)
deriving instance Show (Pat Name)
deriving instance Show (PatSynBind Name Name)
deriving instance Show (PendingSplice Name)
deriving instance Show (ResType (LHsType Name))
deriving instance Show (RoleAnnotDecl Name)
deriving instance Show (RuleBndr Name)
deriving instance Show (RuleDecl Name)
deriving instance Show (RuleDecls Name)
deriving instance Show (Sig Name)
deriving instance Show (SpliceDecl Name)
deriving instance Show (StmtLR Name Name (LHsCmd Name))
deriving instance Show (StmtLR Name Name (LHsExpr Name))
deriving instance Show (TyClDecl Name)
deriving instance Show (TyClGroup Name)
deriving instance Show (TyFamEqn Name (HsTyPats Name))
deriving instance Show (TyFamEqn Name (LHsTyVarBndrs Name))
deriving instance Show (TyFamInstDecl Name)
deriving instance Show (VectDecl Name)
deriving instance Show (WarnDecl Name)
deriving instance Show (WarnDecls Name)

deriving instance Show (ABExport Id)
deriving instance Show (AnnDecl Id)
deriving instance Show (AnnProvenance Id)
deriving instance Show (ArithSeqInfo Id)
deriving instance Show (BooleanFormula (Located Id))
deriving instance Show (ClsInstDecl Id)
deriving instance Show (ConDecl Id)
deriving instance Show (ConDeclField Id)
deriving instance Show (DataFamInstDecl Id)
deriving instance Show (DefaultDecl Id)
deriving instance Show (DerivDecl Id)
deriving instance Show (FamilyDecl Id)
deriving instance Show (FamilyInfo Id)
deriving instance Show (FixitySig Id)
deriving instance Show (ForeignDecl Id)
deriving instance Show (GRHS Id (LHsCmd Id))
deriving instance Show (GRHS Id (LHsExpr Id))
deriving instance Show (GRHSs Id (LHsCmd Id))
deriving instance Show (GRHSs Id (LHsExpr Id))
deriving instance Show (HsBindLR Id Id)
deriving instance Show (HsBracket Id)
deriving instance Show (HsCmd Id)
deriving instance Show (HsCmdTop Id)
deriving instance Show (HsConDeclDetails Id)
deriving instance Show (HsConPatDetails Id)
deriving instance Show (HsDataDefn Id)
deriving instance Show (HsDecl Id)
deriving instance Show (HsExpr Id)
deriving instance Show (HsGroup Id)
deriving instance Show (HsLocalBindsLR Id Id)
deriving instance Show (HsMatchContext Id)
deriving instance Show (HsModule Id)
deriving instance Show (HsOverLit Id)
deriving instance Show (HsPatSynDetails (Located Id))
deriving instance Show (HsPatSynDir Id)
deriving instance Show (HsQuasiQuote Id)
deriving instance Show (HsRecField Id (LHsExpr Id))
deriving instance Show (HsRecField Id (LPat Id))
deriving instance Show (HsRecFields Id (LPat Id))
deriving instance Show (HsRecordBinds Id)
deriving instance Show (HsSplice Id)
deriving instance Show (HsStmtContext Id)
deriving instance Show (HsTupArg Id)
deriving instance Show (HsTyPats Id)
deriving instance Show (HsTyVarBndr Id)
deriving instance Show (HsType Id)
deriving instance Show (HsValBindsLR Id Id)
deriving instance Show (HsWithBndrs Id (LHsType Id))
deriving instance Show (IE Id)
deriving instance Show (ImportDecl Id)
deriving instance Show (InstDecl Id)
deriving instance Show (LHsTyVarBndrs Id)
deriving instance Show (Match Id (LHsCmd Id))
deriving instance Show (Match Id (LHsExpr Id))
deriving instance Show (MatchGroup Id (LHsCmd Id))
deriving instance Show (MatchGroup Id (LHsExpr Id))
deriving instance Show (ParStmtBlock Id Id)
deriving instance Show (Pat Id)
deriving instance Show (PatSynBind Id Id)
deriving instance Show (PendingSplice Id)
deriving instance Show (ResType (LHsType Id))
deriving instance Show (RoleAnnotDecl Id)
deriving instance Show (RuleBndr Id)
deriving instance Show (RuleDecl Id)
deriving instance Show (RuleDecls Id)
deriving instance Show (Sig Id)
deriving instance Show (SpliceDecl Id)
deriving instance Show (StmtLR Id Id (LHsCmd Id))
deriving instance Show (StmtLR Id Id (LHsExpr Id))
deriving instance Show (TyClDecl Id)
deriving instance Show (TyClGroup Id)
deriving instance Show (TyFamEqn Id (HsTyPats Id))
deriving instance Show (TyFamEqn Id (LHsTyVarBndrs Id))
deriving instance Show (TyFamInstDecl Id)
deriving instance Show (VectDecl Id)
deriving instance Show (WarnDecl Id)
deriving instance Show (WarnDecls Id)

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
deriving instance Show HsExplicitFlag
deriving instance Show HsIPName
deriving instance Show HsLit
deriving instance Show HsTupleSort
deriving instance Show HsTyWrapper
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
instance Show Coercion where
  show = showSDocUnsafe . ppr
instance Show PackageKey where
  show = showSDocUnsafe . ppr
instance Show TcCoercion where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (UniqFM a) where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (Tickish a) where
  show = showSDocUnsafe . ppr
instance OutputableBndr a => Show (HsIPBinds a) where
  show = showSDocUnsafe . ppr
  
instance Show a => Show (Bag a) where
  show = show . bagToList


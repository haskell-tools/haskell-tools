{-# LANGUAGE StandaloneDeriving
           , TypeSynonymInstances 
           , FlexibleInstances 
           #-}
-- | A module for showing GHC's syntax tree representation.
module Language.Haskell.Tools.Refactor.DebugGhcAST where

import Language.Haskell.Tools.Refactor.RangeDebug
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST (shortShowSpan)

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
import FieldLabel
import CoreSyn
import UniqFM
import OccName

instance Show a => Show (Located a) where
  show (L l a) = "L(" ++ shortShowSpan l ++ ") (" ++ show a ++ ")"

deriving instance Show (ABExport RdrName)
deriving instance Show (AmbiguousFieldOcc RdrName)
deriving instance Show (AnnDecl RdrName)
deriving instance Show (AnnProvenance RdrName)
deriving instance Show (ApplicativeArg RdrName RdrName)
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
deriving instance Show (FamilyResultSig RdrName)
deriving instance Show (FieldLbl RdrName)
deriving instance Show (FieldOcc RdrName)
deriving instance Show (FixitySig RdrName)
deriving instance Show (ForeignDecl RdrName)
deriving instance Show a => Show (GRHS RdrName a)
deriving instance Show a => Show (GRHSs RdrName a)
deriving instance Show (InjectivityAnn RdrName)
deriving instance Show (HsAppType RdrName)
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
deriving instance Show (HsRecFields RdrName (LPat RdrName))
deriving instance Show (HsRecordBinds RdrName)
deriving instance Show (HsSplice RdrName)
deriving instance Show (HsStmtContext RdrName)
deriving instance Show (HsTupArg RdrName)
deriving instance Show (HsTyVarBndr RdrName)
deriving instance Show (HsType RdrName)
deriving instance Show (HsValBindsLR RdrName RdrName)
deriving instance Show (HsWildCardInfo RdrName)
deriving instance Show (IE RdrName)
deriving instance Show (ImportDecl RdrName)
deriving instance Show (InstDecl RdrName)
deriving instance Show (LHsQTyVars RdrName)
deriving instance Show a => Show (Match RdrName a)
deriving instance Show (MatchFixity RdrName)
deriving instance Show a => Show (MatchGroup RdrName a)
deriving instance Show (ParStmtBlock RdrName RdrName)
deriving instance Show (Pat RdrName)
deriving instance Show (PatSynBind RdrName RdrName)
deriving instance Show (RecordPatSynField (Located RdrName))
deriving instance Show (RoleAnnotDecl RdrName)
deriving instance Show (RuleBndr RdrName)
deriving instance Show (RuleDecl RdrName)
deriving instance Show (RuleDecls RdrName)
deriving instance Show (Sig RdrName)
deriving instance Show (SpliceDecl RdrName)
deriving instance Show (SyntaxExpr RdrName)
deriving instance Show a => Show (StmtLR RdrName RdrName a)
deriving instance Show (TyClDecl RdrName)
deriving instance Show (TyClGroup RdrName)
deriving instance Show a => Show (TyFamEqn RdrName a)
deriving instance Show (TyFamInstDecl RdrName)
deriving instance Show (VectDecl RdrName)
deriving instance Show (WarnDecl RdrName)
deriving instance Show (WarnDecls RdrName)


deriving instance Show (ABExport Name)
deriving instance Show (AmbiguousFieldOcc Name)
deriving instance Show (AnnDecl Name)
deriving instance Show (AnnProvenance Name)
deriving instance Show (ApplicativeArg Name Name)
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
deriving instance Show (FamilyResultSig Name)
deriving instance Show (FieldLbl Name)
deriving instance Show (FieldOcc Name)
deriving instance Show (FixitySig Name)
deriving instance Show (ForeignDecl Name)
deriving instance Show a => Show (GRHS Name a)
deriving instance Show a => Show (GRHSs Name a)
deriving instance Show (InjectivityAnn Name)
deriving instance Show (HsAppType Name)
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
deriving instance Show (HsRecFields Name (LPat Name))
deriving instance Show (HsRecordBinds Name)
deriving instance Show (HsSplice Name)
deriving instance Show (HsStmtContext Name)
deriving instance Show (HsTupArg Name)
deriving instance Show (HsTyVarBndr Name)
deriving instance Show (HsType Name)
deriving instance Show (HsValBindsLR Name Name)
deriving instance Show (HsWildCardInfo Name)
deriving instance Show (IE Name)
deriving instance Show (ImportDecl Name)
deriving instance Show (InstDecl Name)
deriving instance Show (LHsQTyVars Name)
deriving instance Show a => Show (Match Name a)
deriving instance Show (MatchFixity Name)
deriving instance Show a => Show (MatchGroup Name a)
deriving instance Show (ParStmtBlock Name Name)
deriving instance Show (Pat Name)
deriving instance Show (PatSynBind Name Name)
deriving instance Show (RecordPatSynField (Located Name))
deriving instance Show (RoleAnnotDecl Name)
deriving instance Show (RuleBndr Name)
deriving instance Show (RuleDecl Name)
deriving instance Show (RuleDecls Name)
deriving instance Show (Sig Name)
deriving instance Show (SpliceDecl Name)
deriving instance Show (SyntaxExpr Name)
deriving instance Show a => Show (StmtLR Name Name a)
deriving instance Show (TyClDecl Name)
deriving instance Show (TyClGroup Name)
deriving instance Show a => Show (TyFamEqn Name a)
deriving instance Show (TyFamInstDecl Name)
deriving instance Show (VectDecl Name)
deriving instance Show (WarnDecl Name)
deriving instance Show (WarnDecls Name)


deriving instance Show (ABExport Id)
deriving instance Show (AmbiguousFieldOcc Id)
deriving instance Show (AnnDecl Id)
deriving instance Show (AnnProvenance Id)
deriving instance Show (ApplicativeArg Id Id)
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
deriving instance Show (FamilyResultSig Id)
deriving instance Show (FieldLbl Id)
deriving instance Show (FieldOcc Id)
deriving instance Show (FixitySig Id)
deriving instance Show (ForeignDecl Id)
deriving instance Show a => Show (GRHS Id a)
deriving instance Show a => Show (GRHSs Id a)
deriving instance Show (InjectivityAnn Id)
deriving instance Show (HsAppType Id)
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
deriving instance Show (HsRecFields Id (LPat Id))
deriving instance Show (HsRecordBinds Id)
deriving instance Show (HsSplice Id)
deriving instance Show (HsStmtContext Id)
deriving instance Show (HsTupArg Id)
deriving instance Show (HsTyVarBndr Id)
deriving instance Show (HsType Id)
deriving instance Show (HsValBindsLR Id Id)
deriving instance Show (HsWildCardInfo Id)
deriving instance Show (IE Id)
deriving instance Show (ImportDecl Id)
deriving instance Show (InstDecl Id)
deriving instance Show (LHsQTyVars Id)
deriving instance Show a => Show (Match Id a)
deriving instance Show (MatchFixity Id)
deriving instance Show a => Show (MatchGroup Id a)
deriving instance Show (ParStmtBlock Id Id)
deriving instance Show (Pat Id)
deriving instance Show (PatSynBind Id Id)
deriving instance Show (RecordPatSynField (Located Id))
deriving instance Show (RoleAnnotDecl Id)
deriving instance Show (RuleBndr Id)
deriving instance Show (RuleDecl Id)
deriving instance Show (RuleDecls Id)
deriving instance Show (Sig Id)
deriving instance Show (SpliceDecl Id)
deriving instance Show (SyntaxExpr Id)
deriving instance Show a => Show (StmtLR Id Id a)
deriving instance Show (TyClDecl Id)
deriving instance Show (TyClGroup Id)
deriving instance Show a => Show (TyFamEqn Id a)
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
deriving instance Show HsIPName
deriving instance Show HsLit
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

deriving instance Show t => Show (HsImplicitBndrs RdrName t)
deriving instance Show t => Show (HsImplicitBndrs Name t)
deriving instance Show t => Show (HsImplicitBndrs Id t)
deriving instance Show t => Show (HsWildCardBndrs RdrName t)
deriving instance Show t => Show (HsWildCardBndrs Name t)
deriving instance Show t => Show (HsWildCardBndrs Id t)
deriving instance (Show a, Show b) => Show (HsRecField' a b)


instance Show UnitId where
  show = showSDocUnsafe . ppr
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
instance Outputable a => Show (UniqFM a) where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (Tickish a) where
  show = showSDocUnsafe . ppr
instance OutputableBndr a => Show (HsIPBinds a) where
  show = showSDocUnsafe . ppr
  
instance Show a => Show (Bag a) where
  show = show . bagToList


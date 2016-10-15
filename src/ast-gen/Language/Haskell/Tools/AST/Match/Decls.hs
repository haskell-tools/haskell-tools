-- | Pattern matching on declaration-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Decls where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Match.Base

-- * Declarations

pattern TypeDecl :: Ann DeclHead dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage 
pattern TypeDecl dh typ <- Ann _ (UTypeDecl dh typ)

pattern TypeFamily :: Ann DeclHead dom SrcTemplateStage -> AnnMaybe TypeFamilySpec dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern TypeFamily dh famSpec <- Ann _ (UTypeFamilyDecl (Ann _ (UTypeFamily dh famSpec)))

pattern DataFamily :: Ann DeclHead dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern DataFamily dh kind <- Ann _ (UTypeFamilyDecl (Ann _ (UDataFamily dh kind)))

pattern ClosedTypeFamily :: Ann DeclHead dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> AnnList TypeEqn dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern ClosedTypeFamily dh kind typeqs <- Ann _ (UClosedTypeFamilyDecl dh kind typeqs)

pattern DataDecl :: AnnMaybe Context dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern DataDecl ctx dh cons derivs <- Ann _ (UDataDecl DataKeyword ctx dh cons derivs)

pattern NewtypeDecl :: AnnMaybe Context dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern NewtypeDecl ctx dh cons derivs <- Ann _ (UDataDecl NewtypeKeyword ctx dh cons derivs)

pattern GADTDataDecl :: AnnMaybe Context dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> AnnList GadtConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern GADTDataDecl ctx dh kind cons derivs  <- Ann _ (UGDataDecl DataKeyword ctx dh kind cons derivs )

pattern GADTNewtypeDecl :: AnnMaybe Context dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> AnnList GadtConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern GADTNewtypeDecl ctx dh kind cons derivs  <- Ann _ (UGDataDecl NewtypeKeyword ctx dh kind cons derivs )

pattern TypeInstance :: Ann InstanceRule dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern TypeInstance instRule typ <- Ann _ (UTypeInstDecl instRule typ)

pattern DataInstance :: Ann InstanceRule dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage
                    -> Ann Decl dom SrcTemplateStage
pattern DataInstance instRule cons derivs  <- Ann _ (UDataInstDecl DataKeyword instRule cons derivs )

pattern NewtypeInstance :: Ann InstanceRule dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage
                       -> Ann Decl dom SrcTemplateStage
pattern NewtypeInstance instRule cons derivs  <- Ann _ (UDataInstDecl NewtypeKeyword instRule cons derivs )

pattern GadtDataInstance :: Ann InstanceRule dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> AnnList GadtConDecl dom SrcTemplateStage
                       -> Ann Decl dom SrcTemplateStage
pattern GadtDataInstance instRule kind cons  <- Ann _ (UGDataInstDecl DataKeyword instRule kind cons )

pattern ClassDecl :: AnnMaybe Context dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage -> AnnMaybe ClassBody dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern ClassDecl ctx dh body <- Ann _ (UClassDecl ctx dh _ body)

pattern InstanceDecl :: Ann InstanceRule dom SrcTemplateStage -> AnnMaybe InstBody dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern InstanceDecl instRule body <- Ann _ (UInstDecl _ instRule body)

pattern StandaloneDeriving :: Ann InstanceRule dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern StandaloneDeriving instRule <- Ann _ (UDerivDecl _ instRule)

pattern FixityDecl :: Ann FixitySignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern FixityDecl fixity <- Ann _ (UFixityDecl fixity)

pattern TypeSigDecl :: Ann TypeSignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern TypeSigDecl typeSig <- Ann _ (UTypeSigDecl typeSig)

pattern ValueBinding :: Ann ValueBind dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern ValueBinding bind <- Ann _ (UValueBinding bind)

pattern ForeignImport :: Ann CallConv dom SrcTemplateStage -> Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern ForeignImport cc name typ <- Ann _ (UForeignImport cc _ name typ)

pattern PatternSynonym :: Ann PatSynLhs dom SrcTemplateStage -> Ann PatSynRhs dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
pattern PatternSynonym lhs rhs <- Ann _ (UPatternSynonymDecl (Ann _ (UPatternSynonym lhs rhs)))

-- * Pattern synonyms

pattern ConPatSyn :: Ann Name dom SrcTemplateStage -> AnnList Name dom SrcTemplateStage -> Ann PatSynLhs dom SrcTemplateStage
pattern ConPatSyn con args <- Ann _ (UNormalPatSyn con args)

pattern InfixPatSyn :: Ann Name dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Name dom SrcTemplateStage -> Ann PatSynLhs dom SrcTemplateStage
pattern InfixPatSyn lhs op rhs <- Ann _ (UInfixPatSyn lhs op rhs)

pattern RecordPatSyn :: Ann Name dom SrcTemplateStage -> AnnList Name dom SrcTemplateStage -> Ann PatSynLhs dom SrcTemplateStage
pattern RecordPatSyn con args <- Ann _ (URecordPatSyn con args)

pattern SymmetricPatSyn :: Ann Pattern dom SrcTemplateStage -> Ann PatSynRhs dom SrcTemplateStage
pattern SymmetricPatSyn pat <- Ann _ (UBidirectionalPatSyn pat AnnNothing)

pattern OneWayPatSyn :: Ann Pattern dom SrcTemplateStage -> Ann PatSynRhs dom SrcTemplateStage
pattern OneWayPatSyn pat <- Ann _ (UOneDirectionalPatSyn pat)

pattern TwoWayPatSyn :: Ann Pattern dom SrcTemplateStage -> AnnList Match dom stage -> Ann PatSynRhs dom SrcTemplateStage
pattern TwoWayPatSyn pat match <- Ann _ (UBidirectionalPatSyn pat (AnnJust (Ann _ (UPatSynWhere match))))

-- * Type families

pattern TypeFamilyKindSpec :: Ann KindConstraint dom SrcTemplateStage -> Ann TypeFamilySpec dom SrcTemplateStage
pattern TypeFamilyKindSpec kind <- Ann _ (UTypeFamilyKind kind)

pattern TypeFamilyInjectivitySpec :: Ann Name dom SrcTemplateStage -> AnnList Name dom SrcTemplateStage -> Ann TypeFamilySpec dom SrcTemplateStage
pattern TypeFamilyInjectivitySpec res dependent <- Ann _ (UTypeFamilyInjectivity (Ann _ (UInjectivityAnn res dependent)))

-- * Elements of type classes

pattern ClassBody :: AnnList ClassElement dom SrcTemplateStage -> Ann ClassBody dom SrcTemplateStage
pattern ClassBody body <- Ann _ (UClassBody body)

pattern ClassElemSig :: Ann TypeSignature dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
pattern ClassElemSig typeSig <- Ann _ (UClsSig typeSig)

pattern ClassElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
pattern ClassElemDef def <- Ann _ (UClsDef def)

pattern ClassElemTypeFam :: Ann DeclHead dom SrcTemplateStage -> AnnMaybe TypeFamilySpec dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
pattern ClassElemTypeFam dh tfSpec <- Ann _ (UClsTypeFam (Ann _ (UTypeFamily dh tfSpec)))

pattern ClassElemDataFam :: Ann DeclHead dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
pattern ClassElemDataFam dh kind <- Ann _ (UClsTypeFam (Ann _ (UDataFamily dh kind)))

-- * Declaration heads

pattern NameDeclHead :: Ann Name dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
pattern NameDeclHead name <- Ann _ (UDeclHead name)

pattern ParenDeclHead :: Ann DeclHead dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
pattern ParenDeclHead dh <- Ann _ (UDHParen dh)

pattern DeclHeadApp :: Ann DeclHead dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
pattern DeclHeadApp dh tv <- Ann _ (UDHApp dh tv)

pattern InfixDeclHead :: Ann TyVar dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
pattern InfixDeclHead lhs op rhs <- Ann _ (UDHInfix lhs op rhs)

-- * Elements of class instances

pattern InstanceBody :: AnnList InstBodyDecl dom SrcTemplateStage -> Ann InstBody dom SrcTemplateStage
pattern InstanceBody defs <- Ann _ (UInstBody defs)

pattern InstanceElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
pattern InstanceElemDef bind <- Ann _ (UInstBodyNormalDecl bind)

pattern InstanceElemTypeDef :: Ann TypeEqn dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
pattern InstanceElemTypeDef typeEq <- Ann _ (UInstBodyTypeDecl typeEq)

pattern InstanceElemDataDef :: Ann InstanceRule dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage 
                           -> Ann InstBodyDecl dom SrcTemplateStage
pattern InstanceElemDataDef instRule cons derivs  <- Ann _ (UInstBodyDataDecl DataKeyword instRule cons derivs )

pattern InstanceElemNewtypeDef :: Ann InstanceRule dom SrcTemplateStage -> AnnList ConDecl dom SrcTemplateStage -> AnnMaybe Deriving dom SrcTemplateStage 
                           -> Ann InstBodyDecl dom SrcTemplateStage
pattern InstanceElemNewtypeDef instRule cons derivs  <- Ann _ (UInstBodyDataDecl NewtypeKeyword instRule cons derivs )

pattern InstanceElemGadtDataDef :: Ann InstanceRule dom SrcTemplateStage -> AnnMaybe KindConstraint dom SrcTemplateStage -> AnnList GadtConDecl dom SrcTemplateStage 
                               -> AnnMaybe Deriving dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
pattern InstanceElemGadtDataDef instRule kind cons derivs  <- Ann _ (UInstBodyGadtDataDecl _ instRule kind cons derivs )

-- * Data type definitions

pattern GadtConDecl :: AnnList Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann GadtConDecl dom SrcTemplateStage
pattern GadtConDecl names typ <- Ann _ (UGadtConDecl names (Ann _ (UGadtNormalType typ)))

pattern ConDecl :: Ann Name dom SrcTemplateStage -> AnnList Type dom SrcTemplateStage -> Ann ConDecl dom SrcTemplateStage
pattern ConDecl name args <- Ann _ (UConDecl name args)

pattern RecordConDecl :: Ann Name dom SrcTemplateStage -> AnnList FieldDecl dom SrcTemplateStage -> Ann ConDecl dom SrcTemplateStage
pattern RecordConDecl name fields <- Ann _ (URecordDecl name fields)

pattern InfixConDecl :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann ConDecl dom SrcTemplateStage
pattern InfixConDecl lhs op rhs <- Ann _ (UInfixConDecl lhs op rhs)

pattern FieldDecl :: AnnList Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann FieldDecl dom SrcTemplateStage
pattern FieldDecl names typ <- Ann _ (UFieldDecl names typ)

pattern DerivingOne :: Ann InstanceHead dom SrcTemplateStage -> Ann Deriving dom SrcTemplateStage
pattern DerivingOne deriv <- Ann _ (UDerivingOne deriv)

pattern DerivingMulti :: AnnList InstanceHead dom SrcTemplateStage -> Ann Deriving dom SrcTemplateStage
pattern DerivingMulti derivs <- Ann _ (UDerivings derivs)

pattern InstanceRule :: AnnMaybe (AnnList TyVar) dom stage -> AnnMaybe Context dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage -> Ann InstanceRule dom SrcTemplateStage
pattern InstanceRule tvs ctx ih <- Ann _ (UInstanceRule tvs ctx ih)

pattern InstanceHead :: Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
pattern InstanceHead name <- Ann _ (UInstanceHeadCon name)

pattern InfixInstanceHead :: Ann Type dom SrcTemplateStage -> Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
pattern InfixInstanceHead typ n <- Ann _ (UInstanceHeadInfix typ n)

pattern ParenInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
pattern ParenInstanceHead ih <- Ann _ (UInstanceHeadParen ih)

pattern AppInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
pattern AppInstanceHead fun arg <- Ann _ (UInstanceHeadApp fun arg)

pattern TypeEqn :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann TypeEqn dom SrcTemplateStage
pattern TypeEqn lhs rhs <- Ann _ (UTypeEqn lhs rhs)

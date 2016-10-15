-- | Generation of declaration-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTypeSignature@ creates the annotated version of the @TypeSignature@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Decls where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkTypeDecl :: Ann DeclHead dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage 
mkTypeDecl dh typ = mkAnn (child <> " :: " <> child) $ UTypeDecl dh typ

mkTypeFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann TypeFamilySpec dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkTypeFamily dh famSpec = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe (optBefore " ") famSpec))

mkDataFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkDataFamily dh kind = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe (optBefore " ") kind))

mkClosedTypeFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann TypeEqn dom SrcTemplateStage]
                       -> Ann Decl dom SrcTemplateStage
mkClosedTypeFamily dh kind typeqs = mkAnn (child <> child <> " where " <> child) 
                                      $ UClosedTypeFamilyDecl dh (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList typeqs)

mkDataDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage
                -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkDataDecl ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child) 
      $ UDataDecl mkDataKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage
                   -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkNewtypeDecl ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UDataDecl mkNewtypeKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGADTDataDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage)
                    -> [Ann GadtConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkGADTDataDecl ctx dh kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UGDataDecl mkDataKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                  (mkAnnMaybe (optBefore " ") kind) (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkTypeInstance :: Ann InstanceRule dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkTypeInstance instRule typ = mkAnn ("type instance " <> child <> " = " <> child) $ UTypeInstDecl instRule typ

mkDataInstance :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage)
                    -> Ann Decl dom SrcTemplateStage
mkDataInstance instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl mkDataKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeInstance :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage)
                       -> Ann Decl dom SrcTemplateStage
mkNewtypeInstance instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl mkNewtypeKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtDataInstance :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann GadtConDecl dom SrcTemplateStage]
                       -> Ann Decl dom SrcTemplateStage
mkGadtDataInstance instRule kind cons 
  = mkAnn (child <> " instance " <> child <> child <> " where " <> child) 
      $ UGDataInstDecl mkDataKeyword instRule (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList cons)

mkClassDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage -> Maybe (Ann ClassBody dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkClassDecl ctx dh body = mkAnn ("class " <> child <> child <> child <> child) 
                            $ UClassDecl (mkAnnMaybe (optAfter " ") ctx) dh (mkAnnMaybe (optBefore " | ") Nothing) (mkAnnMaybe opt body) 

mkInstanceDecl :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann InstBody dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkInstanceDecl instRule body = mkAnn ("instance " <> child <> child <> child) 
                                 $ UInstDecl (mkAnnMaybe (optBefore " ") Nothing) instRule (mkAnnMaybe opt body)

mkStandaloneDeriving :: Ann InstanceRule dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkStandaloneDeriving instRule = mkAnn ("deriving instance" <> child <> child) $ UDerivDecl (mkAnnMaybe (optBefore " ") Nothing) instRule

mkFixityDecl :: Ann FixitySignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkFixityDecl = mkAnn child . UFixityDecl

mkTypeSigDecl :: Ann TypeSignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkTypeSigDecl = mkAnn child . UTypeSigDecl

mkValueBinding :: Ann ValueBind dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkValueBinding = mkAnn child . UValueBinding

mkTypeFamilyKindSpec :: Ann KindConstraint dom SrcTemplateStage -> Ann TypeFamilySpec dom SrcTemplateStage
mkTypeFamilyKindSpec = mkAnn child . UTypeFamilyKind

mkTypeFamilyInjectivitySpec :: Ann Name dom SrcTemplateStage -> [Ann Name dom SrcTemplateStage] -> Ann TypeFamilySpec dom SrcTemplateStage
mkTypeFamilyInjectivitySpec res dependent 
  = mkAnn child (UTypeFamilyInjectivity $ mkAnn (child <> " -> " <> child) $ UInjectivityAnn res (mkAnnList (listSep " ") dependent))

mkClassBody :: [Ann ClassElement dom SrcTemplateStage] -> Ann ClassBody dom SrcTemplateStage
mkClassBody = mkAnn (" where " <> child) . UClassBody . mkAnnList indentedList

mkClassElemSig :: Ann TypeSignature dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
mkClassElemSig = mkAnn child . UClsSig

mkClassElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
mkClassElemDef = mkAnn child . UClsDef

mkClassElemTypeFam :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann TypeFamilySpec dom SrcTemplateStage) -> Ann ClassElement dom SrcTemplateStage
mkClassElemTypeFam dh tfSpec = mkAnn ("type " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe opt tfSpec))

mkClassElemDataFam :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> Ann ClassElement dom SrcTemplateStage
mkClassElemDataFam dh kind = mkAnn ("data " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe opt kind))

mkNameDeclHead :: Ann Name dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkNameDeclHead = mkAnn child . UDeclHead

mkParenDeclHead :: Ann DeclHead dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkParenDeclHead = mkAnn child . UDHParen

mkDeclHeadApp :: Ann DeclHead dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkDeclHeadApp dh tv = mkAnn (child <> " " <> child) $ UDHApp dh tv

mkInfixDeclHead :: Ann TyVar dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkInfixDeclHead lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UDHInfix lhs op rhs

mkInstanceBody :: [Ann InstBodyDecl dom SrcTemplateStage] -> Ann InstBody dom SrcTemplateStage
mkInstanceBody = mkAnn (" where " <> child) . UInstBody . mkAnnList indentedList

mkInstanceElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemDef = mkAnn child . UInstBodyNormalDecl

mkInstanceElemTypeDef :: Ann TypeEqn dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemTypeDef = mkAnn child . UInstBodyTypeDecl

mkInstanceElemDataDef :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) 
                           -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemDataDef instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl mkDataKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemNewtypeDef :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) 
                           -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemNewtypeDef instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl mkNewtypeKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemGadtDataDef :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann GadtConDecl dom SrcTemplateStage] 
                               -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemGadtDataDef instRule kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyGadtDataDecl mkDataKeyword instRule (mkAnnMaybe opt kind) (mkAnnList (listSepBefore " | " " = ") cons) 
                             (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtConDecl :: [Ann Name dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage -> Ann GadtConDecl dom SrcTemplateStage
mkGadtConDecl names typ = mkAnn (child <> " :: " <> child) $ UGadtConDecl (mkAnnList (listSep ", ") names) (mkAnn child $ UGadtNormalType typ)

mkConDecl :: Ann Name dom SrcTemplateStage -> [Ann Type dom SrcTemplateStage] -> Ann ConDecl dom SrcTemplateStage
mkConDecl name args = mkAnn (child <> child) $ UConDecl name (mkAnnList (listSepBefore " " " ") args)

mkRecordConDecl :: Ann Name dom SrcTemplateStage -> [Ann FieldDecl dom SrcTemplateStage] -> Ann ConDecl dom SrcTemplateStage
mkRecordConDecl name fields = mkAnn (child <> " { " <> child <> " }") $ URecordDecl name (mkAnnList (listSep ", ") fields)

mkInfixConDecl :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann ConDecl dom SrcTemplateStage
mkInfixConDecl lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixConDecl lhs op rhs

mkFieldDecl :: [Ann Name dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage -> Ann FieldDecl dom SrcTemplateStage
mkFieldDecl names typ = mkAnn (child <> " :: " <> child) $ UFieldDecl (mkAnnList (listSep ", ") names) typ

mkDeriving :: [Ann InstanceHead dom SrcTemplateStage] -> Ann Deriving dom SrcTemplateStage
mkDeriving [deriv] = mkAnn child $ UDerivingOne deriv
mkDeriving derivs = mkAnn ("(" <> child <> ")") $ UDerivings (mkAnnList (listSep ", ") derivs)

mkInstanceRule :: Maybe (Ann Context dom SrcTemplateStage) -> Ann InstanceHead dom SrcTemplateStage -> Ann InstanceRule dom SrcTemplateStage
mkInstanceRule ctx ih 
  = mkAnn (child <> child <> child) $ UInstanceRule (mkAnnMaybe (optBefore " ") Nothing) (mkAnnMaybe (optBefore " ") ctx) ih

mkInstanceHead :: Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkInstanceHead = mkAnn child . UInstanceHeadCon

mkInfixInstanceHead :: Ann Type dom SrcTemplateStage -> Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkInfixInstanceHead typ n = mkAnn (child <> child) $ UInstanceHeadInfix typ n

mkParenInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkParenInstanceHead = mkAnn ("(" <> child <> ")") . UInstanceHeadParen

mkAppInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkAppInstanceHead fun arg = mkAnn (child <> " " <> child) $ UInstanceHeadApp fun arg

mkTypeEqn :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann TypeEqn dom SrcTemplateStage
mkTypeEqn lhs rhs = mkAnn (child <> " = " <> child) $ UTypeEqn lhs rhs

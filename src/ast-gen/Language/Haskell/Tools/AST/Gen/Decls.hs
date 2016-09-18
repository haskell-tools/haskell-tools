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
mkTypeDecl dh typ = mkAnn (child <> " :: " <> child) $ TypeDecl dh typ

mkTypeFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann TypeFamilySpec dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkTypeFamily dh famSpec = mkAnn child $ TypeFamilyDecl (mkAnn (child <> child) $ TypeFamily dh (mkAnnMaybe (optBefore " ") famSpec))

mkDataFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkDataFamily dh kind = mkAnn child $ TypeFamilyDecl (mkAnn (child <> child) $ DataFamily dh (mkAnnMaybe (optBefore " ") kind))

mkClosedTypeFamily :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann TypeEqn dom SrcTemplateStage]
                       -> Ann Decl dom SrcTemplateStage
mkClosedTypeFamily dh kind typeqs = mkAnn (child <> child <> " where " <> child) 
                                      $ ClosedTypeFamilyDecl dh (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList typeqs)

mkDataDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage
                -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkDataDecl ctx dh cons derivs 
  = mkAnn (child <> child <> child <> child <> child) 
      $ DataDecl (mkAnn "data " DataKeyword) (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage
                   -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkNewtypeDecl ctx dh cons derivs 
  = mkAnn (child <> child <> child <> child <> child <> child) 
      $ DataDecl (mkAnn "newtype " NewtypeKeyword) (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGADTDataDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage)
                    -> [Ann GadtConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkGADTDataDecl ctx dh kind cons derivs 
  = mkAnn (child <> child <> child <> child <> child <> child) 
      $ GDataDecl (mkAnn "data " DataKeyword) (mkAnnMaybe (optBefore " ") ctx) dh 
                  (mkAnnMaybe (optBefore " ") kind) (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkTypeInstance :: Ann InstanceRule dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkTypeInstance instRule typ = mkAnn ("type instance " <> child <> " = " <> child) $ TypeInstDecl instRule typ

mkDataInstance :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage)
                    -> Ann Decl dom SrcTemplateStage
mkDataInstance instRule cons derivs 
  = mkAnn ("data instance " <> child <> " = " <> child <> child) 
      $ DataInstDecl (mkAnn "data " DataKeyword) instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeInstance :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage)
                       -> Ann Decl dom SrcTemplateStage
mkNewtypeInstance instRule cons derivs 
  = mkAnn ("data instance " <> child <> " = " <> child <> child) 
      $ DataInstDecl (mkAnn "newtype " NewtypeKeyword) instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtDataInstance :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann GadtConDecl dom SrcTemplateStage]
                       -> Ann Decl dom SrcTemplateStage
mkGadtDataInstance instRule kind cons 
  = mkAnn ("data instance " <> child <> child <> " where " <> child) 
      $ GDataInstDecl (mkAnn "data " DataKeyword) instRule (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList cons)

mkClassDecl :: Maybe (Ann Context dom SrcTemplateStage) -> Ann DeclHead dom SrcTemplateStage -> Maybe (Ann ClassBody dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkClassDecl ctx dh body = mkAnn ("class " <> child <> child <> child <> child) 
                            $ ClassDecl (mkAnnMaybe (optAfter " ") ctx) dh (mkAnnMaybe (optBefore " | ") Nothing) (mkAnnMaybe opt body) 

mkInstanceDecl :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann InstBody dom SrcTemplateStage) -> Ann Decl dom SrcTemplateStage
mkInstanceDecl instRule body = mkAnn ("instance " <> child <> child <> child) 
                                 $ InstDecl (mkAnnMaybe (optBefore " ") Nothing) instRule (mkAnnMaybe opt body)

mkStandaloneDeriving :: Ann InstanceRule dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkStandaloneDeriving instRule = mkAnn ("deriving instance" <> child <> child) $ DerivDecl (mkAnnMaybe (optBefore " ") Nothing) instRule

mkFixityDecl :: Ann FixitySignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkFixityDecl = mkAnn child . FixityDecl

mkTypeSigDecl :: Ann TypeSignature dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkTypeSigDecl = mkAnn child . TypeSigDecl

mkValueBinding :: Ann ValueBind dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
mkValueBinding = mkAnn child . ValueBinding

mkTypeFamilyKindSpec :: Ann KindConstraint dom SrcTemplateStage -> Ann TypeFamilySpec dom SrcTemplateStage
mkTypeFamilyKindSpec = mkAnn child . TypeFamilyKind

mkTypeFamilyInjectivitySpec :: Ann Name dom SrcTemplateStage -> [Ann Name dom SrcTemplateStage] -> Ann TypeFamilySpec dom SrcTemplateStage
mkTypeFamilyInjectivitySpec res dependent 
  = mkAnn child (TypeFamilyInjectivity $ mkAnn (child <> " -> " <> child) $ InjectivityAnn res (mkAnnList (listSep " ") dependent))

mkClassBody :: [Ann ClassElement dom SrcTemplateStage] -> Ann ClassBody dom SrcTemplateStage
mkClassBody = mkAnn (" where " <> child) . ClassBody . mkAnnList indentedList

mkClassElemSig :: Ann TypeSignature dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
mkClassElemSig = mkAnn child . ClsSig

mkClassElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann ClassElement dom SrcTemplateStage
mkClassElemDef = mkAnn child . ClsDef

mkClassElemTypeFam :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann TypeFamilySpec dom SrcTemplateStage) -> Ann ClassElement dom SrcTemplateStage
mkClassElemTypeFam dh tfSpec = mkAnn ("type " <> child) $ ClsTypeFam (mkAnn (child <> child) $ TypeFamily dh (mkAnnMaybe opt tfSpec))

mkClassElemDataFam :: Ann DeclHead dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> Ann ClassElement dom SrcTemplateStage
mkClassElemDataFam dh kind = mkAnn ("data " <> child) $ ClsTypeFam (mkAnn (child <> child) $ DataFamily dh (mkAnnMaybe opt kind))

mkNameDeclHead :: Ann Name dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkNameDeclHead = mkAnn child . DeclHead

mkParenDeclHead :: Ann DeclHead dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkParenDeclHead = mkAnn child . DHParen

mkDeclHeadApp :: Ann DeclHead dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkDeclHeadApp dh tv = mkAnn (child <> " " <> child) $ DHApp dh tv

mkInfixDeclHead :: Ann TyVar dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage -> Ann DeclHead dom SrcTemplateStage
mkInfixDeclHead lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ DHInfix lhs op rhs

mkInstanceBody :: [Ann InstBodyDecl dom SrcTemplateStage] -> Ann InstBody dom SrcTemplateStage
mkInstanceBody = mkAnn (" where " <> child) . InstBody . mkAnnList indentedList

mkInstanceElemDef :: Ann ValueBind dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemDef = mkAnn child . InstBodyNormalDecl

mkInstanceElemTypeDef :: Ann TypeEqn dom SrcTemplateStage -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemTypeDef = mkAnn child . InstBodyTypeDecl

mkInstanceElemDataDef :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) 
                           -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemDataDef instRule cons derivs 
  = mkAnn (child <> child <> child <> child) 
      $ InstBodyDataDecl (mkAnn "data " DataKeyword) instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemNewtypeDef :: Ann InstanceRule dom SrcTemplateStage -> [Ann ConDecl dom SrcTemplateStage] -> Maybe (Ann Deriving dom SrcTemplateStage) 
                           -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemNewtypeDef instRule cons derivs 
  = mkAnn (child <> child <> child <> child) 
      $ InstBodyDataDecl (mkAnn "newtype " NewtypeKeyword) instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemGadtDataDef :: Ann InstanceRule dom SrcTemplateStage -> Maybe (Ann KindConstraint dom SrcTemplateStage) -> [Ann GadtConDecl dom SrcTemplateStage] 
                               -> Maybe (Ann Deriving dom SrcTemplateStage) -> Ann InstBodyDecl dom SrcTemplateStage
mkInstanceElemGadtDataDef instRule kind cons derivs 
  = mkAnn (child <> child <> child <> child) 
      $ InstBodyGadtDataDecl (mkAnn "data " DataKeyword) instRule (mkAnnMaybe opt kind) (mkAnnList (listSepBefore " | " " = ") cons) 
                             (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtConDecl :: [Ann Name dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage -> Ann GadtConDecl dom SrcTemplateStage
mkGadtConDecl names typ = mkAnn (child <> " :: " <> child) $ GadtConDecl (mkAnnList (listSep ", ") names) (mkAnn child $ GadtNormalType typ)

mkConDecl :: Ann Name dom SrcTemplateStage -> [Ann Type dom SrcTemplateStage] -> Ann ConDecl dom SrcTemplateStage
mkConDecl name args = mkAnn (child <> child) $ ConDecl name (mkAnnList (listSepBefore " " " ") args)

mkRecordConDecl :: Ann Name dom SrcTemplateStage -> [Ann FieldDecl dom SrcTemplateStage] -> Ann ConDecl dom SrcTemplateStage
mkRecordConDecl name fields = mkAnn (child <> " { " <> child <> " }") $ RecordDecl name (mkAnnList (listSep ", ") fields)

mkInfixConDecl :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann ConDecl dom SrcTemplateStage
mkInfixConDecl lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ InfixConDecl lhs op rhs

mkFieldDecl :: [Ann Name dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage -> Ann FieldDecl dom SrcTemplateStage
mkFieldDecl names typ = mkAnn (child <> " :: " <> child) $ FieldDecl (mkAnnList (listSep ", ") names) typ

mkDeriving :: [Ann InstanceHead dom SrcTemplateStage] -> Ann Deriving dom SrcTemplateStage
mkDeriving [deriv] = mkAnn child $ DerivingOne deriv
mkDeriving derivs = mkAnn ("(" <> child <> ")") $ Derivings (mkAnnList (listSep ", ") derivs)

mkInstanceRule :: Maybe (Ann Context dom SrcTemplateStage) -> Ann InstanceHead dom SrcTemplateStage -> Ann InstanceRule dom SrcTemplateStage
mkInstanceRule ctx ih 
  = mkAnn (child <> child <> child) $ InstanceRule (mkAnnMaybe (optBefore " ") Nothing) (mkAnnMaybe (optBefore " ") ctx) ih

mkInstanceHead :: Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkInstanceHead = mkAnn child . InstanceHeadCon

mkInfixInstanceHead :: Ann Type dom SrcTemplateStage -> Ann Name dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkInfixInstanceHead typ n = mkAnn (child <> child) $ InstanceHeadInfix typ n

mkParenInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkParenInstanceHead = mkAnn ("(" <> child <> ")") . InstanceHeadParen

mkAppInstanceHead :: Ann InstanceHead dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann InstanceHead dom SrcTemplateStage
mkAppInstanceHead fun arg = mkAnn (child <> " " <> child) $ InstanceHeadApp fun arg

mkTypeEqn :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann TypeEqn dom SrcTemplateStage
mkTypeEqn lhs rhs = mkAnn (child <> " = " <> child) $ TypeEqn lhs rhs

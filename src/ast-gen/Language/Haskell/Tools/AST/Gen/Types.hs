-- | Generation of type-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTyForall@ creates the annotated version of the @TyForall@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Types where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- * Generation of types

mkTyForall :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
           => AnnList TyVar dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyForall vars t = mkAnn ("forall " <> child <> " ." <> child <> " " <> child) (TyForall vars t)

mkTypeVarList :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo, SemanticInfo dom SimpleName ~ NoSemanticInfo) 
              => [GHC.Name] -> AnnList TyVar dom SrcTemplateStage
mkTypeVarList ls = mkAnnList (listSep " ") (map (mkTypeVar . mkUnqualName') ls)

mkTyCtx :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
        => Ann Context dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyCtx ctx t = mkAnn (child <> " " <> child) (TyCtx ctx t)

mkTyFun :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
        => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyFun at rt = mkAnn (child <> " -> " <> child) (TyFun at rt)

mkTyTuple :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
          => [Ann Type dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage
mkTyTuple args = mkAnn ("(" <> child <> ")") (TyTuple (mkAnnList (listSep ", ") args))

mkTyUnbTuple :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
             => [Ann Type dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage
mkTyUnbTuple args = mkAnn ("(#" <> child <> "#)") (TyUnbTuple (mkAnnList (listSep ", ") args))

mkTyList :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
         => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyList = mkAnn ("[" <> child <> "]") . TyList

mkTyParArray :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
             => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyParArray = mkAnn ("[:" <> child <> ":]") . TyParArray

mkTyApp :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
        => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyApp ft at = mkAnn (child <> " " <> child) (TyApp ft at)

mkTyInfix :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
          => Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyInfix left op right = mkAnn (child <> " " <> child <> " " <> child) (TyInfix left op right)
             
mkTyParen :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
          => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyParen = mkAnn ("(" <> child <> ")") . TyParen
           
mkTypeVar :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
          => Ann Name dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage
mkTypeVar n = mkAnn (child <> child) (TyVarDecl n noth)

mkTyVar :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
        => Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyVar = wrapperAnn . TyVar

mkTyKinded :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
           => Ann Type dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyKinded t k = mkAnn (child <> " :: " <> child) (TyKinded t k)

mkTyBang :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
         => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyBang = mkAnn ("!" <> child) . TyBang

mkTyUnpack :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
           => Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyUnpack = mkAnn ("{-# UNPACK #-} " <> child) . TyUnpack

mkTyWildcard :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
             => Ann Type dom SrcTemplateStage
mkTyWildcard = mkAnn "_" TyWildcard

mkTyNamedWildcard :: (Domain dom, SemanticInfo dom Type ~ NoSemanticInfo) 
                  => Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyNamedWildcard = mkAnn ("_" <> child) . TyNamedWildc

-- * Generation of contexts

mkContextOne :: (Domain dom, SemanticInfo dom Context ~ NoSemanticInfo) 
             => Ann Assertion dom SrcTemplateStage -> Ann Context dom SrcTemplateStage
mkContextOne = mkAnn (child <> " =>") . ContextOne

mkContextMulti :: (Domain dom, SemanticInfo dom Context ~ NoSemanticInfo) 
               => [Ann Assertion dom SrcTemplateStage] -> Ann Context dom SrcTemplateStage
mkContextMulti = mkAnn ("(" <> child <> ") =>") . ContextMulti . mkAnnList (listSep ", ")

-- * Generation of assertions

mkClassAssert :: (Domain dom, SemanticInfo dom Assertion ~ NoSemanticInfo) 
              => Ann Name dom SrcTemplateStage -> [Ann Type dom SrcTemplateStage] -> Ann Assertion dom SrcTemplateStage
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ ClassAssert n (mkAnnList (listSep " ") args)

mkInfixAssert :: (Domain dom, SemanticInfo dom Assertion ~ NoSemanticInfo) 
              => Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Assertion dom SrcTemplateStage
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ InfixAssert left op right

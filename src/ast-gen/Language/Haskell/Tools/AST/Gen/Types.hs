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

mkTyForall :: [Ann TyVar dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyForall vars t = mkAnn ("forall " <> child <> " . " <> child) (UTyForall (mkAnnList (listSep " ") vars) t)

mkTypeVar' :: GHC.Name -> Ann TyVar dom SrcTemplateStage
mkTypeVar' = mkTypeVar . mkUnqualName'

mkTyCtx :: Ann Context dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyCtx ctx t = mkAnn (child <> " " <> child) (UTyCtx ctx t)

mkTyFun :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyFun at rt = mkAnn (child <> " -> " <> child) (UTyFun at rt)

mkTyTuple :: [Ann Type dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage
mkTyTuple args = mkAnn ("(" <> child <> ")") (UTyTuple (mkAnnList (listSep ", ") args))

mkTyUnbTuple :: [Ann Type dom SrcTemplateStage] -> Ann Type dom SrcTemplateStage
mkTyUnbTuple args = mkAnn ("(#" <> child <> "#)") (UTyUnbTuple (mkAnnList (listSep ", ") args))

mkTyList :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyList = mkAnn ("[" <> child <> "]") . UTyList

mkTyParArray :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyParArray = mkAnn ("[:" <> child <> ":]") . UTyParArray

mkTyApp :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyApp ft at = mkAnn (child <> " " <> child) (UTyApp ft at)

mkTyInfix :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyInfix left op right = mkAnn (child <> " " <> child <> " " <> child) (UTyInfix left op right)
             
mkTyParen :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyParen = mkAnn ("(" <> child <> ")") . UTyParen
           
mkTypeVar :: Ann Name dom SrcTemplateStage -> Ann TyVar dom SrcTemplateStage
mkTypeVar n = mkAnn (child <> child) (UTyVarDecl n noth)

mkTyVar :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyVar = wrapperAnn . UTyVar

mkTyKinded :: Ann Type dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyKinded t k = mkAnn (child <> " :: " <> child) (UTyKinded t k)

mkTyBang :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyBang = mkAnn ("!" <> child) . UTyBang

mkTyLazy :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyLazy = mkAnn ("~" <> child) . UTyLazy

mkTyUnpack :: Ann Type dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyUnpack = mkAnn ("{-# UNPACK #-} " <> child) . UTyUnpack

mkTyWildcard :: Ann Type dom SrcTemplateStage
mkTyWildcard = mkAnn "_" UTyWildcard

mkTyNamedWildcard :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage
mkTyNamedWildcard = mkAnn ("_" <> child) . UTyNamedWildc

-- * Generation of contexts

mkContextOne :: Ann Assertion dom SrcTemplateStage -> Ann Context dom SrcTemplateStage
mkContextOne = mkAnn (child <> " =>") . UContextOne

mkContextMulti :: [Ann Assertion dom SrcTemplateStage] -> Ann Context dom SrcTemplateStage
mkContextMulti = mkAnn ("(" <> child <> ") =>") . UContextMulti . mkAnnList (listSep ", ")

-- * Generation of assertions

mkClassAssert :: Ann Name dom SrcTemplateStage -> [Ann Type dom SrcTemplateStage] -> Ann Assertion dom SrcTemplateStage
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ UClassAssert n (mkAnnList (listSep " ") args)

mkInfixAssert :: Ann Type dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Assertion dom SrcTemplateStage
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAssert left op right

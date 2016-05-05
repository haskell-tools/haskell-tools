-- | Generation of type-level AST fragments for refactorings
{-# LANGUAGE OverloadedStrings #-}
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

mkTyForall :: TemplateAnnot a => AnnList TyVar a -> AnnMaybe Context a -> Ann Type a -> Ann Type a
mkTyForall vars ctx t = mkAnn ("forall " <> child <> " ." <> child <> " " <> child) (TyForall vars ctx t)

mkTypeVarList :: TemplateAnnot a => [GHC.Name] -> AnnList TyVar a
mkTypeVarList ls = mkAnnList (listSep " ") (map (mkTypeVar . mkUnqualName') ls)

mkTyCtx :: TemplateAnnot a => Ann Context a -> Ann Type a -> Ann Type a
mkTyCtx ctx t = mkAnn (child <> " " <> child) (TyCtx ctx t)

mkTyFun :: TemplateAnnot a => Ann Type a -> Ann Type a -> Ann Type a
mkTyFun at rt = mkAnn (child <> " -> " <> child) (TyFun at rt)

mkTyTuple :: TemplateAnnot a => [Ann Type a] -> Ann Type a
mkTyTuple args = mkAnn ("(" <> child <> ")") (TyTuple (mkAnnList (listSep ", ") args))

mkTyUnbTuple :: TemplateAnnot a => [Ann Type a] -> Ann Type a
mkTyUnbTuple args = mkAnn ("(#" <> child <> "#)") (TyUnbTuple (mkAnnList (listSep ", ") args))

mkTyList :: TemplateAnnot a => Ann Type a -> Ann Type a
mkTyList = mkAnn ("[" <> child <> "]") . TyList

mkTyParArray :: TemplateAnnot a => Ann Type a -> Ann Type a
mkTyParArray = mkAnn ("[:" <> child <> ":]") . TyParArray

mkTyApp :: TemplateAnnot a => Ann Type a -> Ann Type a -> Ann Type a
mkTyApp ft at = mkAnn (child <> " " <> child) (TyApp ft at)

mkTyInfix :: TemplateAnnot a => Ann Type a -> Ann Operator a -> Ann Type a -> Ann Type a
mkTyInfix left op right = mkAnn (child <> " " <> child <> " " <> child) (TyInfix left op right)
             
mkTyParen :: TemplateAnnot a => Ann Type a -> Ann Type a
mkTyParen = mkAnn ("(" <> child <> ")") . TyParen
           
mkTypeVar :: TemplateAnnot a => Ann Name a -> Ann TyVar a
mkTypeVar n = mkAnn (child <> optBefore " ") (TyVarDecl n noth)

mkTyVar :: TemplateAnnot a => Ann Name a -> Ann Type a
mkTyVar = wrapperAnn . TyVar

mkTyKinded :: TemplateAnnot a => Ann Type a -> Ann Kind a -> Ann Type a
mkTyKinded t k = mkAnn (child <> " :: " <> child) (TyKinded t k)

mkTyBang :: TemplateAnnot a => Ann Type a -> Ann Type a
mkTyBang = mkAnn ("!" <> child) . TyBang

mkTyUnpack :: TemplateAnnot a => Ann Type a -> Ann Type a
mkTyUnpack = mkAnn ("{-# UNPACK #-} " <> child) . TyUnpack

mkTyWildcard :: TemplateAnnot a => Ann Type a
mkTyWildcard = mkAnn "_" TyWildcard

mkTyNamedWildcard :: TemplateAnnot a => Ann Name a -> Ann Type a
mkTyNamedWildcard = mkAnn ("_" <> child) . TyNamedWildc

-- * Generation of contexts

mkContextOne :: TemplateAnnot a => Ann Assertion a -> Ann Context a
mkContextOne = mkAnn (child <> " =>") . ContextOne

mkContextMulti :: TemplateAnnot a => [Ann Assertion a] -> Ann Context a
mkContextMulti = mkAnn ("(" <> child <> ") =>") . ContextMulti . mkAnnList (listSep ", ")

-- * Generation of assertions

mkClassAssert :: TemplateAnnot a => Ann Name a -> [Ann Type a] -> Ann Assertion a
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ ClassAssert n (mkAnnList (listSep " ") args)

mkInfixAssert :: TemplateAnnot a => Ann Type a -> Ann Name a -> Ann Type a -> Ann Assertion a
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ InfixAssert left op right

-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Kinds where

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

mkKindConstraint :: Ann Kind dom SrcTemplateStage -> Ann KindConstraint dom SrcTemplateStage
mkKindConstraint = mkAnn (" :: " <> child) . KindConstraint

mkKindStar :: Ann Kind dom SrcTemplateStage
mkKindStar = mkAnn "*" KindStar

mkKindUnbox :: Ann Kind dom SrcTemplateStage
mkKindUnbox = mkAnn "#" KindUnbox

mkKindFun :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ KindFn lhs rhs

mkKindParen :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindParen = mkAnn ("(" <> child <> ")") . KindParen

mkKindVar :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindVar = mkAnn child . KindVar

mkKindApp :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ KindApp lhs rhs

mkKindList :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindList = mkAnn ("[" <> child <> "]") . KindList

mkIntKind :: Integer -> Ann Kind dom SrcTemplateStage
mkIntKind i = mkAnn child $ KindPromoted $ mkAnn (fromString $ show i) (PromotedInt i)

mkStringKind :: String -> Ann Kind dom SrcTemplateStage
mkStringKind i = mkAnn child $ KindPromoted $ mkAnn (fromString $ show i) (PromotedString i)

mkConKind :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkConKind = mkAnn child . KindPromoted . mkAnn child . PromotedCon

mkListKind :: [Ann Kind dom SrcTemplateStage] -> Ann Kind dom SrcTemplateStage
mkListKind = mkAnn child . KindPromoted . mkAnn ("[" <> child <> "]") . PromotedList . mkAnnList (listSep ", ")

mkTupleKind :: [Ann Kind dom SrcTemplateStage] -> Ann Kind dom SrcTemplateStage
mkTupleKind = mkAnn child . KindPromoted . mkAnn ("(" <> child <> ")") . PromotedTuple . mkAnnList (listSep ", ")

mkUnitKind :: Ann Kind dom SrcTemplateStage
mkUnitKind = mkAnn child $ KindPromoted $ mkAnn "()" PromotedUnit
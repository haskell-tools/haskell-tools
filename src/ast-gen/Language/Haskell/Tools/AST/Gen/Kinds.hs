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
mkKindConstraint = mkAnn (" :: " <> child) . UKindConstraint

mkKindStar :: Ann Kind dom SrcTemplateStage
mkKindStar = mkAnn "*" UStarKind

mkKindUnbox :: Ann Kind dom SrcTemplateStage
mkKindUnbox = mkAnn "#" UUnboxKind

mkKindFun :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ UFunKind lhs rhs

mkKindParen :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindParen = mkAnn ("(" <> child <> ")") . UParenKind

mkKindVar :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindVar = mkAnn child . UVarKind

mkKindApp :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ UAppKind lhs rhs

mkKindList :: Ann Kind dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkKindList = mkAnn ("[" <> child <> "]") . UListKind

mkIntKind :: Integer -> Ann Kind dom SrcTemplateStage
mkIntKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedInt i)

mkStringKind :: String -> Ann Kind dom SrcTemplateStage
mkStringKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedString i)

mkConKind :: Ann Name dom SrcTemplateStage -> Ann Kind dom SrcTemplateStage
mkConKind = mkAnn child . UPromotedKind . mkAnn child . UPromotedCon

mkListKind :: [Ann Kind dom SrcTemplateStage] -> Ann Kind dom SrcTemplateStage
mkListKind = mkAnn child . UPromotedKind . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (listSep ", ")

mkTupleKind :: [Ann Kind dom SrcTemplateStage] -> Ann Kind dom SrcTemplateStage
mkTupleKind = mkAnn child . UPromotedKind . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (listSep ", ")

mkUnitKind :: Ann Kind dom SrcTemplateStage
mkUnitKind = mkAnn child $ UPromotedKind $ mkAnn "()" UPromotedUnit
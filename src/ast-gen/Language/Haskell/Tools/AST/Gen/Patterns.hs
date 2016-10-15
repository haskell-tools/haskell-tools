-- | Generation of pattern-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkVarPat@ creates the annotated version of the @VarPat@ AST constructor.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Patterns where

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

mkVarPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkVarPat = mkAnn child . UVarPat

mkLitPat :: Ann Literal dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkLitPat = mkAnn child . ULitPat

mkInfixAppPat :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAppPat lhs op rhs

mkAppPat :: Ann Name dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
mkAppPat n pat = mkAnn (child <> child) $ UAppPat n (mkAnnList (listSepBefore " " " ") pat)

mkTuplePat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
mkTuplePat pats = mkAnn ("(" <> child <> ")") $ UTuplePat (mkAnnList (listSep ", ") pats)

mkUnboxTuplePat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
mkUnboxTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UUnboxTuplePat (mkAnnList (listSep ", ") pats)

mkListPat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
mkListPat pats = mkAnn ("[" <> child <> "]") $ UListPat (mkAnnList (listSep ", ") pats)

mkParenPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkParenPat = mkAnn ("(" <> child <> ")") . UParenPat

mkRecPat :: Ann Name dom SrcTemplateStage -> [Ann PatternField dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ URecPat name (mkAnnList (listSep ", ") flds)

mkAsPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkAsPat name pat = mkAnn (child <> "@" <> child) $ UAsPat name pat

mkWildPat :: Ann Pattern dom SrcTemplateStage
mkWildPat = mkAnn "_" UWildPat

mkIrrefutablePat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkIrrefutablePat = mkAnn ("~" <> child) . UIrrefutablePat

mkBangPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkBangPat = mkAnn ("!" <> child) . UBangPat

mkTypeSigPat :: Ann Pattern dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkTypeSigPat pat typ = mkAnn (child <> " :: " <> child) $ UTypeSigPat pat typ

mkViewPat :: Ann Expr dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
mkViewPat name pat = mkAnn (child <> " -> " <> child) $ UViewPat name pat

mkPatternField :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann PatternField dom SrcTemplateStage
mkPatternField name pat = mkAnn (child <> " = " <> child) $ UNormalFieldPattern name pat

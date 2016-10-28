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
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkVarPat :: Name dom -> Pattern dom
mkVarPat = mkAnn child . UVarPat

mkLitPat :: Literal dom -> Pattern dom
mkLitPat = mkAnn child . ULitPat

mkInfixAppPat :: Pattern dom -> Operator dom -> Pattern dom -> Pattern dom
mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAppPat lhs op rhs

mkAppPat :: Name dom -> [Pattern dom] -> Pattern dom
mkAppPat n pat = mkAnn (child <> child) $ UAppPat n (mkAnnList (listSepBefore " " " ") pat)

mkTuplePat :: [Pattern dom] -> Pattern dom
mkTuplePat pats = mkAnn ("(" <> child <> ")") $ UTuplePat (mkAnnList (listSep ", ") pats)

mkUnboxTuplePat :: [Pattern dom] -> Pattern dom
mkUnboxTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UUnboxTuplePat (mkAnnList (listSep ", ") pats)

mkListPat :: [Pattern dom] -> Pattern dom
mkListPat pats = mkAnn ("[" <> child <> "]") $ UListPat (mkAnnList (listSep ", ") pats)

mkParenPat :: Pattern dom -> Pattern dom
mkParenPat = mkAnn ("(" <> child <> ")") . UParenPat

mkRecPat :: Name dom -> [PatternField dom] -> Pattern dom
mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ URecPat name (mkAnnList (listSep ", ") flds)

mkAsPat :: Name dom -> Pattern dom -> Pattern dom
mkAsPat name pat = mkAnn (child <> "@" <> child) $ UAsPat name pat

mkWildPat :: Pattern dom
mkWildPat = mkAnn "_" UWildPat

mkIrrefutablePat :: Pattern dom -> Pattern dom
mkIrrefutablePat = mkAnn ("~" <> child) . UIrrefutablePat

mkBangPat :: Pattern dom -> Pattern dom
mkBangPat = mkAnn ("!" <> child) . UBangPat

mkTypeSigPat :: Pattern dom -> Type dom -> Pattern dom
mkTypeSigPat pat typ = mkAnn (child <> " :: " <> child) $ UTypeSigPat pat typ

mkViewPat :: Expr dom -> Pattern dom -> Pattern dom
mkViewPat name pat = mkAnn (child <> " -> " <> child) $ UViewPat name pat

mkPatternField :: Name dom -> Pattern dom -> PatternField dom
mkPatternField name pat = mkAnn (child <> " = " <> child) $ UNormalFieldPattern name pat

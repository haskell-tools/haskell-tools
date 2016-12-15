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
import Language.Haskell.Tools.Transform

-- | Pattern name binding
mkVarPat :: Name dom -> Pattern dom
mkVarPat = mkAnn child . UVarPat

-- | Literal pattern
mkLitPat :: Literal dom -> Pattern dom
mkLitPat = mkAnn child . ULitPat

-- | Infix constructor application pattern (@ a :+: b @)
mkInfixAppPat :: Pattern dom -> Operator dom -> Pattern dom -> Pattern dom
mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAppPat lhs op rhs

-- | Constructor application pattern (@ Point x y @)
mkAppPat :: Name dom -> [Pattern dom] -> Pattern dom
mkAppPat n pat = mkAnn (child <> child) $ UAppPat n (mkAnnList (after " " $ separatedBy " " list) pat)

-- | Tuple pattern (@ (x,y) @)
mkTuplePat :: [Pattern dom] -> Pattern dom
mkTuplePat pats = mkAnn ("(" <> child <> ")") $ UTuplePat (mkAnnList (separatedBy ", " list) pats)

-- | Unboxed tuple pattern (@ (\# x, y \#) @)
mkUnboxTuplePat :: [Pattern dom] -> Pattern dom
mkUnboxTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UUnboxTuplePat (mkAnnList (separatedBy ", " list) pats)

-- | List pattern (@ [1,2,a,x] @)
mkListPat :: [Pattern dom] -> Pattern dom
mkListPat pats = mkAnn ("[" <> child <> "]") $ UListPat (mkAnnList (separatedBy ", " list) pats)

-- | Parallel array pattern (@ [:1,2,a,x:] @)
mkParArrayPat :: [Pattern dom] -> Pattern dom
mkParArrayPat pats = mkAnn ("[:" <> child <> ":]") $ UParArrPat (mkAnnList (separatedBy ", " list) pats)

-- | Parenthesised patterns
mkParenPat :: Pattern dom -> Pattern dom
mkParenPat = mkAnn ("(" <> child <> ")") . UParenPat

-- | Record pattern (@ Point { x = 3, y } @)
mkRecPat :: Name dom -> [PatternField dom] -> Pattern dom
mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ URecPat name (mkAnnList (separatedBy ", " list) flds)

-- | As-pattern (explicit name binding) (@ ls\@(hd:_) @)
mkAsPat :: Name dom -> Pattern dom -> Pattern dom
mkAsPat name pat = mkAnn (child <> "@" <> child) $ UAsPat name pat

-- | Wildcard pattern: (@ _ @)
mkWildPat :: Pattern dom
mkWildPat = mkAnn "_" UWildPat

-- | Irrefutable pattern (@ ~(x:_) @)
mkIrrefutablePat :: Pattern dom -> Pattern dom
mkIrrefutablePat = mkAnn ("~" <> child) . UIrrefutablePat

-- | Bang pattern (@ !x @)
mkBangPat :: Pattern dom -> Pattern dom
mkBangPat = mkAnn ("!" <> child) . UBangPat

-- | Pattern with explicit type signature (@ x :: Int @)
mkTypeSigPat :: Pattern dom -> Type dom -> Pattern dom
mkTypeSigPat pat typ = mkAnn (child <> " :: " <> child) $ UTypeSigPat pat typ

-- | View pattern (@ f -> Just 1 @)
mkViewPat :: Expr dom -> Pattern dom -> Pattern dom
mkViewPat name pat = mkAnn (child <> " -> " <> child) $ UViewPat name pat

-- | Splice patterns: @$(generateX inp)@
mkSplicePat :: Splice dom -> Pattern dom
mkSplicePat = mkAnn child . USplicePat

-- | Quasi-quoted patterns: @[| 1 + 2 |]@
mkQuasiQuotePat :: QuasiQuote dom -> Pattern dom
mkQuasiQuotePat = mkAnn child . UQuasiQuotePat

-- | Named field pattern (@ p = Point 3 2 @)
mkPatternField :: Name dom -> Pattern dom -> PatternField dom
mkPatternField name pat = mkAnn (child <> " = " <> child) $ UNormalFieldPattern name pat

-- | Named field pun (@ p @)
mkFieldPunPattern :: Name dom -> PatternField dom
mkFieldPunPattern name = mkAnn child $ UFieldPunPattern name

-- | Wildcard field pattern (@ .. @)
mkFieldWildcardPattern :: PatternField dom
mkFieldWildcardPattern = mkAnn child $ UFieldWildcardPattern $ mkAnn ".." FldWildcard
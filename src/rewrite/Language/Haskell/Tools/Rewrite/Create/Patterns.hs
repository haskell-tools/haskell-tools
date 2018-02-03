-- | Generation of pattern-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkVarPat@ creates the annotated version of the @VarPat@ AST constructor.
{-# LANGUAGE ExplicitNamespaces, KindSignatures, MonoLocalBinds, OverloadedStrings #-}

module Language.Haskell.Tools.Rewrite.Create.Patterns where

import Language.Haskell.Tools.AST (UFieldWildcard(..), UPatternField(..), UPattern(..))
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList)
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | Pattern name binding
mkVarPat :: Name -> Pattern
mkVarPat = mkAnn child . UVarPat

-- | Literal pattern
mkLitPat :: Literal -> Pattern
mkLitPat = mkAnn child . ULitPat

-- | Infix constructor application pattern (@ a :+: b @)
mkInfixAppPat :: Pattern -> Operator -> Pattern -> Pattern
mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAppPat lhs op rhs

-- | Constructor application pattern (@ Point x y @)
mkAppPat :: Name -> [Pattern] -> Pattern
mkAppPat n pat = mkAnn (child <> child) $ UAppPat n (mkAnnList (after " " $ separatedBy " " list) pat)

-- | Tuple pattern (@ (x,y) @)
mkTuplePat :: [Pattern] -> Pattern
mkTuplePat pats = mkAnn ("(" <> child <> ")") $ UTuplePat (mkAnnList (separatedBy ", " list) pats)

-- | Unboxed tuple pattern (@ (\# x, y \#) @)
mkUnboxTuplePat :: [Pattern] -> Pattern
mkUnboxTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UUnboxTuplePat (mkAnnList (separatedBy ", " list) pats)

-- | List pattern (@ [1,2,a,x] @)
mkListPat :: [Pattern] -> Pattern
mkListPat pats = mkAnn ("[" <> child <> "]") $ UListPat (mkAnnList (separatedBy ", " list) pats)

-- | Parallel array pattern (@ [:1,2,a,x:] @)
mkParArrayPat :: [Pattern] -> Pattern
mkParArrayPat pats = mkAnn ("[:" <> child <> ":]") $ UParArrPat (mkAnnList (separatedBy ", " list) pats)

-- | Parenthesised patterns
mkParenPat :: Pattern -> Pattern
mkParenPat = mkAnn ("(" <> child <> ")") . UParenPat

-- | Record pattern (@ Point { x = 3, y } @)
mkRecPat :: Name -> [PatternField] -> Pattern
mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ URecPat name (mkAnnList (separatedBy ", " list) flds)

-- | As-pattern (explicit name binding) (@ ls\@(hd:_) @)
mkAsPat :: Name -> Pattern -> Pattern
mkAsPat name pat = mkAnn (child <> "@" <> child) $ UAsPat name pat

-- | Wildcard pattern: (@ _ @)
mkWildPat :: Pattern
mkWildPat = mkAnn "_" UWildPat

-- | Irrefutable pattern (@ ~(x:_) @)
mkIrrefutablePat :: Pattern -> Pattern
mkIrrefutablePat = mkAnn ("~" <> child) . UIrrefutablePat

-- | Bang pattern (@ !x @)
mkBangPat :: Pattern -> Pattern
mkBangPat = mkAnn ("!" <> child) . UBangPat

-- | Pattern with explicit type signature (@ x :: Int @)
mkTypeSigPat :: Pattern -> Type -> Pattern
mkTypeSigPat pat typ = mkAnn (child <> " :: " <> child) $ UTypeSigPat pat typ

-- | View pattern (@ f -> Just 1 @)
mkViewPat :: Expr -> Pattern -> Pattern
mkViewPat name pat = mkAnn (child <> " -> " <> child) $ UViewPat name pat

-- | Splice patterns: @$(generateX inp)@
mkSplicePat :: Splice -> Pattern
mkSplicePat = mkAnn child . USplicePat

-- | Quasi-quoted patterns: @[| 1 + 2 |]@
mkQuasiQuotePat :: QuasiQuote -> Pattern
mkQuasiQuotePat = mkAnn child . UQuasiQuotePat

-- | Named field pattern (@ p = Point 3 2 @)
mkPatternField :: Name -> Pattern -> PatternField
mkPatternField name pat = mkAnn (child <> " = " <> child) $ UNormalFieldPattern name pat

-- | Named field pun (@ p @)
mkFieldPunPattern :: Name -> PatternField
mkFieldPunPattern name = mkAnn child $ UFieldPunPattern name

-- | Wildcard field pattern (@ .. @)
mkFieldWildcardPattern :: PatternField
mkFieldWildcardPattern = mkAnn child $ UFieldWildcardPattern $ mkAnn ".." FldWildcard
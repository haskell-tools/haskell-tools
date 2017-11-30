-- | UPattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.Rewrite.Match.Patterns where

import Language.Haskell.Tools.AST (Ann(..), UPatternField(..), UPattern(..))
import Language.Haskell.Tools.Rewrite.ElementTypes

-- | Pattern name binding
pattern VarPat :: Name -> Pattern
pattern VarPat var <- Ann _ (UVarPat var)

-- | Literal pattern
pattern LitPat :: Literal -> Pattern
pattern LitPat lit <- Ann _ (ULitPat lit)

-- | Infix constructor application pattern (@ a :+: b @)
pattern InfixAppPat :: Pattern -> Operator -> Pattern -> Pattern
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

-- | Constructor application pattern (@ Point x y @)
pattern AppPat :: Name -> PatternList -> Pattern
pattern AppPat n pat <- Ann _ (UAppPat n pat)

-- | Tuple pattern (@ (x,y) @)
pattern TuplePat :: PatternList -> Pattern
pattern TuplePat pats <- Ann _ (UTuplePat pats)

-- | Unboxed tuple pattern (@ (\# x, y \#) @)
pattern UnboxTuplePat :: PatternList -> Pattern
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

-- | List pattern (@ [1,2,a,x] @)
pattern ListPat :: PatternList -> Pattern
pattern ListPat pats <- Ann _ (UListPat pats)

-- | Parallel array pattern (@ [:1,2,a,x:] @)
pattern ParArrayPat :: PatternList -> Pattern
pattern ParArrayPat pats <- Ann _ (UParArrPat pats)

-- | Parenthesised patterns
pattern ParenPat :: Pattern -> Pattern
pattern ParenPat pat <- Ann _ (UParenPat pat)

-- | Record pattern (@ Point { x = 3, y } @)
pattern RecPat :: Name -> PatternFieldList -> Pattern
pattern RecPat name flds <- Ann _ (URecPat name flds)

-- | As-pattern (explicit name binding) (@ ls\@(hd:_) @)
pattern AsPat :: Name -> Pattern -> Pattern
pattern AsPat name pat <- Ann _ (UAsPat name pat)

-- | Wildcard pattern: (@ _ @)
pattern WildPat :: Pattern
pattern WildPat <- Ann _ UWildPat

-- | Irrefutable pattern (@ ~(x:_) @)
pattern IrrefutablePat :: Pattern -> Pattern
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

-- | Bang pattern (@ !x @)
pattern BangPat :: Pattern -> Pattern
pattern BangPat pat <- Ann _ (UBangPat pat)

-- | Pattern with explicit type signature (@ x :: Int @)
pattern TypeSigPat :: Pattern -> Type -> Pattern
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

-- | View pattern (@ f -> Just 1 @)
pattern ViewPat :: Expr -> Pattern -> Pattern
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

-- | Splice patterns: @$(generateX inp)@
pattern SplicePat :: Splice -> Pattern
pattern SplicePat splice <- Ann _ (USplicePat splice)

-- | Quasi-quoted patterns: @[| 1 + 2 |]@
pattern QuasiQuotePat :: QuasiQuote -> Pattern
pattern QuasiQuotePat qq <- Ann _ (UQuasiQuotePat qq)

pattern NPlusKPat :: Name -> Literal -> Pattern
pattern NPlusKPat name lit <- Ann _ (UNPlusKPat name lit)

-- | Named field pattern (@ p = Point 3 2 @)
pattern FieldPattern :: Name -> Pattern -> PatternField
pattern FieldPattern name pat <- Ann _ (UNormalFieldPattern name pat)

-- | Named field pun (@ p @)
pattern FieldPunPattern :: Name -> PatternField
pattern FieldPunPattern name <- Ann _ (UFieldPunPattern name)

-- | Wildcard field pattern (@ .. @)
pattern FieldWildcardPattern :: FieldWildcard -> PatternField
pattern FieldWildcardPattern wildc <- Ann _ (UFieldWildcardPattern wildc)
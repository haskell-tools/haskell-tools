-- | UPattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- | Pattern name binding
pattern VarPat :: Name dom -> Pattern dom
pattern VarPat var <- Ann _ (UVarPat var)

-- | Literal pattern
pattern LitPat :: Literal dom -> Pattern dom
pattern LitPat lit <- Ann _ (ULitPat lit)

-- | Infix constructor application pattern (@ a :+: b @)
pattern InfixAppPat :: Pattern dom -> Operator dom -> Pattern dom -> Pattern dom
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

-- | Constructor application pattern (@ Point x y @)
pattern AppPat :: Name dom -> PatternList dom -> Pattern dom
pattern AppPat n pat <- Ann _ (UAppPat n pat)

-- | Tuple pattern (@ (x,y) @)
pattern TuplePat :: PatternList dom -> Pattern dom
pattern TuplePat pats <- Ann _ (UTuplePat pats)

-- | Unboxed tuple pattern (@ (# x, y #) @)
pattern UnboxTuplePat :: PatternList dom -> Pattern dom
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

-- | List pattern (@ [1,2,a,x] @)
pattern ListPat :: PatternList dom -> Pattern dom
pattern ListPat pats <- Ann _ (UListPat pats)

-- | Parallel array pattern (@ [:1,2,a,x:] @)
pattern ParArrayPat :: PatternList dom -> Pattern dom
pattern ParArrayPat pats <- Ann _ (UParArrPat pats)

-- | Parenthesised patterns
pattern ParenPat :: Pattern dom -> Pattern dom
pattern ParenPat pat <- Ann _ (UParenPat pat)

-- | Record pattern (@ Point { x = 3, y } @)
pattern RecPat :: Name dom -> PatternFieldList dom -> Pattern dom
pattern RecPat name flds <- Ann _ (URecPat name flds)

-- | As-pattern (explicit name binding) (@ ls\@(hd:_) @)
pattern AsPat :: Name dom -> Pattern dom -> Pattern dom
pattern AsPat name pat <- Ann _ (UAsPat name pat)

-- | Wildcard pattern: (@ _ @)
pattern WildPat :: Pattern dom
pattern WildPat <- Ann _ UWildPat

-- | Irrefutable pattern (@ ~(x:_) @)
pattern IrrefutablePat :: Pattern dom -> Pattern dom
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

-- | Bang pattern (@ !x @)
pattern BangPat :: Pattern dom -> Pattern dom
pattern BangPat pat <- Ann _ (UBangPat pat)

-- | Pattern with explicit type signature (@ x :: Int @)
pattern TypeSigPat :: Pattern dom -> Type dom -> Pattern dom
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

-- | View pattern (@ f -> Just 1 @)
pattern ViewPat :: Expr dom -> Pattern dom -> Pattern dom
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

-- | Splice patterns: @$(generateX inp)@
pattern SplicePat :: Splice dom -> Pattern dom
pattern SplicePat splice <- Ann _ (USplicePat splice)

-- | Quasi-quoted patterns: @[| 1 + 2 |]@
pattern QuasiQuotePat :: QuasiQuote dom -> Pattern dom
pattern QuasiQuotePat qq <- Ann _ (UQuasiQuotePat qq)

pattern NPlusKPat :: Name dom -> Literal dom -> Pattern dom
pattern NPlusKPat name lit <- Ann _ (UNPlusKPat name lit)

-- | Named field pattern (@ p = Point 3 2 @)
pattern FieldPattern :: Name dom -> Pattern dom -> PatternField dom
pattern FieldPattern name pat <- Ann _ (UNormalFieldPattern name pat)

-- | Named field pun (@ p @)
pattern FieldPunPattern :: Name dom -> PatternField dom
pattern FieldPunPattern name <- Ann _ (UFieldPunPattern name)

-- | Wildcard field pattern (@ .. @)
pattern FieldWildcardPattern :: FieldWildcard dom -> PatternField dom
pattern FieldWildcardPattern wildc <- Ann _ (UFieldWildcardPattern wildc)
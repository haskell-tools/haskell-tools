-- | Representation of Haskell patterns
module Language.Haskell.Tools.AST.Representation.Patterns where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Exprs (UExpr, UFieldWildcard, UUnboxedSumPlaceHolder)
import Language.Haskell.Tools.AST.Representation.Literals (ULiteral)
import Language.Haskell.Tools.AST.Representation.Names (UName, UOperator)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.TH (UQuasiQuote, USplice)
import Language.Haskell.Tools.AST.Representation.Types (UType)


-- | Representation of patterns for pattern bindings
data UPattern dom stage
  = UVarPat         { _patternName :: Ann UName dom stage
                    } -- ^ Pattern name binding
  | ULitPat         { _patternLiteral :: Ann ULiteral dom stage
                    } -- ^ Literal pattern
  | UInfixAppPat    { _patternLhs :: Ann UPattern dom stage
                    , _patternOperator :: Ann UOperator dom stage
                    , _patternRhs :: Ann UPattern dom stage
                    } -- ^ Infix constructor application pattern (@ a :+: b @)
  | UAppPat         { _patternName :: Ann UName dom stage
                    , _patternArgs :: AnnListG UPattern dom stage
                    } -- ^ Constructor application pattern (@ Point x y @)
  | UTuplePat       { _patternElems :: AnnListG UPattern dom stage
                    } -- ^ Tuple pattern (@ (x,y) @)
  | UUnboxTuplePat  { _patternElems :: AnnListG UPattern dom stage
                    } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | UListPat        { _patternElems :: AnnListG UPattern dom stage
                    } -- ^ List pattern (@ [1,2,a,x] @)
  | UParArrPat      { _patternElems :: AnnListG UPattern dom stage
                    } -- ^ Parallel array pattern (@ [:1,2,a,x:] @)
  | UParenPat       { _patternInner :: Ann UPattern dom stage
                    } -- ^ Parenthesised patterns
  | URecPat         { _patternName :: Ann UName dom stage
                    , _patternFields :: AnnListG UPatternField dom stage
                    } -- ^ Record pattern (@ Point { x = 3, y } @)
  | UAsPat          { _patternName :: Ann UName dom stage
                    , _patternInner :: Ann UPattern dom stage
                    } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | UWildPat -- ^ Wildcard pattern: (@ _ @)
  | UIrrefutablePat { _patternInner :: Ann UPattern dom stage
                    } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | UBangPat        { _patternInner :: Ann UPattern dom stage
                    } -- ^ Bang pattern (@ !x @)
  | UTypeSigPat     { _patternInner :: Ann UPattern dom stage
                    , _patternType :: Ann UType dom stage
                    } -- ^ Pattern with explicit type signature (@ x :: Int @)
  | UViewPat        { _patternExpr :: Ann UExpr dom stage
                    , _patternInner :: Ann UPattern dom stage
                    } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | USplicePat     { _patternSplice :: Ann USplice dom stage
                   } -- ^ Splice patterns: @$(generateX inp)@
  | UQuasiQuotePat { _patQQ :: Ann UQuasiQuote dom stage
                   } -- ^ Quasi-quoted patterns: @[| 1 + 2 |]@
  | UNPlusKPat     { _patternName :: Ann UName dom stage
                   , _patternLit :: Ann ULiteral dom stage
                   }
  | UUnboxedSumPat { _patternSumPlaceholdersBefore :: AnnListG UUnboxedSumPlaceHolder dom stage
                   , _patternInner :: Ann UPattern dom stage
                   , _patternSumPlaceholdersAfter :: AnnListG UUnboxedSumPlaceHolder dom stage
                   } -- ^ Unboxed sum pattern (@ (# | expr #) @).

-- Field specification of a record pattern
data UPatternField dom stage
  = UNormalFieldPattern   { _fieldPatternName :: Ann UName dom stage
                          , _fieldPattern :: Ann UPattern dom stage
                          } -- ^ Named field pattern (@ p = Point 3 2 @)
  | UFieldPunPattern      { _fieldPatternName :: Ann UName dom stage
                          } -- ^ Named field pun (@ p @)
  | UFieldWildcardPattern { _fieldPatternWildcard :: Ann UFieldWildcard dom stage
                          } -- ^ Wildcard field pattern (@ .. @)

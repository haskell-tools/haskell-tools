-- | Representation of Haskell patterns
module Language.Haskell.Tools.AST.Patterns where
          
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base  
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (Expr, FieldWildcard)
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

        
-- | Representation of patterns for pattern bindings
data Pattern dom stage
  = VarPat        { _patternName :: Ann Name dom stage
                  } -- ^ Pattern name binding
  | LitPat        { _patternLiteral :: Ann Literal dom stage
                  } -- ^ Literal pattern
  | InfixPat      { _patternLhs :: Ann Pattern dom stage
                  , _patternOperator :: Ann Operator dom stage
                  , _patternRhs :: Ann Pattern dom stage
                  } -- ^ Infix constructor application pattern (@ a :+: b @)
  | AppPat        { _patternName :: Ann Name dom stage
                  , _patternArgs :: AnnList Pattern dom stage
                  } -- ^ Constructor application pattern (@ Point x y @)
  | TuplePat      { _patternElems :: AnnList Pattern dom stage
                  } -- ^ Tuple pattern (@ (x,y) @)
  | UnboxTuplePat { _patternElems :: AnnList Pattern dom stage
                  } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | ListPat       { _patternElems :: AnnList Pattern dom stage
                  } -- ^ List pattern (@ [1,2,a,x] @)
  | ParArrPat     { _patternElems :: AnnList Pattern dom stage
                  } -- ^ Parallel array pattern (@ [:1,2,a,x:] @)
  | ParenPat      { _patternInner :: Ann Pattern dom stage
                  } -- ^ Parenthesised patterns
  | RecPat        { _patternName :: Ann Name dom stage
                  , _patternFields :: AnnList PatternField dom stage
                  } -- ^ Record pattern (@ Point { x = 3, y } @)
  | AsPat         { _patternName :: Ann Name dom stage
                  , _patternInner :: Ann Pattern dom stage
                  } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | WildPat       -- ^ Wildcard pattern: (@ _ @)
  | IrrPat        { _patternInner :: Ann Pattern dom stage
                  } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | BangPat       { _patternInner :: Ann Pattern dom stage
                  } -- ^ Bang pattern (@ !x @)
  | TypeSigPat    { _patternInner :: Ann Pattern dom stage
                  , _patternType :: Ann Type dom stage
                  } -- ^ Pattern with explicit type signature (@ __ :: Int @)
  | ViewPat       { _patternExpr :: Ann Expr dom stage
                  , _patternInner :: Ann Pattern dom stage
                  } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | SplicePat     { _patternSplice :: Ann Splice dom stage
                  } -- ^ Splice patterns: @$(generateX inp)@
  | QuasiQuotePat { _patQQ :: Ann QuasiQuote dom stage
                  } -- ^ Quasi-quoted patterns: @[| 1 + 2 |]@
  | NPlusKPat     { _patternName :: Ann Name dom stage
                  , _patternLit :: Ann Literal dom stage
                  }
                  
-- Field specification of a record pattern
data PatternField dom stage
  = NormalFieldPattern   { _fieldPatternName :: Ann Name dom stage
                         , _fieldPattern :: Ann Pattern dom stage
                         } -- ^ Named field pattern (@ p = Point 3 2 @)
  | FieldPunPattern      { _fieldPatternName :: Ann Name dom stage
                         } -- ^ Named field pun (@ p @)
  | FieldWildcardPattern { _fieldPatternWildcard :: Ann FieldWildcard dom stage
                         } -- ^ Wildcard field pattern (@ .. @)

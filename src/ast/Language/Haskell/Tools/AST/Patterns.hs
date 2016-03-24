-- | Representation of Haskell patterns
module Language.Haskell.Tools.AST.Patterns where
          
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base  
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (Expr)
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

        
-- | Representation of patterns for pattern bindings
data Pattern a
  = VarPat        { _patternVar :: Ann Name a 
                  } -- ^ Pattern name binding
  | LitPat        { _patternLiteral :: Ann Literal a 
                  } -- ^ Literal pattern
  | InfixPat      { _patternLhs :: Ann Pattern a
                  , _patternOp :: Ann Name a
                  , _patternRhs :: Ann Pattern a
                  } -- ^ Infix constructor application pattern (@ a :+: b @)
  | AppPat        { _patternCon :: Ann Name a
                  , _patternArg :: Ann Pattern a
                  } -- ^ Constructor application pattern (@ Point x y @)
  | TuplePat      { _patternElems :: AnnList Pattern a
                  } -- ^ Tuple pattern (@ (x,y) @)
  | UnboxTuplePat { _patternElems :: AnnList Pattern a
                  } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | ListPat       { _patternElems :: AnnList Pattern a 
                  } -- ^ List pattern (@ [1,2,a,x] @)
  | ParArrPat     { _patternElems :: AnnList Pattern a 
                  } -- ^ Parallel array pattern (@ [:1,2,a,x:] @)
  | ParenPat      { _patternInner :: Ann Pattern a 
                  } -- ^ Parenthesised patterns
  | RecPat        { _patternName :: Ann Name a
                  , _patternFields :: AnnList PatternField a
                  } -- ^ Record pattern (@ Point { x = 3, y } @)
  | AsPat         { _patternName :: Ann Name a
                  , _patternInner :: Ann Pattern a
                  } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | WildPat       -- ^ Wildcard pattern: (@ _ @)
  | IrrPat        { _patternInner :: Ann Pattern a 
                  } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | BangPat       { _patternInner :: Ann Pattern a 
                  } -- ^ Bang pattern (@ !x @)
  | TypeSigPat    { _patternInner :: Ann Pattern a
                  , _patternType :: Ann Type a
                  } -- ^ Pattern with explicit type signature (@ __ :: Int @)
  | ViewPat       { _patternExpr :: Ann Expr a
                  , _patternInner :: Ann Pattern a
                  } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | SplicePat     { _patternSplice :: Ann Splice a 
                  } -- ^ Splice patterns: @$(generateX inp)@
  | QuasiQuotePat { _patQQ :: Ann QuasiQuote a 
                  } -- ^ Quasi-quoted patterns: @[| 1 + 2 |]@
                  
-- Field specification of a record pattern
data PatternField a 
  = NormalFieldPattern   { _fieldPatternName :: Ann Name a
                         , _fieldPattern :: Ann Pattern a
                         } -- ^ Named field pattern (@ p = Point 3 2 @)
  | FieldPunPattern      { _fieldPunName :: Ann Name a 
                         } -- ^ Named field pun (@ p @)
  | FieldWildcardPattern -- ^ Wildcard field pattern (@ .. @)

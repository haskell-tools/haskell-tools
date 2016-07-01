
{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

-- | Simple AST elements of Haskell
module Language.Haskell.Tools.AST.Base where
  
import Language.Haskell.Tools.AST.Ann

data Operator a
  = BacktickOp { _operatorName :: Ann SimpleName a } -- ^ Backtick operator name: @ a `mod` b @
  | NormalOp { _operatorName :: Ann SimpleName a }

data Name a
  = ParenName { _simpleName :: Ann SimpleName a } -- ^ Parenthesized name: @ foldl (+) 0 @
  | NormalName { _simpleName :: Ann SimpleName a }

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data SimpleName a 
  = SimpleName { _qualifiers      :: AnnList UnqualName a
               , _unqualifiedName :: Ann UnqualName a 
               }

nameFromList :: AnnList UnqualName a -> SimpleName a
nameFromList (AnnList a xs) | not (null xs) 
  = SimpleName (AnnList a (init xs)) (last xs) 
nameFromList _ = error "nameFromList: empty list"
         
-- | Parts of a qualified name.         
data UnqualName a 
  = UnqualName { _simpleNameStr :: String } 
               
-- | Program elements formatted as string literals (import packages, pragma texts)
data StringNode a
  = StringNode { _stringNodeStr :: String }
                   
-- | The @data@ or the @newtype@ keyword to define ADTs.
data DataOrNewtypeKeyword a
  = DataKeyword
  | NewtypeKeyword
    
-- | Keywords @do@ or @mdo@ to start a do-block
data DoKind a
  = DoKeyword
  | MDoKeyword
  
-- | The @type@ keyword used to qualify that the type and not the constructor of the same name is referred
data TypeKeyword a = TypeKeyword
  
-- | Recognised overlaps for overlap pragmas. Can be applied to class declarations and class instance declarations.    
data OverlapPragma a
  = EnableOverlap     -- ^ @OVERLAP@ pragma
  | DisableOverlap    -- ^ @NO_OVERLAP@ pragma
  | Overlappable      -- ^ @OVERLAPPABLE@ pragma
  | Overlapping       -- ^ @OVERLAPPING@ pragma
  | Overlaps          -- ^ @OVERLAPS@ pragma
  | IncoherentOverlap -- ^ @INCOHERENT@ pragma
  
-- | Call conventions of foreign functions
data CallConv a
  = StdCall
  | CCall
  | CPlusPlus
  | DotNet
  | Jvm
  | Js
  | JavaScript
  | CApi
  
data ArrowAppl a
  = LeftAppl -- ^ Left arrow application: @-<@
  | RightAppl -- ^ Right arrow application: @>-@
  | LeftHighApp -- ^ Left arrow high application: @-<<@
  | RightHighApp -- ^ Right arrow high application: @>>-@
  
-- | Safety annotations for foreign calls
data Safety a
  = Safe
  | ThreadSafe
  | Unsafe
  | Interruptible

-- | Associativity of an operator.
data Assoc a
  = AssocNone  -- ^ non-associative operator (declared with @infix@)
  | AssocLeft  -- ^ left-associative operator (declared with @infixl@)
  | AssocRight -- ^ right-associative operator (declared with @infixr@)

data Role a
  = Nominal
  | Representational
  | Phantom
  
data ConlikeAnnot a = ConlikeAnnot

-- | Numeric precedence of an operator
data Precedence a
  = Precedence { _precedenceValue :: Int } 

data LineNumber a
  = LineNumber { _lineNumber :: Int } 
     
-- | Controls the activation of a rewrite rule (@ [1] @)
data PhaseControl a
  = PhaseControl { _phaseUntil :: AnnMaybe PhaseInvert a
                 , _phaseNumber :: Ann PhaseNumber a
                 } 

-- | Phase number for rewrite rules
data PhaseNumber a 
  = PhaseNumber { _phaseNum :: Integer }

-- | A tilde that marks the inversion of the phase number
data PhaseInvert a = PhaseInvert
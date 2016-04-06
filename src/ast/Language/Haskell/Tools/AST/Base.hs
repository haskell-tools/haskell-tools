
{-# LANGUAGE TypeFamilies
           , KindSignatures
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

-- | Simple AST elements of Haskell
module Language.Haskell.Tools.AST.Base where
  
import Language.Haskell.Tools.AST.Ann

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data Name a 
  = Name { _qualifiers      :: AnnList SimpleName a
         , _unqualifiedName :: Ann SimpleName a 
         } 

nameFromList :: AnnList SimpleName a -> Name a
nameFromList (AnnList a xs) | not (null xs) 
  = Name (AnnList a (init xs)) (last xs) 
nameFromList _ = error "nameFromList: empty list"
         
-- | Parts of a qualified name.         
data SimpleName a 
  = SimpleName { _simpleNameStr :: String } 
               
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
  
-- | Numeric precedence of an operator
data Precedence a
  = Precedence { _precedenceValue :: Int } 
     
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
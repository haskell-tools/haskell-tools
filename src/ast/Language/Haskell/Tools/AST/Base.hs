
{-# LANGUAGE TypeFamilies
           , KindSignatures
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

-- | Simple AST elements of Haskell
module Language.Haskell.Tools.AST.Base where
  
import Language.Haskell.Tools.AST.Ann

data AnnotationWrapper = AnnotationWrapper
data IdWrapper = IdWrapper

type family IdType a (elem :: * -> *) info
type family ListType a (elem :: * -> *) info
type family MaybeType a (elem :: * -> *) info

type instance IdType AnnotationWrapper elem annot = Ann elem annot
type instance ListType AnnotationWrapper elem annot = AnnList elem annot
type instance MaybeType AnnotationWrapper elem annot = AnnMaybe elem annot

type instance IdType IdWrapper elem annot = elem annot
type instance ListType IdWrapper elem annot = elem annot
type instance MaybeType IdWrapper elem annot = elem annot


-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data Name a = Name { qualifiers      :: AnnList SimpleName a
                   , unqualifiedName :: Ann SimpleName a 
                   } 
                   
nameFromList :: AnnList SimpleName a -> Name a
nameFromList (AnnList xs) | not (null xs) 
  = Name (AnnList $ init xs) (last xs) 
nameFromList _ = error "nameFromList: empty list"
         
-- | Parts of a qualified name.         
data SimpleName a 
  = SimpleName { simpleNameStr :: String } 
               
-- | Program elements formatted as string literals (import packages, pragma texts)
data StringNode a
  = StringNode { stringNodeStr :: String }
                   
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
  = LeftAppl
  | RightAppl
  | LeftHighApp
  | RightHighApp
  
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
  
-- | Numeric precedence of an operator
data Precedence a
  = Precedence { precedenceValue :: Int } 
     
-- | Controls the activation of a rewrite rule (@ [1] @)
data PhaseControl a
  = PhaseControl { phaseInvert :: AnnMaybe PhaseInvert a
                 , phaseNumber :: Ann PhaseNumber a
                 } 

data PhaseNumber a = PhaseNumber { phaseNum :: Integer }

-- | A tilde that marks the inversion of the phase number
data PhaseInvert a = PhaseInvert
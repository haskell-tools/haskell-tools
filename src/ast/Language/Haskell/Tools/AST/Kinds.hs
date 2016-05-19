-- | Representation of Haskell Kinds
module Language.Haskell.Tools.AST.Kinds where

import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Kind constraint (@ :: * -> * @)
data KindConstraint a 
  = KindConstraint { _kindConstr :: Ann Kind a 
                   }
                 
-- | Haskell kinds
data Kind a
  = KindStar  -- ^ @*@, the kind of types
  | KindUnbox -- ^ @#@, the kind of unboxed types
  | KindFn       { _kindLeft :: Ann Kind a
                 , _kindRight :: Ann Kind a
                 } -- ^ @->@, the kind of type constructor
  | KindParen    { _kindParen :: Ann Kind a
                 } -- ^ A parenthesised kind
  | KindVar      { _kindVar :: Ann Name a
                 } -- ^ kind variable (using @PolyKinds@ extension)
  | KindApp      { _kindAppFun :: Ann Kind a
                 , _kindAppArg :: Ann Kind a 
                 } -- ^ Kind application (@ k1 k2 @)
  | KindList     { _kindElem :: Ann Kind a
                 } -- ^ A list kind (@ [k] @)
  | KindPromoted { _kindPromoted :: Ann (Promoted Kind) a
                 } -- ^ A promoted kind (@ '(k1,k2,k3) @)

data Promoted t a
  = PromotedInt    { _promotedIntValue :: Integer }
  | PromotedString { _promotedStringValue :: String }
  | PromotedCon    { _promotedConName :: Ann Name a }
  | PromotedList   { _promotedElements :: AnnList t a }
  | PromotedTuple  { _promotedElements :: AnnList t a }
  | PromotedUnit
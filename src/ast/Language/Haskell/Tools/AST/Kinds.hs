-- | Representation of Haskell Kinds
module Language.Haskell.Tools.AST.Kinds where

import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Kind constraint (@ :: * -> * @)
data KindConstraint dom stage
  = KindConstraint { _kindConstr :: Ann Kind dom stage 
                   }
                 
-- | Haskell kinds
data Kind dom stage
  = KindStar  -- ^ @*@, the kind of types
  | KindUnbox -- ^ @#@, the kind of unboxed types
  | KindFn       { _kindLeft :: Ann Kind dom stage
                 , _kindRight :: Ann Kind dom stage
                 } -- ^ @->@, the kind of type constructor
  | KindParen    { _kindParen :: Ann Kind dom stage
                 } -- ^ A parenthesised kind
  | KindVar      { _kindVar :: Ann Name dom stage
                 } -- ^ kind variable (using @PolyKinds@ extension)
  | KindApp      { _kindAppFun :: Ann Kind dom stage
                 , _kindAppArg :: Ann Kind dom stage 
                 } -- ^ Kind application (@ k1 k2 @)
  | KindList     { _kindElem :: Ann Kind dom stage
                 } -- ^ A list kind (@ [k] @)
  | KindPromoted { _kindPromoted :: Ann (Promoted Kind) dom stage
                 } -- ^ A promoted kind (@ '(k1,k2,k3) @)

data Promoted t dom stage
  = PromotedInt    { _promotedIntValue :: Integer }
  | PromotedString { _promotedStringValue :: String }
  | PromotedCon    { _promotedConName :: Ann Name dom stage }
  | PromotedList   { _promotedElements :: AnnList t dom stage }
  | PromotedTuple  { _promotedElements :: AnnList t dom stage }
  | PromotedUnit
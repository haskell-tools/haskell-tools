-- | Representation of Haskell Kinds
module Language.Haskell.Tools.AST.Kinds where

import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Kind constraint (@ :: * -> * @)
data KindConstraint dom stage
  = UKindConstraint { _kindConstr :: Ann Kind dom stage 
                    }
                 
-- | Haskell kinds
data Kind dom stage
  = UStarKind  -- ^ @*@, the kind of types
  | UUnboxKind -- ^ @#@, the kind of unboxed types
  | UFunKind      { _kindLeft :: Ann Kind dom stage
                  , _kindRight :: Ann Kind dom stage
                  } -- ^ @->@, the kind of type constructor
  | UParenKind    { _kindParen :: Ann Kind dom stage
                  } -- ^ A parenthesised kind
  | UVarKind      { _kindVar :: Ann Name dom stage
                  } -- ^ kind variable (using @PolyKinds@ extension)
  | UAppKind      { _kindAppFun :: Ann Kind dom stage
                  , _kindAppArg :: Ann Kind dom stage 
                  } -- ^ Kind application (@ k1 k2 @)
  | UListKind     { _kindElem :: Ann Kind dom stage
                  } -- ^ A list kind (@ [k] @)
  | UPromotedKind { _kindPromoted :: Ann (Promoted Kind) dom stage
                  } -- ^ A promoted kind (@ '(k1,k2,k3) @)

data Promoted t dom stage
  = UPromotedInt    { _promotedIntValue :: Integer }
  | UPromotedString { _promotedStringValue :: String }
  | UPromotedCon    { _promotedConName :: Ann Name dom stage }
  | UPromotedList   { _promotedElements :: AnnList t dom stage }
  | UPromotedTuple  { _promotedElements :: AnnList t dom stage }
  | UPromotedUnit
{-# LANGUAGE TypeFamilies #-}

module TypeEqualityInName where

import Definitions

{-@ GADTs @-}

g1 = eqRelName                  {-* (TypeFamilies + GADTs) *-}

g2 = nestedEqRelName            {-* TypeFamilies + GADTs, (TypeFamilies + GADTs) *-}

x <+> y = nestedEqRelName       {-* TypeFamilies + GADTs, (TypeFamilies + GADTs) *-}

x <-> y = nestedEqRelName 5 id  {-* (TypeFamilies + GADTs) *-}

{-# LANGUAGE TypeFamilies #-}

module TypeEqualityInName where

import Definitions

{-@ TypeFamilies @-}

g = eqRelName {-* TypeFamilies + GADTs *-}

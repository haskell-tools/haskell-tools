{-# LANGUAGE TypeFamilies #-}

module TypeEqualityInName where

import Definitions

{-@ GADTs @-}

g = eqRelName {-* TypeFamilies + GADTs *-}

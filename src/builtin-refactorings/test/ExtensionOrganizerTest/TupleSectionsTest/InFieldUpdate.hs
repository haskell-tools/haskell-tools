{-# LANGUAGE TupleSections #-}

module InFieldUpdate where

import Definitions

f Rec { tup = tup } = Rec {tup = (0,) 0} {-* TupleSections *-}

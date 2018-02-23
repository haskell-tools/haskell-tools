{-# LANGUAGE MultiWayIf #-}

module InFieldUpdate where

import Definitions

f Rec { num = num } = Rec {num = if | True -> 0 | False -> 1} {-* MultiWayIf *-}

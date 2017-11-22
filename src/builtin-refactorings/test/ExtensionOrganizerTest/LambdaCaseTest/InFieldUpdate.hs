{-# LANGUAGE LambdaCase #-}

module InFieldUpdate where

import Definitions

f Rec { num = num } = Rec {num = (\case {n -> 2*n}) num} {-* LambdaCase *-}

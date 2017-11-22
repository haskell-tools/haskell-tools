{-# LANGUAGE RecordWildCards #-}

module InExpression where

import Definitions

g :: T
g = let a = 3
        b = const True
        c = 3.14
    in T{..} {-* RecordWildCards *-}

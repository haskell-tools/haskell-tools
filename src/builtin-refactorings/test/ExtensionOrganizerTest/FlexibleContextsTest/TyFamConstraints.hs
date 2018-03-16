{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module TyFamConstraints where

import Definitions

f1 :: TF [a] => a -> ()
f1 = const ()

f2 :: [a] ~ [Int] => a -> Int  {-* TypeFamilies + GADTs, TypeFamilies + GADTs *-}
f2 = id                        {-* TypeFamilies + GADTs *-}

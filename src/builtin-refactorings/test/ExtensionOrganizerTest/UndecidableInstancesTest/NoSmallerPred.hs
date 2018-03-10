{-# LANGUAGE FlexibleInstances, UndecidableInstances, ExplicitForAll #-}

module NoSmallerPred where

import Definitions

instance Eq a => C a                           {-* UndecidableInstances, FlexibleInstances *-}
instance Eq (D [a] b) => C2 (a,b)              {-* UndecidableInstances *-}
instance forall a b . Eq (D [a] b) => C3 (a,b) {-* UndecidableInstances *-}
instance Eq (D Int Int) => C4 (Int,Int)        {-* UndecidableInstances, FlexibleInstances *-}

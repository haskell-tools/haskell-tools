{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module TupleConstraint where

import Definitions

instance (Ord a, Eq a) => C a  {-* UndecidableInstances, FlexibleInstances *-}

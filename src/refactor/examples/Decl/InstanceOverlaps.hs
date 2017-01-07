{-# LANGUAGE FlexibleInstances #-}
module Decl.InstanceOverlaps where

data A a = A a

instance {-# OVERLAPPING #-} Show (A String) where
  show (A a) = a
instance {-# OVERLAPS #-} Show a => Show (A [a]) where
  show (A a) = show a
instance {-# OVERLAPPABLE #-} Show (A a) where
  show (A _) = "A"
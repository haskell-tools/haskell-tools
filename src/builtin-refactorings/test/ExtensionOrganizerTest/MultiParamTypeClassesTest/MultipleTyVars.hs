{-# LANGUAGE MultiParamTypeClasses #-}

module MultipleTyVars where

class C2 a b where  {-* MultiParamTypeClasses *-}
  f :: a -> b -> ()

class C3 a b c where  {-* MultiParamTypeClasses *-}
  g :: a -> b -> c -> ()

instance C2 Int Int where  {-* MultiParamTypeClasses *-}
  f _ _ = ()

instance C3 Int Int Int where  {-* MultiParamTypeClasses *-}
  g _ _ _ = ()

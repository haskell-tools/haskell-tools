{-# LANGUAGE FunctionalDependencies #-}

module Basic where

class C a b where               {-* MultiParamTypeClasses *-}
  f :: a -> b -> ()

class FDC a b | a -> b  where   {-* FunctionalDependencies, MultiParamTypeClasses *-}
  g :: a -> b -> ()

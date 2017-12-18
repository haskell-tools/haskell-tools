{-# LANGUAGE FunctionalDependencies #-}

module Basic where

class C a b where
  f :: a -> b -> ()

class FDC a b | a -> b  where   {-* FunctionalDependencies *-}
  g :: a -> b -> ()

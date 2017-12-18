{-# LANGUAGE DefaultSignatures #-}

module Basic where

class DefaultClass a where
  f :: a -> ()
  f = const ()

class C a where
  g :: a -> ()
  default g :: DefaultClass a => a -> ()  {-* DefaultSignatures *-}
  g = f

{-# LANGUAGE KindSignatures #-}

module InDataDecl where

data Foo (f :: * -> *) a = Foo (f a)  {-* KindSignatures, KindSignatures, KindSignatures *-}

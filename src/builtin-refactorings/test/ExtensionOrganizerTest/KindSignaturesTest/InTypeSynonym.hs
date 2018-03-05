{-# LANGUAGE KindSignatures #-}

module InTypeSynonym where

type Foo (f :: * -> *) a = f a  {-* KindSignatures, KindSignatures, KindSignatures *-}

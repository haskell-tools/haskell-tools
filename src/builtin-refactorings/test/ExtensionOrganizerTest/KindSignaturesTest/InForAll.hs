{-# LANGUAGE KindSignatures, ExplicitForAll #-}

module InForAll where

f :: forall (a :: *) . a -> a {-* KindSignatures *-}
f = id

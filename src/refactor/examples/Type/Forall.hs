{-# LANGUAGE ScopedTypeVariables #-}
module Type.Forall where

id :: forall a . a -> a
id x = x

id' :: a -> a
id' x = x

id'' :: forall a . Monoid a => a -> a
id'' x = x
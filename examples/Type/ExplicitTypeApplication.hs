{-# LANGUAGE TypeApplications #-}
module Type.ExplicitTypeApplication where

quad :: a -> b -> c -> d -> (a, b, c, d)
quad w x y z = (w, x, y, z)

foo = quad @Bool @_ @Int False 'c' 17 "Hello!"
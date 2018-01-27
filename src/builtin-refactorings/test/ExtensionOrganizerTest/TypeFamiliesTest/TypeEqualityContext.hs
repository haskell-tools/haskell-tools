{-# LANGUAGE TypeFamilies #-}

module TypeEqualityContext where

f :: a ~ b => a -> b  {-* TypeFamilies, TypeFamilies *-}
f = id

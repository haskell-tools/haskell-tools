{-# LANGUAGE TypeFamilies #-}

module TypeEqualityContext where

{-@ TypeFamilies @-}

f :: a ~ b => a -> b  {-* TypeFamilies + GADTs, TypeFamilies + GADTs *-}
f = id

{-# LANGUAGE TypeFamilies #-}

module TypeEqualityContext where

{-@ GADTs @-}

f :: a ~ b => a -> b  {-* TypeFamilies + GADTs, TypeFamilies + GADTs *-}
f = id                {-* TypeFamilies + GADTs *-}

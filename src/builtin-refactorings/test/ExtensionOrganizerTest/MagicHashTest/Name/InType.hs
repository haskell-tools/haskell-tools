{-# LANGUAGE MagicHash,
             ExplicitForAll,
             KindSignatures,
             TypeOperators
             #-}

module InType where

{-# ANN module "HLint: ignore Redundant bracket" #-}


-- TODO: unboxed tuples, template haskell, etc ...



newtype T a b = T (a,b)

data a :+: b = Plus a b


f1 :: forall a# . a# -> ()     {-* MagicHash, MagicHash *-}
f1 x = ()

f2 :: Show a# => a# -> ()      {-* MagicHash, MagicHash *-}
f2 x = ()

f3 :: a# -> b# -> ()           {-* MagicHash, MagicHash *-}
f3 x y = ()

f4 :: (a#, b#) -> ()           {-* MagicHash, MagicHash *-}
f4 x = ()

f5 :: [a#] -> ()               {-* MagicHash *-}
f5 xs = ()

f6 :: T a# b# -> T a# b#       {-* MagicHash, MagicHash, MagicHash, MagicHash *-}
f6 = id

f7 :: (a#) -> ()               {-* MagicHash *-}
f7 x = ()

f8 :: a# :+: b# -> ()          {-* MagicHash, MagicHash *-} -- TypeOperators
f8 x = ()

f9 :: (a# :: *) -> ()          {-* MagicHash *-} --KindSignatures
f9 x = ()

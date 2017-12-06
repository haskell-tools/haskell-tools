{-# LANGUAGE MagicHash #-}
module Type.Primitives where
import GHC.Prim

data MyInt = MyInt Int#

my3 = MyInt 3#

myPlus :: MyInt -> MyInt -> MyInt
myPlus (MyInt i) (MyInt j) = MyInt (i +# j)

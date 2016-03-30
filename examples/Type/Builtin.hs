{-# LANGUAGE MagicHash #-}
module Type.Builtin where

import GHC.Prim 
import GHC.Base

x1 :: IO ()
x1 = undefined

x2 :: IO Int
x2 = return 2

x3 :: Int
x3 = I# x3'
  where x3' :: Int#
        x3' = 3#

x4 :: (Int, Bool)
x4 = (3,True)


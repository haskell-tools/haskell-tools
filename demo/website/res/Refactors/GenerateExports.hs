{-# LANGUAGE TypeFamilies #-}
-- Quite a few things are defined in this module, but maybe not all of them should be exposed.
-- TODO: use [Ctrl, UAlt and/or Shift + E] generate exports for the module
-- and narrow the generated interface.
module Refactors.GenerateExports where

function :: Int -> Int
function a = a

data TypeCtor = DataCtor { recordName :: Int }

class TypeClass a where
  classFunction :: a -> a

type family UTypeFamily a :: *

data family DataFamily a :: *

foreign import ccall "exp" c_exp :: Double -> Double 


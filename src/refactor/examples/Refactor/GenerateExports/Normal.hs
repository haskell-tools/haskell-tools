{-# LANGUAGE TypeFamilies #-}
module Refactor.GenerateExports.Normal where

function :: Int -> Int
function a = a

data TypeCtor = DataCtor { recordName :: Int }

class TypeClass a where
  classFunction :: a -> a

type family TypeFamily a :: *

data family DataFamily a :: *

foreign import ccall "exp" c_exp :: Double -> Double 


{-# LANGUAGE TypeFamilies #-}
module Decl.DataFamily where

data family Array :: * -> *

data instance Array () = UnitArray Int deriving Show

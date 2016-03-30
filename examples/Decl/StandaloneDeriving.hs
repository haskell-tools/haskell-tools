{-# LANGUAGE StandaloneDeriving #-}
module Decl.StandaloneDeriving where

data WrapStr = WrapStr String 

deriving instance Eq WrapStr

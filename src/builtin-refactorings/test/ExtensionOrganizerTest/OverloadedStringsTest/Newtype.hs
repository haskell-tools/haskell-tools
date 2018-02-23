{-# LANGUAGE OverloadedStrings #-}

module Newtype where

import GHC.Exts (IsString(..))

newtype MyString = MyString String
  deriving Eq

instance IsString MyString where
  fromString = MyString

f :: MyString -> MyString
f "asd" = "qwe"             {-* OverloadedStrings, OverloadedStrings *-}
f x     = x

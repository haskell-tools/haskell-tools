{-# LANGUAGE OverloadedStrings #-}

-- the pragma is only enabled so that the OverloadedStrings checker runs
module Synonym where

import GHC.Exts (IsString(..))

type MyString1 = [Char]
type MyString2 = String

f :: MyString1 -> MyString1
f "asd" = "qwe"
f x     = x

g :: MyString2 -> MyString2
g "asd" = "qwe"
g x     = x

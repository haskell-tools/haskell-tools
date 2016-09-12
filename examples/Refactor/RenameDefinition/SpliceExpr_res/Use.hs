{-# LANGUAGE TemplateHaskell #-}
module Use where

import Define

e :: Int
e = $(exprSplice)
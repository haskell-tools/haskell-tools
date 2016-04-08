{-# LANGUAGE QuasiQuotes #-}
module TH.QuasiQuote.Use where

import TH.QuasiQuote.Define

hello :: Int
hello = 3

main :: IO ()
main = print [expr| 1 + 2 + 4|]

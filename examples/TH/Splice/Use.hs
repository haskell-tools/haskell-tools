{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.Use where

import TH.Splice.Define

$(return [])

$(let x = return [] in x)

$(def "x")

$(defHello)

$defHello2

defHello3

a :: $typ
a = ()

e :: Int
e = $expr
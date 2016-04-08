-- TEMPLATE HASKELL SPLICES DON'T WORK CURRENTLY. SEE: #32
{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.Use where

import TH.Splice.Define

$(def "x")

$(defHello)

$defHello2
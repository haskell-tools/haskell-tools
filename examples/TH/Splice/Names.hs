{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.Names where

import TH.Splice.Define

data A = A

$(nameOf ''A)
nameOf ''A

$(nameOf 'A)
nameOf 'A

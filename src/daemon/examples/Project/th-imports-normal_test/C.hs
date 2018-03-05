{-# LANGUAGE TemplateHaskell #-}
module C where

import A
import Language.Haskell.TH

c' :: Q [Dec]
c' = [d| type X = A'' |]

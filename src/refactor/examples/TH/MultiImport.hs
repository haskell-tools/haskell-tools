{-# LANGUAGE TemplateHaskell #-}
module TH.MultiImport where

import Prelude (last,return)
import qualified Data.Text (last)

$(let f = last in return [])

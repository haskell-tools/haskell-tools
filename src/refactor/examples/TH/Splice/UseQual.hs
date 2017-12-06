{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.UseQual where

import qualified TH.Splice.Define as Def

$(Def.def "x")

{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.UseQualMulti where

import qualified TH.Splice.Define as Def
import qualified TH.Splice.Define as Def2

$(Def.def "x")
$(Def2.def "y")

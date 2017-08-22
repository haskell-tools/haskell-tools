{-# LANGUAGE TemplateHaskell #-}
module A where

import Paths_some_test_package
import Language.Haskell.TH

$(runIO (print version) >> return [])

{-# LANGUAGE CPP #-}
module CPP.ConditionalImportEnd where

import Data.List
import Data.List
#ifndef USE_DATA_LIST
import Control.Monad ((>>))
#endif

a = Nothing >> Nothing

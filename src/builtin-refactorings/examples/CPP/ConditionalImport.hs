{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where

import Data.List
#ifndef USE_DATA_LIST
import Control.Monad (Monad(..))
#endif
import Data.List

a = Nothing >> Nothing

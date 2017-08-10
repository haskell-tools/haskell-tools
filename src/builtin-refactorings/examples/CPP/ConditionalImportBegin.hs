{-# LANGUAGE CPP #-}
module CPP.ConditionalImportBegin where

#ifndef USE_DATA_LIST
import Control.Monad ((>>))
#endif
import Data.List
import Data.List

a = Nothing >> Nothing

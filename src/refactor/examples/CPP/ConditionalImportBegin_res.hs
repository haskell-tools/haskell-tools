{-# LANGUAGE CPP #-}
module CPP.ConditionalImportBegin where

#ifndef USE_DATA_LIST
import Control.Monad ((>>))
#endif


a = Nothing >> Nothing

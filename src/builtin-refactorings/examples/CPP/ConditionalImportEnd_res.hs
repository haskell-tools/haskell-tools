{-# LANGUAGE CPP #-}
module CPP.ConditionalImportEnd where


#ifndef USE_DATA_LIST
import Control.Monad ((>>))
#endif

a = Nothing >> Nothing

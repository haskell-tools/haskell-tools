{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where


#ifndef USE_DATA_LIST
import Control.Monad (Monad(..))
#endif


a = Nothing >> Nothing

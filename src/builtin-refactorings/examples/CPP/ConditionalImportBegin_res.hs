{-# LANGUAGE CPP #-}
module CPP.ConditionalImportBegin where

#ifndef USE_DATA_LIST
import CPP.A(a)
#endif


x = a

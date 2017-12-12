{-# LANGUAGE CPP #-}
module CPP.ConditionalImportBegin where

#ifndef USE_DATA_LIST
import CPP.A(a)
#endif
import CPP.B(b)
import CPP.B(b)

x = a

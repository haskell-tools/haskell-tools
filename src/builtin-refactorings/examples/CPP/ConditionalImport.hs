{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where

import CPP.A(a)
#ifndef USE_DATA_LIST
import CPP.B(b)
#endif
import CPP.A(a)

x = b

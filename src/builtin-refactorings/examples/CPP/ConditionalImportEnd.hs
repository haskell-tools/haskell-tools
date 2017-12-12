{-# LANGUAGE CPP #-}
module CPP.ConditionalImportEnd where

import CPP.B(b)
import CPP.B(b)
#ifndef USE_DATA_LIST
import CPP.A(a)
#endif

x = a

{-# LANGUAGE CPP #-}
module CPP.ConditionalImportOrder where

import CPP.C(c)
#ifndef USE_DATA_LIST
import CPP.B(b)
#endif
import CPP.A(a)

x = (a,b,c)

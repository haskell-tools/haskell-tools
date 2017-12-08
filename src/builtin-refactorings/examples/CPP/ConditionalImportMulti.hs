{-# LANGUAGE CPP #-}
module CPP.ConditionalImportMulti where

import CPP.A
#ifndef USE_DATA_LIST
import CPP.B(b)
import CPP.C(c)
#endif
import CPP.A

x = (b,c)

{-# LANGUAGE CPP #-}
module CPP.ConditionalImportHalfRemoved where


#ifndef USE_DATA_LIST
import CPP.B(b)
#endif
import CPP.C(c)

x = (b,c)

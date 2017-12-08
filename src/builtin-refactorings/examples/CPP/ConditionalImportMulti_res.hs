{-# LANGUAGE CPP #-}
module CPP.ConditionalImportMulti where


#ifndef USE_DATA_LIST
import CPP.B(b)
import CPP.C(c)
#endif


x = (b,c)

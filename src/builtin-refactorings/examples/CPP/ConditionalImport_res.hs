{-# LANGUAGE CPP #-}
module CPP.ConditionalImport where


#ifndef USE_DATA_LIST
import CPP.B(b)
#endif


x = b

{-# LANGUAGE CPP #-}
module CPP.ConditionalImportEnd where


#ifndef USE_DATA_LIST
import CPP.A(a)
#endif

x = a

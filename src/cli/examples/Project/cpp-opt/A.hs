{-# LANGUAGE CPP #-}
module A where

#ifndef MACRO
"The macro 'MACRO' defined in the cabal file is not applied."
#endif

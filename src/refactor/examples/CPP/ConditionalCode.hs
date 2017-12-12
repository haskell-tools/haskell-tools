{-# LANGUAGE CPP #-}
module CPP.ConditionalCode where

#if __GLASGOW_HASKELL__ >= 800
version = "GHC 8"
#else
version = "not GHC 8"
#endif

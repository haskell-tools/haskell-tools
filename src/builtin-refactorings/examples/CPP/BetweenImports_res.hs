{-# LANGUAGE CPP #-}
module CPP.BetweenImports where

import CPP.A(a)
#if !(MIN_VERSION_text(1,2,1))
#endif


x = a

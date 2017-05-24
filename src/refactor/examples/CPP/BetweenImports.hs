{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module CPP.BetweenImports where

import Data.List
#if !(MIN_VERSION_text(1,2,1))
#endif
import Data.Maybe

x = Just

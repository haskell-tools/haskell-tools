{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
module CPP.BetweenImports where

import Data.Maybe (Maybe(..))
#if !(MIN_VERSION_text(1,2,1))
#endif


x = Just

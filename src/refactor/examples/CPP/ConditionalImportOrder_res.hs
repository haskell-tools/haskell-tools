{-# LANGUAGE CPP #-}
module CPP.ConditionalImportOrder where

import Data.List (intersperse)
#ifndef USE_DATA_LIST
import Control.Monad (Monad(..))
#endif
import Control.Applicative ((<$>))

a = Nothing >> Nothing

b = id <$> Nothing
c = intersperse "," ["a","b"]

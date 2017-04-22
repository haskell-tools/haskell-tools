{-# LANGUAGE CPP #-}
module CPP.ConditionalImportHalfRemoved where

import Data.List
#ifndef USE_DATA_LIST
import Control.Monad ((>>))
#endif
import Control.Applicative ((<$>))

a = id <$> (Nothing >> Nothing)

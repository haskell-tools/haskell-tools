{-# LANGUAGE CPP #-}
module CPP.ConditionalImportMulti where

import Data.List
#ifndef USE_DATA_LIST
import Control.Applicative ((<$>))
import Control.Monad ((>>))
#endif
import Data.List

a = id <$> (Nothing >> Nothing)

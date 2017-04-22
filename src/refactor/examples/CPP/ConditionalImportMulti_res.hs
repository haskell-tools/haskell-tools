{-# LANGUAGE CPP #-}
module CPP.ConditionalImportMulti where


#ifndef USE_DATA_LIST
import Control.Applicative ((<$>))
import Control.Monad ((>>))
#endif


a = id <$> (Nothing >> Nothing)

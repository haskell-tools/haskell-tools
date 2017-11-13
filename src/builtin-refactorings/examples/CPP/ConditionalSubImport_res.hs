{-# LANGUAGE CPP #-}
module CPP.ConditionalSubImport where

import Data.List(
#if __GLASGOW_HASKELL__ >= 710

                sortOn
#endif
                )

a = sortOn fst [("a","x")]

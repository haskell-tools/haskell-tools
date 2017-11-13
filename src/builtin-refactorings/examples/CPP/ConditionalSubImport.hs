{-# LANGUAGE CPP #-}
module CPP.ConditionalSubImport where

import Data.List(findIndex,intersperse,nub,sort,sortBy
#if __GLASGOW_HASKELL__ >= 710
                , sortOn
#endif
                )

a = sortOn fst [("a","x")]

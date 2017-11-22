{-# LANGUAGE TemplateHaskell #-}

module Splice where

import Quote

two :: Int
two = $(add1 1) {-* TemplateHaskell *-}

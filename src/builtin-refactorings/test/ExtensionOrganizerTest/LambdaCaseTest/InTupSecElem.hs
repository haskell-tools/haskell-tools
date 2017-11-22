{-# LANGUAGE LambdaCase,
             TupleSections
             #-}

module InTupSecElem where

f = (,\case {[] -> ()}) {-* LambdaCase, TupleSections *-}

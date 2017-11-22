{-# LANGUAGE TupleSections #-}

module InCaseRhs where

f x = case x of
        [] -> (0,)   {-* TupleSections *-}
        xs -> (0,)   {-* TupleSections *-}

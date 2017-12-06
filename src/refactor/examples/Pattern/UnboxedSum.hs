{-# LANGUAGE UnboxedSums #-}
module Pattern.UnboxedSum where

f :: (# Int | Bool #) -> ()
f (# i | #) = ()
f (# | b #) = ()

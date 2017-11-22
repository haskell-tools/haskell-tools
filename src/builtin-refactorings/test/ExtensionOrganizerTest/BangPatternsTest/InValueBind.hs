{-# LANGUAGE BangPatterns #-}

module InValueBind where

((!x):xs) = [1..]   {-* BangPatterns *-}

{-# LANGUAGE ViewPatterns #-}
module Refactor.ExtractBinding.ViewPattern where

f (id . id -> x) = x

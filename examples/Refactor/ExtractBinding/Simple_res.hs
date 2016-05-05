module Refactor.ExtractBinding.Simple where

stms = map (\s -> exaggerate s) ["a", "b"]
exaggerate s = s ++ "!"
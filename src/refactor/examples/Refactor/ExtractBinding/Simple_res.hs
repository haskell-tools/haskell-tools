module Refactor.ExtractBinding.Simple where

stms = map (\s -> exaggerate s) ["a", "b"]
  where exaggerate s = s ++ "!"
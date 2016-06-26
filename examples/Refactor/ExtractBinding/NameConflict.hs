module Refactor.ExtractBinding.NameConflict where

stms = map (\s -> s ++ bang) ["a", "b"]
  where bang = "!"
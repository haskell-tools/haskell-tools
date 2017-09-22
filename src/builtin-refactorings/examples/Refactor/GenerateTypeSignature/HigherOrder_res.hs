module Refactor.GenerateTypeSignature.HigherOrder where

app :: (t1 -> t2) -> t1 -> t2
app f x = f x

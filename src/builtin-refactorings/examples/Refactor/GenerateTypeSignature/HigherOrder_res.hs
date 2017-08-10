module Refactor.GenerateTypeSignature.HigherOrder where

app :: (t1 -> t) -> t1 -> t
app f x = f x

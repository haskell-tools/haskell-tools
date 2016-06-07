module Refactor.GenerateTypeSignature.HigherOrder where

app :: (t -> t1) -> t -> t1
app f x = f x

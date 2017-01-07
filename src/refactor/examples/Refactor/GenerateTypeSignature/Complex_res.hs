module Refactor.GenerateTypeSignature.Complex where

app :: (Maybe a -> t) -> a -> t
app f x = f (Just x)

{-# LANGUAGE TypeOperators #-}
module Module.Imported where

infixr 8 :-
-- | A stack datatype. Just a better looking tuple.
data a :- b = a :- b deriving (Eq, Show)

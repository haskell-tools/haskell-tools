module Decl.LocalBindingInDo where

x :: Maybe ()
x = do let y = f a
             where a = ()
       return y
    where f = id
{-# LANGUAGE TypeOperators #-}

module InfixDataDecl where

data a :+: b = Plus a b  {-* TypeOperators *-}

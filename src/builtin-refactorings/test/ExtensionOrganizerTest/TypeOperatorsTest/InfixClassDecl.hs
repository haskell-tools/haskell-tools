{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module InfixClassDecl where

class a :?: b where  {-* MultiParamTypeClasses *-}
  f :: a -> b -> ()  {-* TypeOperators *-}

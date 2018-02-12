{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}

module NormalClassDecl where

class (:!:) a b where  {-* MultiParamTypeClasses *-}
  g :: a -> b -> ()    {-* TypeOperators *-}

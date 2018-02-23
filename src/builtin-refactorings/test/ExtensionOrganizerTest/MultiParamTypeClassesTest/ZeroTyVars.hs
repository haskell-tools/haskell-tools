{-# LANGUAGE MultiParamTypeClasses #-}

module ZeroTyVars where

class C0 where  {-* MultiParamTypeClasses *-}
  f :: ()

instance C0 where  {-* MultiParamTypeClasses *-}
  f = ()

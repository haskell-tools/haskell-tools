{-# LANGUAGE TypeOperators #-}

module InfixTypeSig where

import InfixDataDecl

h :: a :+: b -> ()  {-* TypeOperators *-}
h _ = ()

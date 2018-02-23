{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

module InfixTypeAnnot where

import InfixDataDecl
import NormalDataDecl

h :: forall a b . (:+:) a b -> (:-:) a b  {-* ExplicitForAll *-}
h (Plus x y) = Minus x y :: a :-: b  {-* TypeOperators *-}

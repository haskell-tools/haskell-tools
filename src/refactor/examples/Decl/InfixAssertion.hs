{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies #-}
module Decl.InfixAssertion where

import GHC.TypeLits

data Proxy (n :: Nat) = Proxy

divSNat :: (1 <= b) => Proxy b -> Proxy b
divSNat n = n

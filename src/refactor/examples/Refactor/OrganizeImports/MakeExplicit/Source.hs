module Refactor.OrganizeImports.MakeExplicit.Source where

a = ()
e = ()
g = ()

data A = B { b :: () } 
       | C

class D a where
  f :: a

instance D () where
  f = ()


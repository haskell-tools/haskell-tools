module Refactor.OrganizeImports.MakeExplicit.ClassSource where

class D a where
  f :: a

instance D () where 
  f = ()

g :: D a => a
g = f


module Refactor.ExtractBinding.SectionWithLocals where

a x = let y = 2
          z = 3
          (<->) = (-)
       in x <-> (y + z)
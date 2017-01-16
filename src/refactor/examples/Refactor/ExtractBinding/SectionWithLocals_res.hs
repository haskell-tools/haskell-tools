module Refactor.ExtractBinding.SectionWithLocals where

a x = let y = 2
          z = 3
          (<->) = (-)
       in f y z (<->) x
  where f y z (<->) = (<-> (y + z))
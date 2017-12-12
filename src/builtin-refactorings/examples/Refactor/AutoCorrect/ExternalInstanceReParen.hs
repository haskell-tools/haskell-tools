module Refactor.AutoCorrect.ExternalInstanceReParen where

-- The only reason why ((show 1) + 2) is not a good result is that there are no Num instance for String
x = show 1 + 2
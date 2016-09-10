module Define where

import Language.Haskell.TH

spliceTyp :: Q Type
spliceTyp = return $ TupleT 0
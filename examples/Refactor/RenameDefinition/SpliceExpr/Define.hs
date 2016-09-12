module Define where

import Language.Haskell.TH

expr :: Q Exp
expr = return $ LitE $ IntegerL 3
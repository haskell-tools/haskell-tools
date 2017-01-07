module Define where

import Language.Haskell.TH

exprSplice :: Q Exp
exprSplice = return $ LitE $ IntegerL 3
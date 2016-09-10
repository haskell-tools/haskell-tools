module Define where

import Language.Haskell.TH

typ :: Q Type
typ = return $ TupleT 0
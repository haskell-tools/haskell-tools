module TH.QuasiQuote.Define where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

expr :: QuasiQuoter
expr = QuasiQuoter { quoteExp = const (return $ VarE $ mkName "hello") }
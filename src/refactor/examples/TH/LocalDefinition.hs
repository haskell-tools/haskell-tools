{-# LANGUAGE TemplateHaskell #-}
module TH.LocalDefinition where

import Language.Haskell.TH

decls :: Q [Dec]
decls = [d| w = 3 |]

exp :: Q Exp
exp = [| 3 |]

exp' :: Q Exp
exp' = [e| 3 |]

pat :: Q Pat
pat = [p| x |]

typ :: Q Type
typ = [t| Int |]
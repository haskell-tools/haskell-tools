module Define where

import Language.Haskell.TH

hello :: Q [Dec]
hello = def "hello"

def :: String -> Q [Dec]
def name = return [ ValD (VarP (mkName name)) (NormalB $ LitE $ IntegerL 3) [] ]
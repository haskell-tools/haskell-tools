module Define where

import Language.Haskell.TH

defHello :: Q [Dec]
defHello = def "hello"

def :: String -> Q [Dec]
def name = return [ ValD (VarP (mkName name)) (NormalB $ LitE $ IntegerL 3) [] ]
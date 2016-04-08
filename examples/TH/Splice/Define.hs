module TH.Splice.Define where

import Language.Haskell.TH

def :: String -> Q [Dec]
def name = return [ ValD (VarP (mkName name)) (NormalB $ LitE $ IntegerL 3) [] ]

defHello :: Q [Dec]
defHello = def "hello"

defHello2 :: Q [Dec]
defHello2 = def "hello2"
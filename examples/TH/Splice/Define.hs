module TH.Splice.Define where

import Language.Haskell.TH

def :: String -> Q [Dec]
def name = return [ ValD (VarP (mkName name)) (NormalB $ LitE $ IntegerL 3) [] ]

defHello :: Q [Dec]
defHello = def "hello"

defHello2 :: Q [Dec]
defHello2 = def "hello2"

defHello3 :: Q [Dec]
defHello3 = def "hello3"

expr :: Q Exp
expr = return $ LitE $ IntegerL 3

typ :: Q Type
typ = return $ TupleT 0

nameOf :: Name -> Q [Dec]
nameOf n = return []

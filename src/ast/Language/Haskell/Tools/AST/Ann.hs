module Language.Haskell.Tools.AST.Ann where

data AnnList e a = AnnList { annListBefore :: String
                           , annListBetween :: String
                           , annListAfter :: String
                           , annListElements :: [Ann e a] 
                           }

newtype AnnMaybe e a = AnnMaybe (Maybe (Ann e a))
newtype AnnEither e1 e2 a = AnnEither (Either (Ann e1 a) (Ann e2 a))
  
data Ann elem annot = Ann annot (elem annot)
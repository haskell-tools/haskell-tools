module Language.Haskell.Tools.AST.Ann where

newtype AnnList e a = AnnList [Ann e a]
newtype AnnMaybe e a = AnnMaybe (Maybe (Ann e a))
newtype AnnEither e1 e2 a = AnnEither (Either (Ann e1 a) (Ann e2 a))

annJust = AnnMaybe . Just
annNothing = AnnMaybe Nothing
annRight = AnnEither . Right
annLeft = AnnEither . Left
  
data Ann elem annot = Ann annot (elem annot)
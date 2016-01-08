{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
           , StandaloneDeriving
           #-}

module Language.Haskell.Tools.AST.Ann where

newtype AnnList e a = AnnList [Ann e a]
newtype AnnMaybe e a = AnnMaybe (Maybe (Ann e a))
newtype AnnEither e1 e2 a = AnnEither (Either (Ann e1 a) (Ann e2 a))

annJust :: Ann e a -> AnnMaybe e a
annJust = AnnMaybe . Just

annNothing :: AnnMaybe e a
annNothing = AnnMaybe Nothing

annLeft :: Ann e1 a -> AnnEither e1 e2 a
annLeft = AnnEither . Left

annRight :: Ann e2 a -> AnnEither e1 e2 a
annRight = AnnEither . Right

data Ann elem annot
  = Ann { annotation   :: annot
        , element      :: elem annot
        }

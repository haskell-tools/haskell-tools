{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
           , StandaloneDeriving
           #-}

module Language.Haskell.Tools.AST.Ann where

newtype AnnList e a       = AnnList [Ann e a]
newtype AnnMaybe e a      = AnnMaybe (Maybe (Ann e a))
newtype AnnEither e1 e2 a = AnnEither (Either (Ann e1 a) (Ann e2 a))

annJust       = AnnMaybe . Just
annNothing    = AnnMaybe Nothing
annRight      = AnnEither . Right
annLeft       = AnnEither . Left
  
data Ann elem annot
  = Ann
    { element      :: elem annot
    , annotation   :: annot
    }
  deriving (Eq, Show)

deriving instance (Eq (Ann e a)) => Eq (AnnList e a)
deriving instance (Show (Ann e a)) => Show (AnnList e a)
deriving instance (Eq (Ann e a)) => Eq (AnnMaybe e a)
deriving instance (Show (Ann e a)) => Show (AnnMaybe e a)
deriving instance (Eq (Ann e1 a), Eq (Ann e2 a)) => Eq (AnnEither e1 e2 a)
deriving instance (Show (Ann e1 a), Show (Ann e2 a)) => Show (AnnEither e1 e2 a)


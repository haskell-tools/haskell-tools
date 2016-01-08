{-# LANGUAGE FlexibleContexts
           , UndecidableInstances
           , StandaloneDeriving
           #-}

module Language.Haskell.Tools.AST.Ann where

data Ann elem annot
  = Ann { annotation   :: annot
        , element      :: elem annot
        }

newtype AnnList e a = AnnList [Ann e a]
newtype AnnMaybe e a = AnnMaybe (Maybe (Ann e a))

annNil :: AnnList e a
annNil = AnnList []

annCons :: AnnList e a -> Ann e a -> AnnList e a
annCons (AnnList l) = AnnList . (:l)

annJust :: Ann e a -> AnnMaybe e a
annJust = AnnMaybe . Just

annNothing :: AnnMaybe e a
annNothing = AnnMaybe Nothing

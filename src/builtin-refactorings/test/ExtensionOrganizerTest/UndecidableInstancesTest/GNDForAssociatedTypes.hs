{-# LANGUAGE UndecidableInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}

module GNDForAssociatedTypes where

class C a where
  type T a :: *                 {-* TypeFamilies, KindSignatures *-}

newtype Loop = MkLoop Loop
  deriving C                    {-* GeneralizedNewtypeDeriving, UndecidableInstances *-}

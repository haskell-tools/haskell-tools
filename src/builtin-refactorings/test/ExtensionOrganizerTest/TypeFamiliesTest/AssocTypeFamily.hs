{-# LANGUAGE TypeFamilies #-}

module AssocTypeFamily where

class Collects ce where
  type Elem ce :: *           {-* TypeFamilies, KindSignatures *-}
  type instance Elem ce = ()  {-* TypeFamilies *-}

instance Collects [a] where
  type Elem [a] = a           {-* TypeFamilies *-}

{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module TyFamNoSmaller where

import Definitions hiding (CT)

type instance T1 a = T1 a         {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT [a] = CT [a]
  CT a   = CT a                   {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C [a] where
  type T [a] = T [[a]]            {-* UndecidableInstances, TypeFamilies *-}

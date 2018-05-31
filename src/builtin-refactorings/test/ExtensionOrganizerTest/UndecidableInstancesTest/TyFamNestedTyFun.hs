{-# LANGUAGE UndecidableInstances, TypeFamilies #-}

module TyFamNestedTyFun where

import Definitions hiding (CT)

type instance T1 [a] = T1 (T1 a)  {-* UndecidableInstances, TypeFamilies *-}

type family CT a where
  CT [[a]] = CT (CT a)
  CT a     = CT (CT a)            {-* UndecidableInstances, UndecidableInstances, TypeFamilies *-}

instance C [a] where
  type T [a] = T (T [a])          {-* UndecidableInstances, TypeFamilies *-}

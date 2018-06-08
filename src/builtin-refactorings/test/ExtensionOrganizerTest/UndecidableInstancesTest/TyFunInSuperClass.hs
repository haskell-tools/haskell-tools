{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}

module TyFunInSuperClass where

import Definitions (CT)

class CT a => C a  {-* UndecidableInstances *-}

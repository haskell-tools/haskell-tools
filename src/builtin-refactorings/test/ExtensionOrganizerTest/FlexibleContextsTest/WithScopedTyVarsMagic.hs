{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module WithScopedTyVarsMagic where

import Definitions

f :: Eq a => a -> ()
f = id fCtxt :: C [a] => a -> ()  {-* (FlexibleContexts), FlexibleContexts *-}

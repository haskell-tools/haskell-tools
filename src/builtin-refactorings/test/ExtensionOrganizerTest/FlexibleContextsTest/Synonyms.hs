{-# LANGUAGE FlexibleContexts #-}

module Synonyms where

import Definitions

f1 = fCtxt                 {-* (FlexibleContexts) *-}

f2 = fNestedCtxt           {-* (FlexibleContexts), FlexibleContexts  *-}

f3 :: Ctxt a => a -> ()    {-* FlexibleContexts, FlexibleContexts *-}
f3 = const ()              {-* FlexibleContexts *-}

f4 :: FunWCtxt a           {-* FlexibleContexts, FlexibleContexts *-}
f4 = const ()              {-* FlexibleContexts *-}

f5 :: SimpleEq a => a -> ()
f5 = const ()

f6 :: Eq a => a -> ()
f6 = id fCtxt :: FunWCtxt a    {-* (FlexibleContexts), (FlexibleContexts) *-}

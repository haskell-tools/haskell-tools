-- | Mapping between Cabal's and GHC's representation of language extensions.
module Language.Haskell.Tools.Daemon.MapExtensions
  ( module Language.Haskell.Tools.Daemon.MapExtensions 
  , module Language.Haskell.Tools.Refactor.Utils.Extensions
  ) where

import DynFlags (DynFlags, xopt_unset, xopt_set)
import Language.Haskell.TH.LanguageExtensions as GHC (Extension)
import Language.Haskell.Tools.Refactor.Utils.Extensions

-- * Not imported from DynFlags.hs, so I copied it here
setExtensionFlag', unSetExtensionFlag' :: GHC.Extension -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]
unSetExtensionFlag' f dflags = xopt_unset dflags f

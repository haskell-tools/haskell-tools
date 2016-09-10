module Refactor.RenameDefinition.DefineSplices where

import Language.Haskell.TH

nameOf :: Name -> Q [Dec]
nameOf n = return []

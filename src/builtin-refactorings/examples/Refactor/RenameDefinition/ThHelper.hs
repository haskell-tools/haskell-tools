module Refactor.RenameDefinition.ThHelper where

import Language.Haskell.TH

nameOf :: Name -> Q [Dec]
nameOf n = return []

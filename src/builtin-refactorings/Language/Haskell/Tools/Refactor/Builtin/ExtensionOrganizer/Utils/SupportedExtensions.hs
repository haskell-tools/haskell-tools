module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.SupportedExtensions where

import Language.Haskell.TH.LanguageExtensions

unregularExts :: String -> String
unregularExts "CPP" = "Cpp"
unregularExts "NamedFieldPuns" = "RecordPuns"
unregularExts "GeneralisedNewtypeDeriving" = "GeneralizedNewtypeDeriving"
unregularExts e = e

isSupported :: Extension -> Bool
isSupported = flip elem fullyHandledExtensions

fullyHandledExtensions :: [Extension]
fullyHandledExtensions = syntacticExtensions
                      ++ derivingExtensions
                      -- ++ [FlexibleInstances]

syntacticExtensions :: [Extension]
syntacticExtensions = [ RecordWildCards, TemplateHaskell, BangPatterns
                      , PatternSynonyms, TupleSections, LambdaCase, QuasiQuotes
                      , ViewPatterns, MagicHash, UnboxedTuples]

derivingExtensions :: [Extension]
derivingExtensions = [ DeriveDataTypeable, DeriveGeneric, DeriveFunctor
                     , DeriveFoldable, DeriveTraversable, DeriveLift
                     , DeriveAnyClass, GeneralizedNewtypeDeriving
                     , StandaloneDeriving, DerivingStrategies ]

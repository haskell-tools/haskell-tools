# Organize Extensions refactoring

The extensions refactoring takes a module and removes unused GHC extensions.

As a Haskell module changes, usually new GHC extensions are enabled for it, but unused extensions are rarely removed, because the need for them cannot be tested conveniently. This leads to a lot of extensions that clutter the source code and misdirect the developers. This refactoring resolves the issue automatically, removing language extensions that are not necessary.

Organize extension only removes the extensions that it can decide to be unused. Currently these are:
`RecordWildCards`, `TemplateHaskell`, `BangPatterns`
, `PatternSynonyms`, `TupleSections`, `LambdaCase`, `QuasiQuotes`
, `ViewPatterns`, `MagicHash`, `UnboxedTuples`
, `DeriveDataTypeable`, `DeriveGeneric`, `DeriveFunctor`
, `DeriveFoldable`, `DeriveTraversable`, `DeriveLift`
, `DeriveAnyClass`, `GeneralizedNewtypeDeriving`
, `StandaloneDeriving`, `DerivingStrategies`

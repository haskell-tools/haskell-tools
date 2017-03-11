# Haskell-tools overview

Haskell-tools is backed by the [GHC API](http://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-8.0.1/index.html). The GHC AST is converted into a syntax tree optimized for both analysis and modification. This also makes it easier to keep up with the latest GHC version. Aside from source information, the nodes of the syntax tree also carry semantic information, for example unique names extracted from the GHC AST.

The ht-refact tool works in a purely AST-based way. The traditional problem with purely syntax tree based transformations is that they don't keep comments or layout (it is not part of the syntax tree). We solved this problem by annotating the syntax tree with additional information regarding the original format of the source code of a given AST element. So every transformation is basically a mapping of syntax trees. This makes writing refactorings easier, since we don't have to synchronize the token streams with the syntax trees. As it can be seen most refactorings are around a hundred lines of code or shorter.

[[haskell_tools_architecture.png]]

To write the refactorings, it is possible to use [uniplate](http://hackage.haskell.org/package/uniplate) as well as a lens-like library [references](https://github.com/nboldi/references).

The most standard Haskell refactoring tool works by mapping source ranges to source related information by processing GHC's API annotations. After the transformation had been done the modified source is generated from these annotations and the modified syntax tree. This is supported by the [ghc-exactprint] (https://github.com/alanz/ghc-exactprint) package.

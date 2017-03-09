# Installation (stable release)

  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - On Linux you might need to install libz-dev and libgmp-dev (`sudo apt-get install libz-dev libgmp-dev`).

  - *Recommended*: use [stack](https://docs.haskellstack.org/en/stable/README/) for installing the project
    - `stack install haskell-tools-daemon haskell-tools-cli`
  - *Alternative*: use cabal
    - Use `cabal install haskell-tools-daemon haskell-tools-cli`
    - Because of the lack of consistent snapshots, you might need to install some dependencies with exact versions before trying to install a haskell tools package if the dependency analysis fails. You should check out the latest versions from [stackage.org](stackage.org) of the package that causes the failure. For example once I needed to pre-install `hashable-1.2.5.0`

# Installation (last build)

  - *Recommended*: use [stack](https://docs.haskellstack.org/en/stable/README/) for installing the project
    - `stack setup`
    - `stack install`
  - *Alternative*: you have to cabal-install each package of the repository in the following order: `ast`, `backend-ghc`, `prettyprint`, `rewrite`, `refactor`, `daemon`, `cli`, `demo`, `debug`.

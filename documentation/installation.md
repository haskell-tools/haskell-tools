## Installation (last release from Hackage)

  - Make sure you have the latest [haskell-platform](https://www.haskell.org/platform/)
  - On Linux you might need to install libz-dev and libgmp-dev (`sudo apt-get install libz-dev libgmp-dev`).
  - Use `cabal install haskell-tools-refactor` to install the library. Also install `haskell-tools-cli` or `haskell-tools-demo` to try the command line interface, or to setup the demo for yourself.

## Installation (last build from source)

  - *Recommended*: use [stack](https://docs.haskellstack.org/en/stable/README/) for building the project
    - `stack setup`
    - `stack build`
  - *Alternative*: you have to cabal-install each package of the repository in the following order: `ast`, `ast-ghc`, `ast-trf`, `ast-gen`, `ast-ppr`, `refactor`.

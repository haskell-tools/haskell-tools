#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

# run the self-tests
stack exec --rts-options -M3.5G -- ht-refact --no-watch --no-history --execute="Exit" demo src/ast src/backend-ghc src/builtin-refactorings src/cli src/daemon src/debug src/experimental-refactorings src/prettyprint src/refactor src/rewrite

# test the completeness of distribution packages
for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
do
  echo "Extracting the distribution of ${PKG}"
  cp `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
  rm -r src/${PKG}/*
  tar -zx -f haskell-tools-${PKG}-*.tar.gz
  mv haskell-tools-${PKG}-*/* src/${PKG}
done
echo "Running tests on the extracted folders"
stack --no-terminal test haskell-tools-rewrite
stack --no-terminal test haskell-tools-refactor
stack --no-terminal test haskell-tools-cli
stack --no-terminal test haskell-tools-daemon
stack --no-terminal test haskell-tools-demo

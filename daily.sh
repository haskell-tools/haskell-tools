#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

if [ "$TRAVIS_EVENT_TYPE" = "cron" ]; then
  for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
  do
    echo "Extracting the distribution of ${PKG}"
    cp `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
    rm -r src/${PKG}/*
    tar -zx -f haskell-tools-${PKG}-*.tar.gz
    mv haskell-tools-${PKG}-*/* src/${PKG}
  done
  echo "Running tests on the extracted folders"
  stack --no-terminal --coverage test haskell-tools-rewrite
  stack --no-terminal --coverage test haskell-tools-refactor
  stack --no-terminal --coverage test haskell-tools-cli
  stack --no-terminal --coverage test haskell-tools-daemon
  stack --no-terminal --coverage test haskell-tools-demo
fi

#if [ "$TRAVIS_EVENT_TYPE" = "cron" ];
#then
# Run benchmarks. Need to uninstall first to run this without coverage reporting, so the result will be more accurate.
#travis_wait stack --no-terminal bench haskell-tools-cli > benchmark.txt 2>&1
stack sdist
for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
do
  cp -v `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
  rm -v -r src/${PKG}/*
  tar -zxf haskell-tools-${PKG}-*.tar.gz -C src/${PKG}
done
stack --no-terminal test --coverage haskell-tools-rewrite
stack --no-terminal test --coverage haskell-tools-refactor
stack --no-terminal test --coverage haskell-tools-cli
stack --no-terminal test --coverage haskell-tools-daemon
stack --no-terminal test --coverage haskell-tools-demo
#fi

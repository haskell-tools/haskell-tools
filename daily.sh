#if [ "$TRAVIS_EVENT_TYPE" = "cron" ];
#then
# Run benchmarks. Need to uninstall first to run this without coverage reporting, so the result will be more accurate.
#travis_wait stack --no-terminal bench haskell-tools-cli > benchmark.txt 2>&1
stack sdist
for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
do
  echo "Testing distribution of ${PKG}"
  cp -v `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
  rm -r src/${PKG}/*
  tar -zxf haskell-tools-${PKG}-*.tar.gz -C src/${PKG}
done
echo "Running tests on the extracted folders"
stack --no-terminal test haskell-tools-rewrite
stack --no-terminal test haskell-tools-refactor
stack --no-terminal test haskell-tools-cli
stack --no-terminal test haskell-tools-daemon
stack --no-terminal test haskell-tools-demo
#fi

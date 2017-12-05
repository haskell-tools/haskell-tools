#!/bin/bash

benchmarks="NoBenchmarksWereGiven"

if [[ $@ == "" ]]
then
  benchmarks=""
else
  benchmarks=$@
fi

stack bench --benchmark-arguments "Traversal/$benchmarks --time-limit 10 --output ExtensionOrganizerBenchmark.html" haskell-tools-builtin-refactorings:ht-extension-organizer-benchmark

#!/bin/bash

benchmarks="NoBenchmarksWereGiven"

if [[ $@ == "" ]]
then
  benchmarks=""
else
  benchmarks=$@
fi

stack bench --benchmark-arguments "ExtensionOrganizerBenchmark/$benchmarks --output ExtensionOrganizerBenchmark.html" haskell-tools-builtin-refactorings:ht-extension-organizer-benchmark

#!/bin/bash

tests="NoTestsWereGiven"

if [[ $@ == "" ]]
then
  tests="ExtensionOrganizerTest"
else
  tests=$@
fi

stack test  --test-arguments="-p $tests" haskell-tools-builtin-refactorings:ht-extension-organizer-test

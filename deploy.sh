#!/bin/bash

# Pull requests shouldn't try to deploy
if [ "$TRAVIS_EVENT_TYPE" = "pull_request" ]; then
    echo "Pull request, skipping deploy"
    exit 0
fi

echo "Starting deploy"

# Decrypt the private key using travis encrypted file service
openssl aes-256-cbc -K $encrypted_961cdb62d58f_key -iv $encrypted_961cdb62d58f_iv -in deploykey.enc -out deploykey -d
chmod 600 deploykey

# Start an ssh session and add the private key
eval `ssh-agent -s`
ssh-add deploykey

# Clone the existing repo into out/. Download only the files for the given folder
mkdir out
git clone -b master git@github.com:haskell-tools/haskell-tools.github.io out

# Remove folders from the github.io repo that has no corresponding active branches
git ls-remote --heads https://github.com/haskell-tools/haskell-tools.git | grep -o -E "[a-zA-Z0-9-]+$" > branches.txt
find out -maxdepth 1 -type d -path "./*" -exec sh -c \
    'for f; do f=${f#./}; grep -qw "$f" branches.txt || rm -rf "$f"; done' sh {} +

# Publish api and coverage info on pushes
if [ "$TRAVIS_EVENT_TYPE" = "push" ]; then
    # Clean out existing contents
    rm -rf out/$TRAVIS_BRANCH/api/**
    rm -rf out/$TRAVIS_BRANCH/coverage/**

    # Move generated haddock documentation
    mkdir -p out/$TRAVIS_BRANCH/api
    mv .stack-work/install/x86_64-linux/*/*/doc/* out/$TRAVIS_BRANCH/api

    # Move the test coverage report
    mkdir -p out/$TRAVIS_BRANCH/coverage
    mv .stack-work/install/x86_64-linux/*/*/hpc/combined/all/* out/$TRAVIS_BRANCH/coverage
fi

# On nightly, publish the benchmark results
if [ "$TRAVIS_EVENT_TYPE" = "cron" ]; then
  # Copy the benchmark report
  mv -f benchmark.txt out/$TRAVIS_BRANCH/benchmark.txt
fi

# Create an index page
mv branch-info-index.html out/$TRAVIS_BRANCH/index.html

cd out

git config user.name "Travis CI"
git config user.email "nboldi@caesar.elte.hu"

git add -A .
git commit -m "Updating API documentation for: https://github.com/haskell-tools/haskell-tools/commit/$TRAVIS_COMMIT"
git push

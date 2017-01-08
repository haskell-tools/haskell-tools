#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

# Pull requests and commits to other branches shouldn't try to deploy
if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
    echo "Skipping deploy"
    exit 0
fi

echo "Starting deploy"

# Decrypt the private key using travis encrypted file service
openssl aes-256-cbc -K $encrypted_961cdb62d58f_key -iv $encrypted_961cdb62d58f_iv -in deploykey.enc -out deploykey -d
chmod 600 deploykey

# Start an ssh session and add the private key
eval `ssh-agent -s`
ssh-add deploykey

# Clone the existing repo into out/
git clone git@github.com:haskell-tools/haskell-tools.github.io out

# Clean out existing contents
rm -rf out/$TRAVIS_BRANCH/api/**
rm -rf out/$TRAVIS_BRANCH/coverage/**

# Copy generated haddock documentation

mkdir -p out/$TRAVIS_BRANCH/api
cp -r .stack-work/install/x86_64-linux/*/*/doc/* out/$TRAVIS_BRANCH/api

# Copy the test coverage report

mkdir -p out/$TRAVIS_BRANCH/coverage
cp -r .stack-work/install/x86_64-linux/*/*/hpc/combined/all/* out/$TRAVIS_BRANCH/coverage

# Copy the benchmark report
cp benchmark.txt out/$TRAVIS_BRANCH/benchmark.txt

# Create an index page
cp branch-info-index.html out/$TRAVIS_BRANCH/index.html

cd out

git config user.name "Travis CI"
git config user.email "nboldi@caesar.elte.hu"
git config push.default simple

git add -A .
git commit -m "Updating API documentation for: https://github.com/haskell-tools/haskell-tools/commit/$TRAVIS_COMMIT"
git push -f
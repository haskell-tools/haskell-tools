#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

SOURCE_BRANCH="master"

if ! [ -n "$GITHUB_API_KEY" ]; then
    echo "No API key given, skipping deploy"
    exit 0
fi

# Pull requests and commits to other branches shouldn't try to deploy, just build to verify
# if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_BRANCH" != "master" ]; then
#     echo "Skipping deploy"
#     exit 0
# fi

echo "Starting deploy"

# Clone the existing gh-pages for this repo into out/
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deply)
git clone https://github.com/haskell-tools/haskell-tools.github.io out

# Clean out existing contents
rm -rf out/**/* || exit 0

# Copy generated haddock documentation
cp -r .stack-work/install/x86_64-linux/nightly-2016-09-10/8.0.1/doc out/api
cd out

git config user.name "Travis CI"
git config user.email "nboldi@elte.hu"
git config push.default simple

git add .
git commit -m "Updating the API documentation"

echo "$GITHUB_API_KEY" | git push -f https://haskell-tools-deploy@github.com/haskell-tools/haskell-tools.github.io.git
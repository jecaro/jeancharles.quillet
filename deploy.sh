#!/usr/bin/env bash

set -o xtrace
set -o nounset
set -o errexit

# Check that the directory is clean
# https://unix.stackexchange.com/a/394674
git diff-index --quiet HEAD

# Build the site
cabal run site -- clean
cabal run site -- build

# Check that the links are good
cabal run site -- check

# Upload on the server
rsync -avh --delete _site/ jeancharles.quillet.org:site

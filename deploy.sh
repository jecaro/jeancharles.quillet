#!/usr/bin/env bash

set -o xtrace
set -o nounset
set -o errexit

cabal run site -- clean
cabal run site -- build
cabal run site -- check

rsync -avh --delete _site/ jeancharles.quillet.org:site

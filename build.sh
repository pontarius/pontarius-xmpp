#!/bin/sh
git submodule init
git submodule update
cabal-dev install ./xml-types-pickle
cabal-dev install ./stringprep-hs
cabal-dev install-deps
cabal-dev configure
cabal-dev build

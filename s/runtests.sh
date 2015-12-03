#!/bin/sh

BASEDIR=$(cd "$(dirname "$0")"/..; pwd -P)
cd $BASEDIR

cabal install --enable-tests --only-dependencies
cabal test

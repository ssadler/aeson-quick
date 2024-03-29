#!/bin/bash

function publish () {
  set -e

  dir=$(mktemp -d dist-docs.XXXXXX)
  trap 'rm -r "$dir"' EXIT

  # assumes cabal 2.4 or later
  cabal v2-haddock --builddir="$dir" --haddock-for-hackage --enable-doc

  cabal upload -d --publish $dir/*-docs.tar.gz
}


cmd=$1
shift
eval $cmd

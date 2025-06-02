#!/bin/bash

set -e
shopt -s globstar

protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --proto_path=proto \
    --haskell_out=./src \
    --haskell_opt='Opt{ imports = [], pragmas = ["DeriveGeneric"], stockInstances = ["GHC.Generics.Generic"], defaultInstances = [] }' \
	./proto/**/*.proto

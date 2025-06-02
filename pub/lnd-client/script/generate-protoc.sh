#!/bin/bash

set -e
shopt -s globstar

protoc \
    --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
    --proto_path=proto \
    --haskell_out=./src \
    --haskell_opt='Opt{ imports = [], pragmas = ["DeriveDataTypeable", "DeriveGeneric"], stockInstances = ["Data.Data.Data", "GHC.Generics.Generic"], defaultInstances = [] }' \
	./proto/**/*.proto

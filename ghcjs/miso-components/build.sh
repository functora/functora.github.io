#!/usr/bin/env bash

set -e

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
else
    echo "Building for prod"
    dev_mode=false
fi

rm -rf dist
mkdir dist
cp ./*.html dist/

if command -v wasm32-wasi-cabal &>/dev/null; then
    wasm32-wasi-cabal build miso-components
else
    cabal \
        --with-compiler=wasm32-wasi-ghc \
        --with-hc-pkg=wasm32-wasi-ghc-pkg \
        --with-hsc2hs=wasm32-wasi-hsc2hs \
        build miso-components
fi

hs_wasm_path=$(find . -name "*.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
     --input "$hs_wasm_path" --output ghc_wasm_jsffi.js

if $dev_mode; then
    cp "$hs_wasm_path" dist/bin.wasm
else
    wizer --allow-wasi --wasm-bulk-memory true --init-func _initialize -o dist/bin.wasm "$hs_wasm_path"
    wasm-opt ${1+"$@"} dist/bin.wasm -o dist/bin.wasm
    wasm-tools strip -o dist/bin.wasm dist/bin.wasm
fi

cp ./*.js dist

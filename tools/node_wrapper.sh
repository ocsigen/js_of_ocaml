#!/bin/sh
export PATH=$(echo $PATH | cut -d : -f 2-) # Do not call oneself recursively
exec node --experimental-wasm-exnref --experimental-wasm-imported-strings --experimental-wasm-stack-switching --stack-size=10000 "$@"

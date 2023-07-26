#!/bin/sh
export PATH=$(echo $PATH | cut -d : -f 2-) # Do not call oneself recursively
exec node --experimental-wasm-stringref --experimental-wasm-gc --experimental-wasm-stack-switching "$@"

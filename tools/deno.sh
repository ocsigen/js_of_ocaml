#!/bin/sh
export PATH=$(echo $PATH | cut -d : -f 2-) # Do not call oneself recursively
exec deno run --allow-env --allow-read "$@"

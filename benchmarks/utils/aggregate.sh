#!/bin/sh
TEMP=$(mktemp)

tee $TEMP | jq '.name |= "Js vs Wasm" | .results[].metrics[].name |= .+"/'$1'"'
cat $TEMP
rm $TEMP

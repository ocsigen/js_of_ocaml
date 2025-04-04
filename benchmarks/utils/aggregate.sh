#!/bin/sh
TEMP=$(mktemp)

tee $TEMP | \
jq '.name |= "Js vs Wasm"' | \
jq '.results[] |= select(.name | test("Code size") | not)' |\
jq '.results[].metrics[] |= select(.name as $n | ["kb", "boyer", "fannkuch_redux_2"] | index($n) | not)' | \
jq '.results[].metrics[].name |= if test("/") then .+"-'$1'" else .+"/'$1'" end'
cat $TEMP
rm $TEMP

#!/bin/sh

if [ "$1" = "exec" ]; then
    jq '{name: ((.compiler[:1] | ascii_upcase) + .compiler[1:]), results: [{name: "'$NAME'", metrics: [{name: "Execution time", "units":"s", value: (.time | split(":") | map(tonumber) | .[0] * 60 + .[1])}]}]}'
else
    jq '{name: ((.compiler[:1] | ascii_upcase) + .compiler[1:]), results: [{name: "'$NAME'", metrics: [{name: "Compilation time", "units":"s", value: (.time | split(":") | map(tonumber) | .[0] * 60 + .[1])}, {name: "Memory usage", "units":"KiB", value:(.mem)}]}]}'
fi

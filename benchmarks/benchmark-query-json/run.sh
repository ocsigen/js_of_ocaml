#!/bin/bash
# usage: run.sh <script> <data directory>
set -e
node "$1" . "$2/big.json"
node "$1" 'map(.)' "$2/big.json"
node "$1" 'filter(.base."Attack" > 100) | map(.name.english)' "$2/big.json"
node "$1" keys "$2/huge.json"

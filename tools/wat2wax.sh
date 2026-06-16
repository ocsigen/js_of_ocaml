#!/bin/bash
# Convert every Wasm runtime `.wat` file to its `.wax` counterpart.
#
# Wax (https://github.com/vouillon/wax/) offers a Rust-like syntax for
# WebAssembly together with a converter between the wax, wat and wasm
# formats. This script translates each runtime/wasm/*.wat file into a
# `.wax` file written to a separate output directory, leaving the source
# tree untouched. It is used by CI to check that all the Wasm runtime
# files can be converted to wax.
#
# Usage: tools/wat2wax.sh [OUTPUT_DIR]
#   OUTPUT_DIR defaults to _build/wax
#
# Set WAX to override the wax binary (default: `wax`, or `opam exec -- wax`
# when wax is not directly on PATH).
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/runtime/wasm"
OUT="${1:-$ROOT/_build/wax}"

if [ -n "${WAX:-}" ]; then
  wax() { command $WAX "$@"; }
elif command -v wax >/dev/null 2>&1; then
  wax() { command wax "$@"; }
else
  wax() { opam exec -- wax "$@"; }
fi

mkdir -p "$OUT"

errors=0
count=0

echo "Converting $SRC/*.wat to wax in $OUT ..."
echo

for wat in "$SRC"/*.wat; do
  [ -e "$wat" ] || continue
  count=$((count + 1))
  name="$(basename "$wat" .wat)"
  dst="$OUT/$name.wax"
  if wax -i wat -f wax -o "$dst" "$wat" 2>"$OUT/$name.err"; then
    echo "✓ $name.wat -> $name.wax"
    rm -f "$OUT/$name.err"
  else
    echo "✗ $name.wat - CONVERSION FAILED"
    sed 's/^/    /' "$OUT/$name.err"
    errors=$((errors + 1))
  fi
done

echo
if [ "$errors" -eq 0 ]; then
  echo "All $count Wasm runtime files converted to wax."
  exit 0
else
  echo "$errors of $count file(s) failed to convert."
  exit 1
fi

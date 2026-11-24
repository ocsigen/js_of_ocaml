#!/bin/bash
# Convert every Wasm runtime `.wat` file to its `.wax` counterpart.
#
# Wax (https://github.com/vouillon/wax/) offers a Rust-like syntax for
# WebAssembly together with a converter between the wax, wat and wasm
# formats. This script translates each runtime/wasm/*.wat file into a
# `.wax` file. The generated files are committed in runtime/wasm/wax so
# that the runtime can be read (and reviewed) in wax syntax; the `.wat`
# files remain the source of truth. CI runs this script with `--check`
# to make sure the committed files are up to date.
#
# Usage: tools/wat2wax.sh [--check] [OUTPUT_DIR]
#   OUTPUT_DIR defaults to runtime/wasm/wax
#   --check    do not touch OUTPUT_DIR; convert to a temporary directory
#              and fail if the result differs from OUTPUT_DIR
#
# The output depends on the version of wax: use the one installed by the
# lint-wax job in .github/workflows/lint.yml.
#
# Set WAX to override the wax binary (default: `wax`, or `opam exec -- wax`
# when wax is not directly on PATH).
set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/runtime/wasm"

check=false
if [ "${1:-}" = "--check" ]; then
  check=true
  shift
fi

REF="${1:-$SRC/wax}"

if [ -n "${WAX:-}" ]; then
  wax() { command $WAX "$@"; }
elif command -v wax >/dev/null 2>&1; then
  wax() { command wax "$@"; }
else
  wax() { opam exec -- wax "$@"; }
fi

if $check; then
  OUT="$(mktemp -d)"
  trap 'rm -rf "$OUT"' EXIT
else
  OUT="$REF"
  mkdir -p "$OUT"
  # Drop the files whose source has been removed.
  for wax in "$OUT"/*.wax; do
    [ -e "$wax" ] || continue
    [ -e "$SRC/$(basename "$wax" .wax).wat" ] || rm -f "$wax"
  done
fi

errors=0
count=0

echo "Converting $SRC/*.wat to wax in $OUT ..."
echo

for wat in "$SRC"/*.wat; do
  [ -e "$wat" ] || continue
  count=$((count + 1))
  name="$(basename "$wat" .wat)"
  dst="$OUT/$name.wax"
  if wax -i wat -f wax -v -W all=error -o "$dst" "$wat" 2>"$OUT/$name.err"; then
    echo "✓ $name.wat -> $name.wax"
    rm -f "$OUT/$name.err"
  else
    echo "✗ $name.wat - CONVERSION FAILED"
    sed 's/^/    /' "$OUT/$name.err"
    errors=$((errors + 1))
  fi
done

echo
if [ "$errors" -ne 0 ]; then
  echo "$errors of $count file(s) failed to convert."
  exit 1
fi
echo "All $count Wasm runtime files converted to wax."

if $check; then
  if ! diff -r -u "$REF" "$OUT"; then
    echo
    echo "The wax files in ${REF#"$ROOT"/} are out of sync with the wat files."
    echo "Run tools/wat2wax.sh and commit the result."
    exit 1
  fi
  echo "The wax files in ${REF#"$ROOT"/} are up to date."
fi

#!/bin/bash
# Count the representation conversions (box/unbox/tag/untag) executed by
# the benchmarks when compiled with wasm_of_ocaml, at --opt 1 and 2, in
# one or several configurations.
#
# Usage: count.sh OUTDIR
#
# Writes OUTDIR/results.txt, one line per program, --opt level and
# configuration followed by the counts, then prints a summary table.
# Also checks that every build produces the same output and exit code as
# ocamlrun.
#
# Environment: WASM_OF_OCAML (compiler, default: the in-tree build),
# CONFIGS (configurations to compare, see run_one.sh), JOBS (parallel
# jobs, default 8). Binaryen and node must be in PATH.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
OUT=${1:?usage: count.sh OUTDIR}
export WASM_OF_OCAML=${WASM_OF_OCAML:-$ROOT/_build/default/compiler/bin-wasm_of_ocaml/wasm_of_ocaml.exe}
mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)
"$HERE/build.sh" "$ROOT" "$OUT"
ls "$OUT/bytes" | sed -n 's/\.byte$//p' |
  xargs -P "${JOBS:-8}" -n1 "$HERE/run_one.sh" "$OUT" > "$OUT/results.txt"
python3 "$HERE/table.py" "$OUT/results.txt"

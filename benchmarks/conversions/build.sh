#!/bin/bash
# build.sh ROOT OUT — build the bytecode of each benchmark into OUT/bytes,
# with the flags of the benchmark harness (no -g). Sources are copied to
# OUT/src so that the object files are not written into the repository.
set -e
ROOT=$1
OUT=$2
B=$ROOT/benchmarks
mkdir -p "$OUT/bytes" "$OUT/src/ml" "$OUT/src/bigarrays" "$OUT/src/g2pp"
cp "$B"/sources/ml/*.ml "$OUT/src/ml/"
for f in "$OUT"/src/ml/*.ml; do
  ocamlc "$f" -o "$OUT/bytes/$(basename "$f" .ml).byte"
done
cp "$B/benchmark-others/bigarrays/bench.ml" "$OUT/src/bigarrays/"
ocamlc "$OUT/src/bigarrays/bench.ml" -o "$OUT/bytes/bigarrays.byte"
cp "$B"/benchmark-others/lexifi-g2pp/*.ml "$B"/benchmark-others/lexifi-g2pp/*.mli \
  "$OUT/src/g2pp/"
(cd "$OUT/src/g2pp" &&
   ocamlc date.mli date.ml math.mli math.ml optimization.mli optimization.ml \
     g2pp_calibration.mli g2pp_calibration.ml main.ml -o "$OUT/bytes/lexifi_g2pp.byte")

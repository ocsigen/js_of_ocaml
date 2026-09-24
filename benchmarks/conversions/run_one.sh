#!/bin/bash
# run_one.sh OUT NAME — compile OUT/bytes/NAME.byte at --opt 1 and 2, in
# each configuration of CONFIGS, with conversion counting. Print one line
# per build: "NAME OPT CONFIG COUNTS". Report on stderr the builds whose
# output or exit code differ from ocamlrun.
#
# CONFIGS is a space-separated list of NAME[:FLAGS], the flags being
# separated by commas (default: "default", with no extra flag). For
# instance: CONFIGS="range norange:--disable,int-range".
OUT=$1
n=$2
d=$OUT/wasm/$n
mkdir -p "$d"
ref=$( (cd "$d" && ocamlrun "$OUT/bytes/$n.byte" 2> /dev/null; echo "exit=$?") | md5sum)
for o in 1 2; do
  for config in ${CONFIGS:-default}; do
    mode=${config%%:*}
    extra=""
    [ "$config" != "$mode" ] && extra=${config#*:}
    flags="--opt=$o --debug count-conversions ${extra//,/ }"
    js=$o.$mode.js
    # shellcheck disable=SC2086 # $flags is a list of options
    if ! "$WASM_OF_OCAML" -q $flags "$OUT/bytes/$n.byte" -o "$d/$js" 2> "$d/$o.$mode.err"; then
      echo "$n $o $mode COMPILE-FAILED"
      continue
    fi
    (cd "$d" && node "$js" > "$o.$mode.stdout" 2> "$o.$mode.stderr"; echo "exit=$?" >> "$o.$mode.stdout")
    [ "$(md5sum < "$d/$o.$mode.stdout")" = "$ref" ] || echo "$n $o $mode: output differs from ocamlrun" >&2
    counts=$(sed -n 's/^conversions: //p' "$d/$o.$mode.stderr")
    echo "$n $o $mode $counts"
  done
done

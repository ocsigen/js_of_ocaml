#!/bin/bash
# run_one.sh OUT NAME — compile OUT/bytes/NAME.byte at --opt 1 and 2, with
# LCM enabled and disabled, with conversion counting. Print one line per
# build: "NAME OPT MODE COUNTS". Report on stderr the builds whose output
# or exit code differ from ocamlrun.
OUT=$1
n=$2
d=$OUT/wasm/$n
mkdir -p "$d"
ref=$( (cd "$d" && ocamlrun "$OUT/bytes/$n.byte" 2> /dev/null; echo "exit=$?") | md5sum)
for o in 1 2; do
  for mode in lcm nolcm; do
    flags="--opt=$o --debug count-conversions"
    [ $mode = nolcm ] && flags="$flags --disable lcm"
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

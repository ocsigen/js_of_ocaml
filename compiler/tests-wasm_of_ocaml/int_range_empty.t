A comparison which cannot hold for a 31-bit integer, such as
`x > max_int`, constrains the variable to an empty range: the code it
guards is unreachable, and its ranges must be empty (`Bot`), not empty
intervals, which `cannot_overflow` would accept. `x - 0` and `x + 0` keep
the bounds of the constrained range; `x land 0xff` checks that the
analysis is performed, which is only the case from `--opt 2`.

  $ cat > prog.ml <<'EOF2'
  > let above x = if x > 0x3fffffff then x - 0 else x
  > 
  > let below x = if x < -0x40000000 then x + 0 else x
  > 
  > let low x = x land 0xff
  > EOF2
  $ ocamlc -c prog.ml
  $ wasm_of_ocaml compile --opt 2 --debug int-range prog.cmo -o prog.wasmo 2>&1 \
  >   | awk -F'[][, ]+' '/: \[/ { if ($2 + 0 > $3 + 0) n++; else if ($2 == 0 && $3 == 255) m++ }
  >                      END { print "empty intervals:", n + 0, "- [0, 255]:", m + 0 }'
  empty intervals: 0 - [0, 255]: 1

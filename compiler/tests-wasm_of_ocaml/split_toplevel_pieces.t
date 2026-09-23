Check that the toplevel function of the split tests is actually split: their
expected output alone would not notice if nothing was outlined any more. The
exact number of pieces depends on the OCaml version (the toplevel function
also initializes the standard library), so we only check that there are many.

  $ ocamlc -g split_toplevel.ml -o split_toplevel.bc
  $ wasm_of_ocaml --set toplevel_split_size=5 --debug split split_toplevel.bc -o split_toplevel.js 2>&1 \
  >   | awk '/split into/ { print ($5 >= 10 ? "split" : "not split (" $5 " pieces)") }'
  split
  $ ocamlc -g split_toplevel_switch.ml -o split_toplevel_switch.bc
  $ wasm_of_ocaml --set toplevel_split_size=5 --debug split split_toplevel_switch.bc -o split_toplevel_switch.js 2>&1 \
  >   | awk '/split into/ { print ($5 >= 10 ? "split" : "not split (" $5 " pieces)") }'
  split

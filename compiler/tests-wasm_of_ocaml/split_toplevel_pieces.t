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

A function called under an exception handler signals exceptions by
returning null, and its callers then branch to the enclosing handler. At
toplevel, this handler wraps the whole function, but each call to [check]
can still be outlined into its own function. The exception raised by the
last one must still reach the handler, and be reported as uncaught.

  $ cat > raising.ml << EOF
  > let results = ref []
  > let () = at_exit (fun () -> List.iter print_endline (List.rev !results))
  > let record s = results := s :: !results
  > let check s = if s = "" then raise Exit else s ^ "!"
  > let () = record (try check "" with Exit -> "caught")
  > let () = record (check "a")
  > let () = record (check "b")
  > let () = record (check "c")
  > let () = record (check "")
  > let () = record "not reached"
  > EOF
  $ ocamlc -g raising.ml -o raising.bc
  $ wasm_of_ocaml --set toplevel_split_size=5 --debug split raising.bc -o raising.js 2>&1 \
  >   | awk '/split into/ { print ($5 >= 10 ? "split" : "not split (" $5 " pieces)") }'
  split
  $ node raising.js
  caught
  a!
  b!
  c!
  Fatal error: exception Stdlib.Exit
  [2]

Wasm assets are located relative to the runtime file, not the process entry
point, so a program still finds them when it is loaded through `require` (e.g.
behind a CLI wrapper) rather than run directly.

  $ echo 'let () = print_string "ok\n"' > prog.ml
  $ ocamlc prog.ml -o prog.bc
  $ dune exec -- wasm_of_ocaml prog.bc -o prog.js

Run directly:

  $ node prog.js
  ok

Load through a wrapper in another directory, so `require.main` is the wrapper
rather than the compiled program:

  $ mkdir sub
  $ echo 'require("../prog.js")' > sub/wrapper.js
  $ node sub/wrapper.js
  ok

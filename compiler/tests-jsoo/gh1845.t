
  $ echo 'prerr_endline "test"' > test.ml

  $ ocamlc test.ml -o test.bc

  $ dune exec -- js_of_ocaml test.bc -o test.js

  $ node test.js
  test

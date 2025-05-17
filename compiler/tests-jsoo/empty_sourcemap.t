  $ echo 'prerr_endline "a"' > a.ml
  $ echo 'prerr_endline "b"' > b.ml
  $ ocamlc -g a.ml -c
  $ ocamlc -g b.ml -c
  $ ocamlc -g a.cmo b.cmo -o test.bc

Build object files and executable with --empty-sourcemap:

  $ dune exec -- js_of_ocaml --sourcemap --empty-sourcemap a.cmo -o a.js
  $ cat a.map
  {"version":3,"file":"a.js","sources":[],"sourcesContent":[],"names":[],"mappings":""}
  $ dune exec -- js_of_ocaml --sourcemap --empty-sourcemap b.cmo -o b.js
  $ cat b.map
  {"version":3,"file":"b.js","sources":[],"sourcesContent":[],"names":[],"mappings":""}
  $ dune exec -- js_of_ocaml --sourcemap --empty-sourcemap test.bc -o test.js
  $ cat test.map
  {"version":3,"file":"test.js","sources":[],"sourcesContent":[],"names":[],"mappings":""}

Build object files with sourcemap and link with --empty-sourcemap:

  $ dune exec -- js_of_ocaml --sourcemap a.cmo -o a.js
  $ dune exec -- js_of_ocaml --sourcemap b.cmo -o b.js
  $ dune exec -- js_of_ocaml link --sourcemap --resolve-sourcemap-url=true --empty-sourcemap a.js b.js -o test.js -a
  $ cat test.map
  {"version":3,"file":"test.js","sections":[]}

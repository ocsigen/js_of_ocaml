From OCaml 5.6, the bytecode carries the kind and layout of the bigarray
at each access, as known by the type checker. The checks of the accesses to
a C layout bigarray can then be removed even when the bigarray is not
created in the function, as for the parameters below.

  $ cat > removed.ml <<'EOF2'
  > open Bigarray
  > 
  > let sum (b : (int, int8_unsigned_elt, c_layout) Array1.t) =
  >   let r = ref 0 and i = ref 0 in
  >   while !i < Array1.dim b do
  >     r := !r + b.{!i};
  >     incr i
  >   done;
  >   !r
  > 
  > let fill (b : (float, float64_elt, c_layout) Array1.t) =
  >   let i = ref 0 in
  >   while !i < Array1.dim b do
  >     b.{!i} <- 1.;
  >     incr i
  >   done
  > EOF2
  $ cat > kept.ml <<'EOF2'
  > open Bigarray
  > 
  > let too_far (b : (int, int8_unsigned_elt, c_layout) Array1.t) =
  >   let r = ref 0 and i = ref 0 in
  >   while !i <= Array1.dim b do
  >     r := !r + b.{!i};
  >     incr i
  >   done;
  >   !r
  > 
  > let fortran (b : (int, int8_unsigned_elt, fortran_layout) Array1.t) =
  >   let r = ref 0 and i = ref 0 in
  >   while !i < Array1.dim b do
  >     r := !r + b.{!i};
  >     incr i
  >   done;
  >   !r
  > 
  > let any_layout (b : (int, int8_unsigned_elt, 'l) Array1.t) =
  >   let r = ref 0 and i = ref 0 in
  >   while !i < Array1.dim b do
  >     r := !r + b.{!i};
  >     incr i
  >   done;
  >   !r
  > EOF2
  $ ocamlc -c removed.ml kept.ml
  $ wasm_of_ocaml compile --opt 2 --debug stats removed.cmo -o removed.wasmo 2>&1 | grep 'bound checks'
  Stats - bound checks removed: 0 arrays, 0 strings, 2 bigarrays
  $ wasm_of_ocaml compile --opt 2 --debug stats kept.cmo -o kept.wasmo 2>&1 | grep 'bound checks'
  Stats - bound checks removed: 0 arrays, 0 strings, 0 bigarrays

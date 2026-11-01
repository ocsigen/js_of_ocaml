With separate compilation, a compilation unit records the shapes of the
values it exports (the arity of its functions), so that units using it can
call these functions directly instead of going through a generic
[caml_callN]-like helper (here, [$apply_2]).

  $ cat > a.ml << EOF
  > let f x y = x + y
  > EOF
  $ cat > b.ml << EOF
  > let () = print_int (A.f 1 2)
  > EOF
  $ ocamlc -c a.ml
  $ ocamlc -c b.ml
  $ wasm_of_ocaml compile a.cmo -o a.wasmo

Without the shape of [A], the arity of [A.f] is unknown.

  $ wasm_of_ocaml compile --debug wat b.cmo -o b.wasmo
  $ grep -c 'call \$apply_2' B.wat
  1

With it, [A.f] is called directly.

  $ wasm_of_ocaml compile --debug wat --load-shape a.wasmo b.cmo -o b.wasmo
  $ grep -c 'call \$apply_2' B.wat
  0
  [1]

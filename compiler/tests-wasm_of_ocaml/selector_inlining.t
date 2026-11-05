With Wasm, a function which only selects a value according to its
argument (a switch whose branches each return a constant or a field) is
inlined, even when it is larger than a trivial function and called
several times.

  $ cat > prog.ml <<EOF
  > type t = { a : int; b : int; c : int; d : int }
  > type k = A | B | C | D
  > let select t k = match k with A -> t.a | B -> t.b | C -> t.c | D -> t.d
  > let t = { a = 1; b = 2; c = 3; d = 4 }
  > let () = List.iter (fun k -> print_int (select t k)) [ A; B; C; D ]
  > let () =
  >   print_int (select t (if Array.length Sys.argv > 5 then A else D));
  >   print_newline ()
  > EOF
  $ ocamlc -g prog.ml -o prog.bc
  $ wasm_of_ocaml --opt 2 --pretty --debug inlining prog.bc -o prog.js 2>&1 |
  > sed -n 's/^.* <- v[0-9]*{select}(select): \([a-z]*\) uses:\([0-9]*\).*/\1 uses:\2/p' | sort -u
  true uses:1
  true uses:2
  $ node prog.js
  12344

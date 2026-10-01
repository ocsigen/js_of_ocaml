The range analysis removes the bound checks of the accesses whose index is
known to be valid, and only those. The accesses of `bound_checks.ml` are
checked at run time; here we count the checks which are removed. Only a
single compilation unit is compiled, so that the count does not depend on
the standard library. The analysis is only performed from `--opt 2`.

  $ cat > prog.ml <<'EOF2'
  > (* Removed *)
  > let sum a =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a - 1 do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let sum_float (a : float array) =
  >   let r = ref 0. and i = ref 0 in
  >   while !i < Array.length a do
  >     r := !r +. a.(!i);
  >     incr i
  >   done;
  >   !r
  > 
  > let count s =
  >   let n = ref 0 in
  >   for i = 0 to String.length s - 1 do
  >     if s.[i] = 'e' then incr n
  >   done;
  >   !n
  > 
  > let get a pos = if pos < 0 || pos >= Array.length a then 0 else a.(pos)
  > 
  > let iter f a =
  >   for i = 0 to Array.length a - 1 do
  >     f a.(i)
  >   done
  > 
  > (* Kept *)
  > let too_far a =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a do
  >     r := !r + a.(i)
  >   done;
  >   !r
  > 
  > let other_array a b =
  >   let r = ref 0 in
  >   for i = 0 to Array.length a - 1 do
  >     r := !r + b.(i)
  >   done;
  >   !r
  > 
  > let string_too_far s pos = if pos <= String.length s then s.[pos] else ' '
  > 
  > let decremented a =
  >   let c = ref 0 in
  >   let dec = Sys.opaque_identity (fun () -> decr c) in
  >   dec ();
  >   if !c < Array.length a then a.(!c) else 0
  > 
  > let variable_bound a short =
  >   let r = ref 0 in
  >   let rec loop i =
  >     let arr = if i < 3 then a else short in
  >     let hi = Array.length arr - 1 in
  >     if i <> hi
  >     then (
  >       if i >= 0 then r := !r + arr.(i);
  >       loop (i + 1))
  >   in
  >   loop (-1);
  >   !r
  > EOF2
  $ ocamlc -c prog.ml
  $ wasm_of_ocaml compile --opt 2 --debug stats prog.cmo -o prog.wasmo 2>&1 | grep 'bound checks'
  Stats - bound checks removed: 4 arrays, 1 strings, 0 bigarrays

  $ wasm_of_ocaml compile --debug stats prog.cmo -o prog.wasmo 2>&1 | grep 'bound checks'
  [1]

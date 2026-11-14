(* The parameter [_x] of [g] and [h] is not used, so the dead code
   sentinel (an integer) is passed at the call site with two possible
   callees, which goes through the wrapper of [g]. The parameter of [g]
   must not be unboxed by the wrapper. Parameters are only unboxed with
   the type checker's hints (OCaml 5.6). *)

let escaped = ref []

(* [g] escapes, hidden from the flow analysis *)
let[@inline never] escape f = escaped := Obj.repr f :: !escaped

let[@inline never] g (_x : float) (n : int) =
  print_int n;
  n + 1

let[@inline never] h (_x : float) (n : int) =
  print_int n;
  n + 2

let () = escape g

let () =
  let a = float_of_string (Sys.opaque_identity "1.5") in
  print_int (g a 3);
  let f = if Array.length Sys.argv > 0 then g else h in
  print_int (f a 4);
  let k : float -> int -> int = Obj.obj (List.hd !escaped) in
  print_int (k 1. 5);
  print_newline ()

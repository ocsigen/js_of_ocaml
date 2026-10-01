(* A conversion of a value of unknown type must not be performed before an
   instruction which may raise and which precedes it: the value is only
   known to have the right representation once the conversion is reached. *)

(* The untagging of [x] follows a primitive which raises when [x] is not
   an integer: it must not be moved before the primitive, although it is
   performed on both branches and used after them. ([x] is read from a
   reference rather than passed as a parameter, so that it is not passed
   untagged.) *)
let cell = ref (Obj.repr 0)

let conv s b =
  let x = !cell in
  let y =
    if b
    then
      let q = int_of_string s in
      (Obj.obj x : int) + q
    else (Obj.obj x : int) + 1
  in
  y + (Obj.obj x : int)

let conv x s b =
  cell := x;
  conv s b

let () =
  assert (conv (Obj.repr 5) "1" true = 11);
  assert (conv (Obj.repr 5) "z" false = 11);
  assert (
    match conv (Obj.repr (Sys.opaque_identity "abc")) "z" true with
    | _ -> false
    | exception Failure _ -> true)

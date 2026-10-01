(* A conversion of a value of unknown type must not be performed before an
   instruction which may raise and which precedes it: the value is only
   known to have the right representation once the conversion is reached. *)

type ('a, 'b) eq = Refl : ('a, 'a) eq

type _ w =
  | I : int w
  | F : float w
  | S : string w

(* These functions raise when the value has another type *)
let check_int : type a. a w -> (a, int) eq = function
  | I -> Refl
  | F | S -> raise Exit

let check_float : type a. a w -> (a, float) eq = function
  | F -> Refl
  | I | S -> raise Exit

let raises f =
  match f () with
  | _ -> false
  | exception Exit -> true

(* [x] is untagged, or unboxed, on every path from the entry, but after a
   call which may raise: it must not be passed untagged or unboxed *)
let g : type a. a w -> a -> int -> int =
 fun w x k ->
  let Refl = check_int w in
  x + k

let h : type a. a w -> a -> float -> float =
 fun w x k ->
  let Refl = check_float w in
  x +. k

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
  assert (g I 5 (Sys.opaque_identity 1) = 6);
  assert (raises (fun () -> g S (Sys.opaque_identity "abc") 1));
  assert (h F 5. (Sys.opaque_identity 1.) = 6.);
  assert (raises (fun () -> h S (Sys.opaque_identity "abc") 1.));
  assert (raises (fun () -> h I (Sys.opaque_identity 3) 1.));
  assert (conv (Obj.repr 5) "1" true = 11);
  assert (conv (Obj.repr 5) "z" false = 11);
  assert (
    match conv (Obj.repr (Sys.opaque_identity "abc")) "z" true with
    | _ -> false
    | exception Failure _ -> true)

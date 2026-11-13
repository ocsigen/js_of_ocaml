(* A conversion which may fail (the type of the operand is not known) is
   moved out of a loop when the loop performs it whenever it is entered:
   here, the unboxing of [x], at the beginning of the body of the [for]
   loop. *)

type _ t =
  | F : float t
  | S : string t

let sum (type a) (w : a t) (x : a) n =
  match w with
  | F ->
      let s = ref 0. in
      for i = 1 to n do
        s := !s +. x +. float i
      done;
      int_of_float !s
  | S -> String.length x + n

(* The untagging of [x] is available when the loop is entered, but is not
   kept across the call in the loop: it is performed again after the
   call *)
let count f x n =
  let s = ref (x + 1) in
  for _ = 1 to n do
    s := f (!s + x)
  done;
  !s

let () =
  assert (sum F 1.5 10 = 70);
  assert (sum F 0.5 0 = 0);
  assert (sum S "abc" 10 = 13);
  assert (count succ 3 10 = 44);
  assert (count succ 3 0 = 4)

(* The compiler passes the method names of a class to CamlinternalOO as
   constant blocks, which are read as arrays. The global flow analysis
   does not track constants, but their elements must still be taken into
   account when typing array accesses. Otherwise, [f]'s parameter gets
   unboxed as an integer and the string is cast to an integer. Here, a
   constant tuple is used as an array in the same way. *)

let test () =
  let names : string array = Obj.magic ("a", "b") in
  let rec f n x = if n = 0 then Hashtbl.hash x else f (n - 1) x in
  let i = f 2 3 in
  let j = f 2 names.(1) in
  Printf.printf "%d %d\n" i j

let () = test ()

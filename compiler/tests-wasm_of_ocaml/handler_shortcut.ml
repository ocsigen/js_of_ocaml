(* Without [-g], ocamlc turns [r] into a mutable variable, assigned in the
   body of the [try]. The exception handler is an empty block that jumps
   to the code after the [try], passing the value of [r]. Deadcode
   bypasses this block, so [r] ends up read in the arguments of the
   handler continuation of the [try] block. These arguments are evaluated
   in the handler, and so must see the last value assigned to [r]. *)

let s = if Array.length Sys.argv > 10 then "12" else "abc"

let test () =
  let r = ref 0 in
  (try
     r := 1;
     ignore (int_of_string s);
     r := 2
   with _ -> ());
  !r

let () = assert (test () = 1)

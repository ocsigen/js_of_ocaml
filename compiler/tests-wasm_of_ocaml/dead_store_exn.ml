(* Without debug information, [r] becomes a mutable variable assigned in
   the body of the [try]. The first assignment is overwritten by the
   second one when no exception is raised, but its value is read after
   the handler when [int_of_string s] raises: it must not be removed as
   a dead store. *)

let s = if Array.length Sys.argv > 10 then "12" else "abc"

let test () =
  let r = ref 0 in
  (try
     r := 1;
     ignore (Sys.opaque_identity (int_of_string s));
     r := 2
   with _ -> ());
  !r

let () = assert (test () = 1)

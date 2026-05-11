(* Without debug information, [r] becomes a mutable variable assigned in
   the body of the [try]. Sinking [int_of_string s] into its use would
   move the call past this assignment, and the handler would see 5. *)

let s = if Array.length Sys.argv > 10 then "12" else "abc"

let test () =
  let r = ref 0 in
  try
    let x = int_of_string s in
    r := 5;
    print_int x;
    1
  with _ -> !r

let () = assert (test () = 0)

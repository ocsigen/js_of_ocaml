(* Without debug information, [r] becomes a mutable variable assigned in
   the body of the [try]. Sinking [int_of_string s] into its use would
   move the call past this assignment, and the handler would see 5.

   Whether the call can be sunk depends on how its use is compiled: the
   two variants below cover separate compilation ([print_int] is a direct
   call) and whole-program compilation (where [print_int] is inlined, but
   not [Printf.printf]). *)

let s = if Array.length Sys.argv > 10 then "12" else "abc"

let test () =
  let r = ref 0 in
  try
    let x = int_of_string s in
    r := 5;
    print_int x;
    1
  with _ -> !r

let test' () =
  let r = ref 0 in
  try
    let x = int_of_string s in
    r := 5;
    Printf.printf "%d" x;
    1
  with _ -> !r

let () = assert (test () = 0)

let () = assert (test' () = 0)

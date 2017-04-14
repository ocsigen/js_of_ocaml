(* https://github.com/ocsigen/js_of_ocaml/issues/507
 * Missing parentheses around "-1" in generated code #507 *)

open Common
let log_stop = log_start "Missing parentheses around \"-1\" in generated code"

let _ =
  let r = ref 0.0 in
  for _ = 1 to 100 do
    r := !r -. (-1.0) *. !r;
  done;
  ()

let _ = log_stop ()

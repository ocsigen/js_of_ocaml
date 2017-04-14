open Common

let log_stop = log_start "match .. with exception"

exception A
exception B

let a_exn () =
  raise A

(* Make sure that [a] doesn't look constant *)
let a () = if Random.int 1 + 1 = 0 then 2 else 4

let b_exn () =
  raise B

(* https://github.com/ocsigen/js_of_ocaml/issues/400
 * match .. with exception is no compiled properly *)
let () =
  assert
    (try
      match a () with
      | exception (A|B) -> true
      | _n -> b_exn ()
    with B -> true)

let _ = log_stop ()

(* TEST *)

let a = -0.
let b = +0.

let _ =
  if Sys.backend_type = Other "js_of_ocaml" then assert (a == b) else
  assert(not (a == b))

let f () =
  let a = -0. in
  let b = +0. in
  if Sys.backend_type = Other "js_of_ocaml" then assert (a == b) else
  assert(not (a == b))

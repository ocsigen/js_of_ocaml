[@@@warning "-69"]

type t =
  { x : float
  ; y : float
  }

let () =
  let f x = { x; y = 2. } in
  let x = f 1. in
  Format.printf "%f@." x.y

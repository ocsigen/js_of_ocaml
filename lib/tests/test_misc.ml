open Js_of_ocaml

(* Make sure we don't emit an unused warning for method not using "self" *)

let x =
  object%js (self)
    method foo = self##bar

    method bar = ()
  end

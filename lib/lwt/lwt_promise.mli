open Js_of_ocaml
open Promise

val to_lwt : 'a promise Js.t -> 'a Lwt.t
val of_lwt : (unit -> 'a Lwt.t) -> 'a promise Js.t

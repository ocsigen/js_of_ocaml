(* See over_application.ml. [make] has arity 1 and does not perform any
   effect, but the closure it returns does. *)

type _ Effect.t += E : int -> int Effect.t

let[@inline never] make () =
  let r = ref 1 in
  fun x -> Effect.perform (E x) + !r

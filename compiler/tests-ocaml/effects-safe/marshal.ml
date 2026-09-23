(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : unit t

let () =
  try_with (fun h eff -> Effect.Safe.perform h eff [@nontail]) E
  { effc = fun (type a) (e : a t) ->
      Some (fun k ->
          (* We have to make sure that neither the match nor the call
             to Marshal.to_string are eliminated, so we call
             print_string and we print the result of the marshalling
             function. *)
          match print_string "";
            Stdlib.Marshal.to_string k [] with
          | x -> Printf.printf "%S" x; assert false
          | exception (Invalid_argument _) -> print_endline "ok"
          ) }

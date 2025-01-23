(* TEST *)

open Effect
open Effect.Deep

type _ t += E : unit t

let () =
  try_with perform E
  { effc = fun (type a) (e : a t) ->
      match e with
      | E -> Some (fun k ->
          (* We have to make sure that neither the match nor the call
             to caml_equal are eliminated, so we call
             print_string and we print the result of caml_equal. *)
          begin match print_string ""; k = k with
          | b -> Printf.printf "%b" b; assert false
          | exception (Invalid_argument _) -> print_endline "ok"
          end;
          begin match Hashtbl.hash k with
          | _ -> print_endline "ok"
          end)
      | e -> None }

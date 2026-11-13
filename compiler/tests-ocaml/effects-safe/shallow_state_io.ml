(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Shallow

type _ t += Get   : int t
          | Set   : int -> unit t
          | Print : string -> unit t

let handle_state init f x =
  let rec loop : type a r. int -> (a, r) continuation -> a -> r * int =
    fun state k x ->
      continue_with k x
      { retc = (fun result -> result, state);
        exnc = (fun e -> raise e);
        effc = (fun (type b) (eff : b t) ->
          match eff with
          | Get -> Some (fun (k : (b,r) continuation) ->
              loop state k state)
          | Set new_state -> Some (fun (k : (b,r) continuation) ->
              loop new_state k ())
          | e -> None) }
  in
  loop init (Safe.fiber f) x

let handle_print f =
  let rec loop : type r. (unit, r) continuation -> r =
    fun k ->
      continue_with k ()
      { retc = (fun x -> x);
        exnc = (fun e -> raise e);
        effc = (fun (type a) (eff : a t) ->
          match eff with
          | Print s -> Some (fun (k : (a,r) continuation) ->
              print_string s; loop k)
          | e -> None) }
  in
  loop (Safe.fiber f)

let comp h () =
  Effect.Safe.perform h (Print (Printf.sprintf "Initial state: %d\n" (Effect.Safe.perform h Get)));
  Effect.Safe.perform h (Set 42);
  Effect.Safe.perform h (Print (Printf.sprintf "Updated state: %d\n" (Effect.Safe.perform h Get)));
  Effect.Safe.perform h (Set 43)

let main () =
  let (), i = handle_print (fun _h () -> handle_state 0 comp ()) in
  Printf.printf "Final state: %d\n" i

let _ = main ()

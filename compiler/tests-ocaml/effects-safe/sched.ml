(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

exception E
type _ t += Yield : unit t
          | Fork : (Handler.t @ local -> string) -> unit t
          | Ping : unit t
exception Pong

let say = print_string

let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then `Finished
    else continue (Queue.pop run_q) ()
  in
  let rec spawn (f : Handler.t @ local -> string) =
    match_with (fun h () -> f h) ()
    { retc = (function
        | "ok" -> say "."; dequeue ()
        | s -> failwith ("Unexpected result: " ^ s));
      exnc = (function
        | E -> say "!"; dequeue ()
        | e -> raise e);
      effc = fun (type a) (e : a t) ->
        match e with
        | Yield -> Some (fun (k : (a, _) continuation) ->
            say ","; enqueue k; dequeue ())
        | Fork f -> Some (fun (k : (a, _) continuation) ->
            say "+"; enqueue k; spawn f)
        | Ping -> Some (fun (k : (a, _) continuation) ->
            say "["; discontinue k Pong)
        | _ -> None }
  in
  spawn main

let test h =
  say "A";
  Effect.Safe.perform h (Fork (fun h ->
     Effect.Safe.perform h Yield; say "C"; Effect.Safe.perform h Yield;
     begin match_with (fun h () ->
       Effect.Safe.perform h Ping; failwith "no pong?") ()
      { retc = (fun x -> x);
        exnc = (function
          | Pong -> say "]"
          | e -> raise e);
        effc = fun (type a) (e : a t) ->
          match e with
          | Yield -> Some (fun (k : (a,_) continuation) -> failwith "what?")
          | _ -> None }
     end;
     raise E));
  Effect.Safe.perform h (Fork (fun _h -> say "B"; "ok"));
  say "D";
  Effect.Safe.perform h Yield;
  say "E";
  "ok"

let () =
  let `Finished = run test in
  say "\n"

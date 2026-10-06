(* TEST
   { bytecode; }
   { native; }
*)

open Effect
open Effect.Deep
open Effect.Deep.Safe

type _ t += E : int -> int t
          | F : unit t

let rec nest h = function
  | 0 -> Effect.Safe.perform h (E 42)
  | n ->
     match_with (fun h _ -> Printf.printf "[%d\n" n; nest h (n - 1)) ()
     { retc = (fun x -> Printf.printf " %d]\n" n; x);
       exnc = (fun e -> Printf.printf " !%d]\n" n; raise e);
       effc = fun (type a) (e : a t) ->
         match e with
         | F -> Some (fun k -> assert false)
         | _ -> None }

let () =
  match_with (fun h n -> nest h n) 5
  { retc = (fun x -> Printf.printf "= %d\n" x);
    exnc = (fun e -> raise e);
    effc = fun (type a) (e : a t) ->
      match e with
      | E n -> Some (fun (k : (a, _) continuation) -> continue k (n + 100))
      | _ -> None }

let () =
  match_with (fun h n -> nest h n) 5
  { retc = (fun x -> assert false);
    exnc = (fun e -> Printf.printf "%s\n" (Printexc.to_string e));
    effc = fun (type a) (e : a t) ->
      match e with
      | F -> Some (fun k -> assert false)
      | _ -> None }

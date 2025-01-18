[@@@ocaml.warning "-27-32"]

open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int t

let comp1 () =
  let a = Xchg 0 in
  let x = perform a in
  let b = Xchg 1 in
  let y = perform b in
  x + y

let comp2 () =
  let _ = perform (Xchg 0) in
  raise Not_found

let comp3 () =
  let _ = perform (Xchg 0) in
  int_of_string "fdjsl"

let handle comp =
  (*  try*)
  Format.printf "%d@."
  @@ match_with
       comp
       ()
       { retc = (fun x -> x - 30)
       ; exnc = (fun _ -> 42)
       ; effc =
           (fun (type a) (eff : a t) ->
             match eff with
             | Xchg n -> Some (fun (k : (a, _) continuation) -> continue k (n + 17))
             | _ -> None)
       }
(*with Not_found -> assert false*)

let () =
  handle comp1;
  handle comp2;
  handle comp3

type 'a status =
  | Complete of 'a
  | Suspended of
      { msg : int
      ; cont : (int, 'a status) continuation
      }

let step (f : unit -> 'a) () : 'a status =
  match_with
    f
    ()
    { retc = (fun v -> Complete v)
    ; exnc = raise
    ; effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | Xchg msg -> Some (fun (cont : (a, _) continuation) -> Suspended { msg; cont })
          | _ -> None)
    }

let rec run_both a b =
  match a (), b () with
  | Complete va, Complete vb -> va, vb
  | Suspended { msg = m1; cont = k1 }, Suspended { msg = m2; cont = k2 } ->
      run_both (fun () -> continue k1 m2) (fun () -> continue k2 m1)
  | _ -> failwith "Improper synchronization"

let comp2 () = perform (Xchg 21) * perform (Xchg 21)

let () =
  let x, y = run_both (step comp1) (step comp2) in
  Format.printf ">> %d %d@." x y

type _ Effect.t += Fork : (unit -> unit) -> unit t | Yield : unit t

let fork f = perform (Fork f)

let yield () = perform Yield

let xchg v = perform (Xchg v)

(* A concurrent round-robin scheduler *)
let run (main : unit -> unit) : unit =
  let exchanger = ref None in
  (* waiting exchanger *)
  let run_q = Queue.create () in
  (* scheduler queue *)
  let enqueue k v =
    let task () = continue k v in
    Queue.push task run_q
  in
  let dequeue () =
    if Queue.is_empty run_q
    then () (* done *)
    else
      let task = Queue.pop run_q in
      task ()
  in
  let rec spawn (f : unit -> unit) : unit =
    match_with
      f
      ()
      { retc = dequeue
      ; exnc =
          (fun e ->
            print_endline (Printexc.to_string e);
            dequeue ())
      ; effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | Yield ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k ();
                    dequeue ())
            | Fork f ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k ();
                    spawn f)
            | Xchg n ->
                Some
                  (fun (k : (int, unit) continuation) ->
                    match !exchanger with
                    | Some (n', k') ->
                        exchanger := None;
                        enqueue k' n;
                        continue k n'
                    | None ->
                        exchanger := Some (n, k);
                        dequeue ())
            | _ -> None)
      }
  in
  spawn main

let _ =
  run (fun _ ->
      fork (fun _ ->
          Format.printf "[t1] Sending 0@.";
          let v = xchg 0 in
          Format.printf "[t1] received %d@." v);
      fork (fun _ ->
          Format.printf "[t2] Sending 1@.";
          let v = xchg 1 in
          Format.printf "[t2] received %d@." v))

(*****)

type _ Effect.t += E : string t | F : string t

let foo () = perform F ^ " " ^ perform E ^ " " ^ perform F

let bar () =
  try_with
    foo
    ()
    { effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | E -> Some (fun (k : (a, _) continuation) -> continue k "Coucou!")
          | _ -> None)
    }

let baz () =
  try_with
    bar
    ()
    { effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | F -> Some (fun (k : (a, _) continuation) -> continue k "Hello, world!")
          | _ -> None)
    }

let () = Format.printf "%s@." (baz ())

(****)

let () =
  Format.printf
    "%s@."
    (try_with
       (fun () -> try perform F with Not_found -> "Discontinued")
       ()
       { effc = (fun (type a) (eff : a t) -> Some (fun k -> discontinue k Not_found)) })

let () =
  Format.printf
    "%s@."
    (try_with
       (fun () -> try perform F with Unhandled _ -> "Unhandled")
       ()
       { effc = (fun (type a) (eff : a t) -> None) })

let () = Format.printf "%s@." (try bar () with Unhandled _ -> "Saw unhandled exception")

let () =
  try
    Format.printf "%d@."
    @@ try_with
         perform
         (Xchg 0)
         { effc =
             (fun (type a) (eff : a t) ->
               match eff with
               | Xchg n ->
                   Some (fun (k : (a, _) continuation) -> continue k 21 + continue k 21)
               | _ -> None)
         }
  with Continuation_already_resumed -> Format.printf "One-shot@."

(****)

let invert (type a) ~(iter : (a -> unit) -> unit) : a Seq.t =
  let module M = struct
    type _ Effect.t += Yield : a -> unit t
  end in
  let yield v = perform (M.Yield v) in
  fun () ->
    match_with
      iter
      yield
      { retc = (fun _ -> Seq.Nil)
      ; exnc = raise
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | M.Yield v ->
                Some (fun (k : (b, _) continuation) -> Seq.Cons (v, continue k))
            | _ -> None)
      }

let s = invert ~iter:(Fun.flip String.iter "OCaml")

let next = Seq.to_dispenser s

let rec loop () =
  match next () with
  | Some c ->
      Format.printf "%c" c;
      loop ()
  | None -> Format.printf "@."

let () = loop ()

(****)

type _ Effect.t += Send : int -> unit Effect.t | Recv : int Effect.t

open! Effect.Shallow

let run (comp : unit -> unit) : unit =
  let rec loop_send : type a. (a, unit) continuation -> a -> unit =
   fun k v ->
    continue_with
      k
      v
      { retc = Fun.id
      ; exnc = raise
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Send n -> Some (fun (k : (b, _) continuation) -> loop_recv n k ())
            | Recv -> failwith "protocol violation"
            | _ -> None)
      }
  and loop_recv : type a. int -> (a, unit) continuation -> a -> unit =
   fun n k v ->
    continue_with
      k
      v
      { retc = Fun.id
      ; exnc = raise
      ; effc =
          (fun (type b) (eff : b Effect.t) ->
            match eff with
            | Recv -> Some (fun (k : (b, _) continuation) -> loop_send k n)
            | Send v -> failwith "protocol violation"
            | _ -> None)
      }
  in
  loop_send (fiber comp) ()

let () =
  run (fun () ->
      Format.printf "Send 42@.";
      perform (Send 42);
      Format.printf "Recv: %d@." (perform Recv);
      Format.printf "Send 43@.";
      perform (Send 43);
      Format.printf "Recv: %d@." (perform Recv))

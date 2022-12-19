(*
Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

(* Monadic Reflection :
   http://www.cs.ioc.ee/mpc-amast06/msfp/filinski-slides.pdf *)
open Effect
open Effect.Deep

(* The monad signature *)
module type MONAD = sig
  type +_ t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Build reify and reflect operations for any monad *)
module RR (M : MONAD) : sig
  val reify : (unit -> 'a) -> 'a M.t

  val reflect : 'a M.t -> 'a
end = struct
  type _ Effect.t += E : 'a M.t -> 'a Effect.t

  let reify f =
    match_with
      f
      ()
      { retc = (fun x -> M.return x)
      ; exnc = raise
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | E m -> Some (fun k -> M.bind m (continue k))
            | _ -> None)
      }

  let reflect m = perform (E m)
end

(* The state monad *)
module State = struct
  type 'a t = int -> int * 'a

  let return v s = s, v

  let bind m k s =
    let s, a = m s in
    k a s

  let get s = s, s

  let put s _ = s, ()

  let run s ~init = s init
end

(* Reify and reflect for State *)
module StateR = RR (State)

(* val put : int -> unit State.t *)
let put v = StateR.reflect (State.put v)

(* val get : unit -> int State.t *)
let get () = StateR.reflect State.get

(* val run_state : (unit -> 'a) -> init:int -> 'a *)
let run_state f ~init =
  let final, v = State.run (StateR.reify f) ~init in
  Printf.printf "Final state: %d\n" final;
  v

(* The exception monad *)
module Exception = struct
  type 'a t =
    | Ok of 'a
    | Exn of exn

  let return v = Ok v

  let bind m k =
    match m with
    | Ok v -> k v
    | Exn e -> Exn e

  let raise exn = Exn exn

  let run m ~catch =
    match m with
    | Ok v -> v
    | Exn e -> catch e
end

(* Reify and reflect for Exception *)
module ExceptionR = RR (Exception)

(* val raise : exn -> 'a *)
let raise e = ExceptionR.reflect (Exception.raise e)

(* val run_exception : (unit -> 'a) -> catch:(exn -> 'a) -> 'a *)
let run_exception m ~catch = Exception.run (ExceptionR.reify m) ~catch

(* Using the state monad *)
let state_example () =
  let initial = get () in
  Printf.printf "Initial state: %d\n" initial;
  put 10;
  assert (get () = 10);
  put (get () + 1);
  assert (get () = 11);
  put 12;
  `Initial initial, `Final (get ())

(* Using the exception monad *)
let exception_example () =
  Printf.printf "Raising an exception\n";
  raise (Failure "An error!") |> ignore;
  Printf.printf "This shouldn't be displayed\n"

(* Using both exceptions and state *)
let combined_example () =
  Printf.printf "Initial state: %d\n" (get ());
  put 100;
  raise (Failure "An error!") |> ignore;
  put 200

let print_exception e = Printf.printf "Exception: %s\n" (Printexc.to_string e)

let%expect_test _ =
  run_state ~init:10 state_example |> ignore;

  [%expect {|
    Initial state: 10
    Final state: 12 |}];

  run_exception ~catch:print_exception exception_example;

  [%expect {|
    Raising an exception
    Exception: Failure("An error!") |}];

  (run_exception ~catch:print_exception
  @@ fun () -> run_state ~init:10 @@ fun () -> combined_example ());

  [%expect {|
    Initial state: 10
    Exception: Failure("An error!") |}];

  (run_state ~init:10
  @@ fun () -> run_exception ~catch:print_exception @@ fun () -> combined_example ());
  [%expect
    {|
    Initial state: 10
    Exception: Failure("An error!")
    Final state: 100 |}]

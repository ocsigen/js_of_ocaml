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

open Effect
open Effect.Deep

type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t

type _ Effect.t += Yield : unit Effect.t

let fork f = perform (Fork f)

let yield () = perform Yield

(* A concurrent round-robin scheduler *)
let run main =
  let run_q = Queue.create () in
  let enqueue k = Queue.push k run_q in
  let dequeue () = if Queue.is_empty run_q then () else continue (Queue.pop run_q) () in
  let rec spawn f =
    (* Effect handler => instantiates fiber *)
    match_with
      f
      ()
      { retc = (fun () -> dequeue ())
      ; exnc =
          (fun e ->
            print_string (Printexc.to_string e);
            dequeue ())
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Yield ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k;
                    dequeue ())
            | Fork f ->
                Some
                  (fun (k : (a, unit) continuation) ->
                    enqueue k;
                    spawn f)
            | _ -> None)
      }
  in
  spawn main

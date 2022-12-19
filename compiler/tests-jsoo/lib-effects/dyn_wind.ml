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

(* User-land dynamic wind:
   http://okmij.org/ftp/continuations/implementations.html#dynamic-wind *)
open Effect
open Effect.Deep

let dynamic_wind before_thunk thunk after_thunk =
  before_thunk ();
  let res =
    match_with
      thunk
      ()
      { retc = Fun.id
      ; exnc =
          (fun e ->
            after_thunk ();
            raise e)
      ; effc =
          (fun (type a) (e : a Effect.t) ->
            Some
              (fun (k : (a, _) continuation) ->
                after_thunk ();
                let res' = perform e in
                before_thunk ();
                continue k res'))
      }
  in
  after_thunk ();
  res

type _ Effect.t += E : unit Effect.t

let%expect_test _ =
  let bt () = Printf.printf "IN\n" in
  let at () = Printf.printf "OUT\n" in
  let foo () =
    Printf.printf "peform E\n";
    perform E;
    Printf.printf "peform E\n";
    perform E;
    Printf.printf "done\n"
  in
  try_with
    (dynamic_wind bt foo)
    at
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | E ->
              Some
                (fun (k : (a, _) continuation) ->
                  Printf.printf "handled E\n";
                  continue k ())
          | _ -> None)
    };
  [%expect
    {|
    IN
    peform E
    OUT
    handled E
    IN
    peform E
    OUT
    handled E
    IN
    done
    OUT |}]

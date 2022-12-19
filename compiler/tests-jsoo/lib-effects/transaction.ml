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

open Printf
open Effect
open Effect.Deep

type bottom

module type TXN = sig
  type 'a t

  val atomically : (unit -> unit) -> unit

  val ref : 'a -> 'a t

  val ( ! ) : 'a t -> 'a

  val ( := ) : 'a t -> 'a -> unit
end

module Txn : TXN = struct
  type 'a t = 'a ref

  type _ Effect.t += Update : 'a t * 'a -> unit Effect.t

  let atomically f =
    let comp =
      match_with
        f
        ()
        { retc = (fun x _ -> x)
        ; exnc =
            (fun e rb ->
              rb ();
              raise e)
        ; effc =
            (fun (type a) (e : a Effect.t) ->
              match e with
              | Update (r, v) ->
                  Some
                    (fun (k : (a, _) continuation) rb ->
                      let old_v = !r in
                      r := v;
                      continue k () (fun () ->
                          r := old_v;
                          rb ()))
              | _ -> None)
        }
    in
    comp (fun () -> ())

  let ref = ref

  let ( ! ) = ( ! )

  let ( := ) r v = perform (Update (r, v))
end

exception Res of int

open Txn

let%expect_test _ =
  (try
     atomically (fun () ->
         let r = ref 10 in
         printf "T0: %d\n" !r;
         try
           atomically (fun () ->
               r := 20;
               r := 21;
               printf "T1: Before abort %d\n" !r;
               raise (Res !r) |> ignore;
               printf "T1: After abort %d\n" !r;
               r := 30)
         with
         | Res v ->
             printf "T0: T1 aborted with %d\n" v;
             printf "T0: %d\n" !r
         | e -> printf "inner exception: %s\n" (Printexc.to_string e))
   with e -> printf "outer exception: %s\n" (Printexc.to_string e));
  [%expect {|
    T0: 10
    T1: Before abort 21
    T0: T1 aborted with 21
    T0: 10 |}]

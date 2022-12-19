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

(* state.ml *)

(* This file introduces the type [CELL] formalizing a memory cell
   as a functor that, for any given type, implements the [STATE]
   interface.

   Three cell implementations are given:
   (1) [GlobalMutVar], an implementation using global state.
   (2) [LocalMutVar], an implementation using local state.
   (3) [StPassing], a functional implementation in state-passing style.

   The stating-passing--style implementation comes from

     https://gist.github.com/kayceesrk/3c307d0340fbfc68435d4769ad447e10 .
*)

open Effect
open Effect.Deep

(* --------------------------------------------------------------------------- *)
(** Type Definitions. *)

(* [TYPE] specifies a type [t]. *)
module type TYPE = sig
  type t
end

(* [STATE] is the type of a module that offers the functions [get] and [set]
   for manipulating a piece of mutable state with contents in the type [t].
   This module must also offer a function [run] for handling computations
   that perform the operations [get] and [set].
*)
module type STATE = sig
  type t

  val get : unit -> t

  val set : t -> unit

  val run : init:t -> (unit -> 'a) -> t * 'a
end

(* [CELL] is the type of a functor that produces an
   implementation of [STATE] for any given type.
*)
module type CELL = functor (T : TYPE) -> STATE with type t = T.t

(* Note.

     The signatures [STATE] and [CELL] are equivalent to the following
     record types, respectively:

     ```ocaml
       type 's state = {
         get : unit -> 's;
         set : 's -> unit;
         run : 'a. init:'s -> (unit -> 'a) -> 's * 'a
       }

       type cell = {
         fresh : 's. unit -> 's state
       }
     ```

     We prefer the signatures [STATE] and [CELL] over the record types,
     because implementations of these interfaces often need to declare
     new effect names (which comes more naturally in the scope of a
     module definition) and because we need a module signature of cells
     to declare the functor signature [HEAP] in the file [ref.ml] (if we
     want to avoid types such as [cell -> (module REF)]).
*)

(* --------------------------------------------------------------------------- *)
(** Global State. *)

(* [GlobalMutVar] implements a cell using the global state.

   The module produced by this functor allocates a fresh reference [var],
   which initially holds the value [None]. The operations [get] and [set]
   perform accesses to this reference, but can be called only in the scope
   of [run].

   Nested applications of [run] (given by the same module), such as

   ```ocaml
     let open GlobalMutVar(struct type t = int end) in
     run ~init:0 (fun _ -> run ~init:1 (fun _ -> ()))
   ```,

   are unsafe, because the innermost [run] resets [var] to [None].
   The final read to [var] performed by the outermost [run] (to construct
   the pair [t * 'a]) is thus invalidated.

   Parallel applications of [run] (given by the same module) are unsafe,
   because an instance of [run] can reset [var] to [None] while parallel
   instances are still ongoing. Moreover, accesses to [var] will suffer
   from race conditions.
*)
module GlobalMutVar : CELL =
functor
  (T : TYPE)
  ->
  struct
    type t = T.t

    let var = ref None

    let get () =
      match !var with
      | Some x -> x
      | None -> assert false

    let set y = var := Some y

    let run ~init main =
      set init
      |> fun _ ->
      main () |> fun res -> get () |> fun x -> (var := None) |> fun _ -> x, res
  end

(* --------------------------------------------------------------------------- *)
(** Local State. *)

(* [LocalMutVar] implements a cell using effect handlers and local mutable
   state. The operations [get] and [set] are opaque: they are simply defined
   as [perform] instructions to the effects [Get] and [Set], respectively.
   The program [run] interprets these effects as accesses to a local
   reference [var].

   Nested applications of [run] are safe, but [get] and [set] are handled
   by the innermost [run]. As an example, the program

   ```ocaml
     let open LocalMutVar(struct type t = int end) in
     run ~init:0 (fun _ -> set 3; run ~init:1 (fun _ -> get() + get()))
   ```

   evaluates to [(3, (1, 2))].

   Parallel executions of [run] in separate stacks are safe. Even though
   the effect names [Get] and [Set] are shared among multiple instances
   of [get] and [set], there is no interference among these instances,
   because effect names are immutable.
*)
module LocalMutVar : CELL =
functor
  (T : TYPE)
  ->
  struct
    type t = T.t

    type _ Effect.t += Get : t Effect.t

    type _ Effect.t += Set : t -> unit Effect.t

    let get () = perform Get

    let set y = perform (Set y)

    let run (type a) ~init main : t * a =
      let var = ref init in
      match_with
        main
        ()
        { retc = (fun res -> !var, res)
        ; exnc = raise
        ; effc =
            (fun (type b) (e : b Effect.t) ->
              match e with
              | Get -> Some (fun (k : (b, t * a) continuation) -> continue k (!var : t))
              | Set y ->
                  Some
                    (fun k ->
                      var := y;
                      continue k ())
              | _ -> None)
        }
  end

(* --------------------------------------------------------------------------- *)
(** State-Passing Style. *)

(* [StPassing] implements a cell using effect handlers and the state-passing
   technique.

   Like the functor [LocalMutVar], the operations [get] and [set] are
   implemented as [perform] instructions to the effects [Get] and [Set],
   respectively. However, instead of interpreting these effects as accesses to
   a reference, [run] applies the programming technique state-passing style,
   which avoids mutable state, thus assigning a functional interpretation to
   [Get] and [Set]. More specifically, the program [run main ~init] performs
   the application of the handler that monitors [main()] to the contents of the
   cell, which initially is [init]. When [main()] performs an effect, the
   effect branch can access the current state of the cell by immediately
   returning a lambda abstraction that binds the contents of the cell as its
   single formal argument. The continuation captures the evaluation context up
   to (and including) the handler, therefore, when resuming the continuation,
   the handler must reconstruct its immediately surrounding frame
   corresponding to the application to the contents of the cell.

   Nested applications of [run] are safe. Parallel executions of [run] in
   separate stacks are safe. The same remarks as for the functor [LocalMutVar]
   apply.
*)
module StPassing : CELL =
functor
  (T : TYPE)
  ->
  struct
    type t = T.t

    type _ Effect.t += Get : t Effect.t

    type _ Effect.t += Set : t -> unit Effect.t

    let get () = perform Get

    let set y = perform (Set y)

    let run (type a) ~init (main : unit -> a) : t * a =
      match_with
        main
        ()
        { retc = (fun res x -> x, res)
        ; exnc = raise
        ; effc =
            (fun (type b) (e : b Effect.t) ->
              match e with
              | Get ->
                  Some (fun (k : (b, t -> t * a) continuation) (x : t) -> continue k x x)
              | Set y -> Some (fun k (_x : t) -> continue k () y)
              | _ -> None)
        }
        init
  end

(* --------------------------------------------------------------------------- *)
(** Examples. *)

open Printf

module IntCell = StPassing (struct
  type t = int
end)

module StrCell = StPassing (struct
  type t = string
end)

let main () : unit =
  IntCell.(
    printf "%d\n" (get ());
    set 42;
    printf "%d\n" (get ());
    set 21;
    printf "%d\n" (get ()));
  StrCell.(
    set "Hello...";
    printf "%s\n" (get ());
    set "...World!";
    printf "%s\n" (get ()))

let%expect_test _ =
  ignore (IntCell.run ~init:0 (fun () -> StrCell.run ~init:"" main));
  [%expect {|
    0
    42
    21
    Hello...
    ...World! |}]

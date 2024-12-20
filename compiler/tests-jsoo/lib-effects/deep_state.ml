(* deep_state.ml *)

open Effect
open Effect.Shallow

module type State = sig
  type a

  type _ Effect.t += Get : a Effect.t

  type _ Effect.t += Set : a -> unit Effect.t
end

module Make (S : State) = struct
  let rec loop : type x y. S.a -> (x, y) continuation -> x -> y =
   fun s k x ->
    continue_with
      k
      x
      { retc = (fun y -> y)
      ; exnc = raise
      ; effc =
          (fun (type b) (e : b Effect.t) ->
            match e with
            | S.Get -> Some (fun (k : (b, _) continuation) -> loop s k s)
            | S.Set s -> Some (fun (k : (b, _) continuation) -> loop s k ())
            | _ -> None)
      }

  let handle (s : S.a) (f : unit -> 'a) : 'a = loop s (fiber f) ()

  let get () = perform S.Get

  let set v = perform (S.Set v)
end

module IntState = struct
  type a = int

  type _ Effect.t += Get : int Effect.t

  type _ Effect.t += Set : int -> unit Effect.t
end

module StringState = struct
  type a = string

  type _ Effect.t += Get : string Effect.t

  type _ Effect.t += Set : string -> unit Effect.t
end

let main () =
  let depth = int_of_string Sys.argv.(1) in
  let ops = int_of_string Sys.argv.(2) in
  Printf.printf "Running deepstate: depth=%d ops=%d\n" depth ops;
  let module SS = Make (StringState) in
  let rec setup_deep_state n () =
    if n = 0
    then
      for _ = 1 to ops do
        (*         SS.set (SS.get () ^ "_" ^ (string_of_int i)) *)
        SS.set (SS.get ())
      done
    (*       print_endline @@ SS.get() *)
    else
      let module IS = Make (IntState) in
      IS.handle 0 @@ setup_deep_state (n - 1)
  in

  SS.handle "Hello, world!" @@ setup_deep_state depth

let _ = main ()
